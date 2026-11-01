(** A single RV64GC hart. [ecall] is delegated to a pluggable handler (see {!Os}
    for the Linux user-mode environment). *)

open Instr

type trap =
  | Illegal_instruction of int  (** raw bits *)
  | Breakpoint
  | Misaligned_atomic of int64
  | Unknown_csr of int
  | Unhandled_ecall

exception Trap of trap

let pp_trap ppf = function
  | Illegal_instruction bits ->
      Format.fprintf ppf "illegal instruction 0x%08x" bits
  | Breakpoint -> Format.pp_print_string ppf "breakpoint (ebreak)"
  | Misaligned_atomic a ->
      Format.fprintf ppf "misaligned atomic access at 0x%Lx" a
  | Unknown_csr c -> Format.fprintf ppf "access to unimplemented CSR 0x%03x" c
  | Unhandled_ecall -> Format.pp_print_string ppf "ecall with no environment"

(* Registers live in [Bytes] rather than an [int64 array]: an array stores
   each int64 as a separate heap block, so every register write allocated.
   [Bytes.get_int64_le]/[set_int64_le] compile to plain memory accesses. *)
type t = {
  x : Bytes.t;  (** 32 little-endian int64s; [x0] is never written *)
  f : Bytes.t;  (** 32 raw bit patterns; singles are NaN-boxed *)
  pc_cell : Bytes.t;  (** the program counter, one int64; see {!pc} *)
  mem : Memory.t;
  decoded : Decode_cache.t;
  mutable fflags : int;
  mutable frm : int;
  mutable instret : int;  (** instructions retired; 63 bits never overflow *)
  mutable reservation : int64 option;  (** LR/SC reservation address *)
  mutable exit_code : int option;
  mutable ecall : t -> unit;  (** environment call handler *)
  mcsrs : (int, int64) Hashtbl.t;
      (** machine-mode CSRs, as plain storage for now (ROADMAP phase 2) *)
}

let create () =
  let mem = Memory.create () and decoded = Decode_cache.create () in
  mem.on_code_write <- Decode_cache.invalidate decoded;
  {
    x = Bytes.make (32 * 8) '\000';
    f = Bytes.make (32 * 8) '\000';
    pc_cell = Bytes.make 8 '\000';
    mem;
    decoded;
    fflags = 0;
    frm = 0;
    instret = 0;
    reservation = None;
    exit_code = None;
    ecall = (fun _ -> raise (Trap Unhandled_ecall));
    mcsrs = Hashtbl.create 16;
  }

let pc t = Bytes.get_int64_le t.pc_cell 0
let set_pc t v = Bytes.set_int64_le t.pc_cell 0 v

(** Copy a program image into memory and point [pc] at its entry. The stack and
    process environment are the job of {!Os}. *)
let load t (img : Image.t) =
  List.iter
    (fun (s : Image.segment) -> Memory.blit_bytes t.mem s.vaddr s.data)
    img.segments;
  set_pc t img.entry;
  match List.assoc_opt "__global_pointer$" img.symbols with
  | Some gp -> Bytes.set_int64_le t.x (8 * (Reg.gp :> int)) gp
  | None -> ()

(* {1 Register access} *)

(** Integer register [n] (0-31), by number. *)
let get_x t n = Bytes.get_int64_le t.x (8 * n)

let set_x t n v = if n <> 0 then Bytes.set_int64_le t.x (8 * n) v

(** FP register [n] (0-31), as raw bits. *)
let get_f t n = Bytes.get_int64_le t.f (8 * n)

let set_f t n v = Bytes.set_int64_le t.f (8 * n) v
let get t (r : Reg.t) = get_x t (r :> int)
let set t (r : Reg.t) v = set_x t (r :> int) v
let getf t (r : Freg.t) = get_f t (r :> int)
let setf t (r : Freg.t) v = set_f t (r :> int) v
let get_s t r = Fpu.unbox_s (getf t r)
let set_s t r v = setf t r (Fpu.box_s v)

(* {1 Integer helpers} *)

let[@inline] sext32 v = Int64.of_int32 (Int64.to_int32 v)
let[@inline] zext32 v = Int64.logand v 0xffff_ffffL
let[@inline] of_bool b = if b then 1L else 0L

let[@inline] sext ~bits v =
  Int64.shift_right (Int64.shift_left v (64 - bits)) (64 - bits)

(* The helpers below are marked [@inline] so that the ALU's results stay
   unboxed: OCaml (without flambda) boxes an int64 returned by a call, and
   one boxed case makes every case of [alu] allocate. They avoid local
   functions, which would prevent inlining. *)

let[@inline] lo32 v = Int64.logand v 0xffff_ffffL
let[@inline] hi32 v = Int64.shift_right_logical v 32

(** High 64 bits of the unsigned 128-bit product. *)
let[@inline] mulhu a b =
  let open Int64 in
  let a0 = lo32 a and a1 = hi32 a and b0 = lo32 b and b1 = hi32 b in
  let p00 = mul a0 b0
  and p01 = mul a0 b1
  and p10 = mul a1 b0
  and p11 = mul a1 b1 in
  let mid = add (add (hi32 p00) (lo32 p01)) (lo32 p10) in
  add (add (add p11 (hi32 p01)) (hi32 p10)) (hi32 mid)

let[@inline] mulh a b =
  let open Int64 in
  let h = mulhu a b in
  let h = if compare a 0L < 0 then sub h b else h in
  if compare b 0L < 0 then sub h a else h

let[@inline] mulhsu a b =
  let h = mulhu a b in
  if Int64.compare a 0L < 0 then Int64.sub h b else h

(* Division follows the spec: no traps. x/0 = -1, x%0 = x, and
   MIN / -1 = MIN with remainder 0. *)
let[@inline] sdiv ~min a b =
  if b = 0L then -1L else if a = min && b = -1L then a else Int64.div a b

let[@inline] srem ~min a b =
  if b = 0L then a else if a = min && b = -1L then 0L else Int64.rem a b

(* [Int64.unsigned_div] with the same algorithm, but inlinable. *)
let[@inline] unsigned_div n d =
  let open Int64 in
  if compare d 0L < 0 then if unsigned_compare n d < 0 then 0L else 1L
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_compare r d >= 0 then succ q else q

let[@inline] udiv a b = if b = 0L then -1L else unsigned_div a b

let[@inline] urem a b =
  if b = 0L then a else Int64.sub a (Int64.mul (unsigned_div a b) b)

let rec clz64 v n =
  if n = 64 || Int64.compare v 0L < 0 then n
  else clz64 (Int64.shift_left v 1) (n + 1)

let clz v = clz64 v 0

let ctz v =
  if v = 0L then 64
  else
    let rec go v n =
      if Int64.logand v 1L = 1L then n
      else go (Int64.shift_right_logical v 1) (n + 1)
    in
    go v 0

let cpop v =
  let rec go v n =
    if v = 0L then n else go (Int64.logand v (Int64.sub v 1L)) (n + 1)
  in
  go v 0

let[@inline] rol a n =
  Int64.(logor (shift_left a n) (shift_right_logical a ((64 - n) land 63)))

let[@inline] ror a n = rol a ((64 - n) land 63)

let[@inline] rol32 a n =
  let a = zext32 a in
  sext32
    Int64.(logor (shift_left a n) (shift_right_logical a ((32 - n) land 31)))

let[@inline] ror32 a n = rol32 a ((32 - n) land 31)

let bytes_map f v =
  let r = ref 0L in
  for i = 0 to 7 do
    let b = Int64.(logand (shift_right_logical v (8 * i)) 0xffL) in
    r := Int64.logor !r (f i b)
  done;
  !r

let rev8 = bytes_map (fun i b -> Int64.shift_left b (8 * (7 - i)))

let orc_b =
  bytes_map (fun i b -> if b = 0L then 0L else Int64.shift_left 0xffL (8 * i))

let[@inline] alu op a b =
  let open Int64 in
  let sh = to_int b land 63 and sh32 = to_int b land 31 in
  let u32 = zext32 in
  match op with
  | Add -> add a b
  | Sub -> sub a b
  | Sll -> shift_left a sh
  | Slt -> of_bool (compare a b < 0)
  | Sltu -> of_bool (unsigned_compare a b < 0)
  | Xor -> logxor a b
  | Srl -> shift_right_logical a sh
  | Sra -> shift_right a sh
  | Or -> logor a b
  | And -> logand a b
  | Addw -> sext32 (add a b)
  | Subw -> sext32 (sub a b)
  | Sllw -> sext32 (shift_left a sh32)
  | Srlw -> sext32 (shift_right_logical (u32 a) sh32)
  | Sraw -> sext32 (shift_right (sext32 a) sh32)
  | Mul -> mul a b
  | Mulh -> mulh a b
  | Mulhsu -> mulhsu a b
  | Mulhu -> mulhu a b
  | Div -> sdiv ~min:min_int a b
  | Divu -> udiv a b
  | Rem -> srem ~min:min_int a b
  | Remu -> urem a b
  | Mulw -> sext32 (mul a b)
  | Divw -> sext32 (sdiv ~min:(-0x8000_0000L) (sext32 a) (sext32 b))
  | Divuw -> sext32 (udiv (u32 a) (u32 b))
  | Remw -> sext32 (srem ~min:(-0x8000_0000L) (sext32 a) (sext32 b))
  | Remuw -> sext32 (urem (u32 a) (u32 b))
  | Sh1add -> add (shift_left a 1) b
  | Sh2add -> add (shift_left a 2) b
  | Sh3add -> add (shift_left a 3) b
  | Add_uw -> add (u32 a) b
  | Sh1add_uw -> add (shift_left (u32 a) 1) b
  | Sh2add_uw -> add (shift_left (u32 a) 2) b
  | Sh3add_uw -> add (shift_left (u32 a) 3) b
  | Andn -> logand a (lognot b)
  | Orn -> logor a (lognot b)
  | Xnor -> lognot (logxor a b)
  | Max -> if compare a b >= 0 then a else b
  | Maxu -> if unsigned_compare a b >= 0 then a else b
  | Min -> if compare a b <= 0 then a else b
  | Minu -> if unsigned_compare a b <= 0 then a else b
  | Rol -> rol a sh
  | Ror -> ror a sh
  | Rolw -> rol32 a sh32
  | Rorw -> ror32 a sh32
  | Bclr -> logand a (lognot (shift_left 1L sh))
  | Bset -> logor a (shift_left 1L sh)
  | Binv -> logxor a (shift_left 1L sh)
  | Bext -> logand (shift_right_logical a sh) 1L

let[@inline] imm_alu op a imm =
  let b = Int64.of_int imm in
  match op with
  | Addi -> alu Add a b
  | Slti -> alu Slt a b
  | Sltiu -> alu Sltu a b
  | Xori -> alu Xor a b
  | Ori -> alu Or a b
  | Andi -> alu And a b
  | Addiw -> alu Addw a b

let[@inline] shift_alu op a shamt =
  let b = Int64.of_int shamt in
  match op with
  | Slli -> alu Sll a b
  | Srli -> alu Srl a b
  | Srai -> alu Sra a b
  | Slliw -> alu Sllw a b
  | Srliw -> alu Srlw a b
  | Sraiw -> alu Sraw a b
  | Rori -> alu Ror a b
  | Roriw -> alu Rorw a b
  | Bclri -> alu Bclr a b
  | Bseti -> alu Bset a b
  | Binvi -> alu Binv a b
  | Bexti -> alu Bext a b
  | Slli_uw -> Int64.shift_left (zext32 a) shamt

let unary op a =
  let i = Int64.of_int in
  match op with
  | Clz -> i (clz a)
  | Ctz -> i (ctz a)
  | Cpop -> i (cpop a)
  | Clzw -> i (clz (Int64.shift_left a 32) |> min 32)
  | Ctzw -> i (min 32 (ctz a))
  | Cpopw -> i (cpop (zext32 a))
  | Sext_b -> sext ~bits:8 a
  | Sext_h -> sext ~bits:16 a
  | Zext_h -> Int64.logand a 0xffffL
  | Rev8 -> rev8 a
  | Orc_b -> orc_b a

let[@inline] branch_taken op a b =
  match op with
  | Beq -> a = b
  | Bne -> a <> b
  | Blt -> Int64.compare a b < 0
  | Bge -> Int64.compare a b >= 0
  | Bltu -> Int64.unsigned_compare a b < 0
  | Bgeu -> Int64.unsigned_compare a b >= 0

let load_value t op addr =
  let m = t.mem in
  match op with
  | Lb -> sext ~bits:8 (Memory.load m addr 1)
  | Lh -> sext ~bits:16 (Memory.load m addr 2)
  | Lw -> sext32 (Memory.load m addr 4)
  | Ld -> Memory.load m addr 8
  | Lbu -> Memory.load m addr 1
  | Lhu -> Memory.load m addr 2
  | Lwu -> Memory.load m addr 4

let store_size = function Sb -> 1 | Sh -> 2 | Sw -> 4 | Sd -> 8

(* {1 CSRs} *)

let csr_read t csr =
  match csr with
  | 0x001 -> Int64.of_int t.fflags
  | 0x002 -> Int64.of_int t.frm
  | 0x003 -> Int64.of_int ((t.frm lsl 5) lor t.fflags)
  | 0xc00 | 0xc01 | 0xc02 -> Int64.of_int t.instret (* cycle = time *)
  | 0x301 (* misa: RV64 IMAFDC + U *) ->
      Int64.(logor (shift_left 2L 62) (of_int 0x10112d))
  | 0xf11 | 0xf12 | 0xf13 | 0xf14 (* vendor/arch/impl ids, hart id *) -> 0L
  | 0x300 | 0x304 | 0x305 | 0x340 | 0x341 | 0x342 | 0x343 | 0x344 ->
      Option.value ~default:0L (Hashtbl.find_opt t.mcsrs csr)
  | _ -> raise (Trap (Unknown_csr csr))

let csr_write t csr v64 =
  let v = Int64.to_int v64 in
  match csr with
  | 0x001 -> t.fflags <- v land 0x1f
  | 0x002 -> t.frm <- v land 0x7
  | 0x003 ->
      t.fflags <- v land 0x1f;
      t.frm <- (v lsr 5) land 0x7
  | 0x301 -> () (* misa is WARL; writes ignored *)
  | 0x300 | 0x304 | 0x305 | 0x340 | 0x341 | 0x342 | 0x343 | 0x344 ->
      Hashtbl.replace t.mcsrs csr v64
  | _ -> raise (Trap (Unknown_csr csr))
(* counters are read-only *)

(* {1 Floating point} *)

let rm_of t = function
  | Dyn -> (
      match Opcode.lookup Opcode.rm_code t.frm with
      | Some Dyn | None -> None (* invalid frm: illegal instruction *)
      | Some rm -> Some rm)
  | rm -> Some rm

(** Run a format-generic FP operation on the registers of [fmt]. *)
let fp_exec t ~bits (i : Instr.t) =
  let resolve rm =
    match rm_of t rm with
    | Some rm -> rm
    | None -> raise (Trap (Illegal_instruction bits))
  in
  let flag (v, fl) =
    t.fflags <- t.fflags lor fl;
    v
  in
  let rd_d r = getf t r and rd_s r = get_s t r in
  match i with
  | Farith { op; fmt = D; rd; rs1; rs2; rm } ->
      setf t rd
        (flag (Fpu.arith Fpu.double (resolve rm) op (rd_d rs1) (rd_d rs2)))
  | Farith { op; fmt = S; rd; rs1; rs2; rm } ->
      set_s t rd
        (flag (Fpu.arith Fpu.single (resolve rm) op (rd_s rs1) (rd_s rs2)))
  | Fsqrt { fmt = D; rd; rs1; rm } ->
      setf t rd (flag (Fpu.fsqrt Fpu.double (resolve rm) (rd_d rs1)))
  | Fsqrt { fmt = S; rd; rs1; rm } ->
      set_s t rd (flag (Fpu.fsqrt Fpu.single (resolve rm) (rd_s rs1)))
  | Fma { op; fmt = D; rd; rs1; rs2; rs3; rm } ->
      setf t rd
        (flag
           (Fpu.fma Fpu.double (resolve rm) op (rd_d rs1) (rd_d rs2) (rd_d rs3)))
  | Fma { op; fmt = S; rd; rs1; rs2; rs3; rm } ->
      set_s t rd
        (flag
           (Fpu.fma Fpu.single (resolve rm) op (rd_s rs1) (rd_s rs2) (rd_s rs3)))
  | Fsign { op = (Fmin | Fmax) as op; fmt = D; rd; rs1; rs2 } ->
      setf t rd
        (flag
           (Fpu.min_max Fpu.double ~is_max:(op = Fmax) (rd_d rs1) (rd_d rs2)))
  | Fsign { op = (Fmin | Fmax) as op; fmt = S; rd; rs1; rs2 } ->
      set_s t rd
        (flag
           (Fpu.min_max Fpu.single ~is_max:(op = Fmax) (rd_s rs1) (rd_s rs2)))
  | Fsign { op; fmt = D; rd; rs1; rs2 } ->
      setf t rd (Fpu.sgnj_d op (rd_d rs1) (rd_d rs2))
  | Fsign { op; fmt = S; rd; rs1; rs2 } ->
      set_s t rd (Fpu.sgnj_s op (rd_s rs1) (rd_s rs2))
  | Fcmp { op; fmt = D; rd; rs1; rs2 } ->
      set t rd
        (of_bool (flag (Fpu.compare Fpu.double op (rd_d rs1) (rd_d rs2))))
  | Fcmp { op; fmt = S; rd; rs1; rs2 } ->
      set t rd
        (of_bool (flag (Fpu.compare Fpu.single op (rd_s rs1) (rd_s rs2))))
  | Fclass { fmt = D; rd; rs1 } ->
      set t rd (Int64.of_int (Fpu.fclass_d (rd_d rs1)))
  | Fclass { fmt = S; rd; rs1 } ->
      set t rd (Int64.of_int (Fpu.fclass_s (rd_s rs1)))
  | Fcvt_to_int { fmt = D; ity; rd; rs1; rm } ->
      set t rd (flag (Fpu.to_int_bits Fpu.double (resolve rm) ity (rd_d rs1)))
  | Fcvt_to_int { fmt = S; ity; rd; rs1; rm } ->
      set t rd (flag (Fpu.to_int_bits Fpu.single (resolve rm) ity (rd_s rs1)))
  | Fcvt_of_int { fmt = D; ity; rd; rs1; rm } ->
      setf t rd (flag (Fpu.of_int Fpu.double (resolve rm) ity (get t rs1)))
  | Fcvt_of_int { fmt = S; ity; rd; rs1; rm } ->
      set_s t rd (flag (Fpu.of_int Fpu.single (resolve rm) ity (get t rs1)))
  | Fcvt_fmt { fmt = S; rd; rs1; rm } ->
      set_s t rd (flag (Fpu.d_to_s (resolve rm) (rd_d rs1)))
  | Fcvt_fmt { fmt = D; rd; rs1; rm = _ } ->
      setf t rd (flag (Fpu.s_to_d (rd_s rs1)))
  | Fmv_to_int { fmt = D; rd; rs1 } -> set t rd (getf t rs1)
  | Fmv_to_int { fmt = S; rd; rs1 } -> set t rd (sext32 (getf t rs1))
  | Fmv_of_int { fmt = D; rd; rs1 } -> setf t rd (get t rs1)
  | Fmv_of_int { fmt = S; rd; rs1 } -> set_s t rd (Int64.to_int32 (get t rs1))
  | _ -> assert false

(* {1 Atomics} *)

let amo t ~op ~width ~rd ~rs1 ~rs2 =
  let addr = get t rs1 in
  let size = match width with Word -> 4 | Dword -> 8 in
  if Int64.to_int addr land (size - 1) <> 0 then
    raise (Trap (Misaligned_atomic addr));
  let norm = match width with Word -> sext32 | Dword -> Fun.id in
  let unsigned = match width with Word -> zext32 | Dword -> Fun.id in
  match op with
  | Lr ->
      set t rd (norm (Memory.load t.mem addr size));
      t.reservation <- Some addr
  | Sc ->
      let ok = t.reservation = Some addr in
      if ok then Memory.store t.mem addr size (get t rs2);
      t.reservation <- None;
      set t rd (if ok then 0L else 1L)
  | _ ->
      let old = norm (Memory.load t.mem addr size) in
      let src = norm (get t rs2) in
      let pick_u cmp =
        if cmp (Int64.unsigned_compare (unsigned old) (unsigned src)) then old
        else src
      in
      let v =
        match op with
        | Amoswap -> src
        | Amoadd -> Int64.add old src
        | Amoxor -> Int64.logxor old src
        | Amoand -> Int64.logand old src
        | Amoor -> Int64.logor old src
        | Amomin -> if Int64.compare old src <= 0 then old else src
        | Amomax -> if Int64.compare old src >= 0 then old else src
        | Amominu -> pick_u (fun c -> c <= 0)
        | Amomaxu -> pick_u (fun c -> c >= 0)
        | Lr | Sc -> assert false
      in
      Memory.store t.mem addr size v;
      set t rd old

(* {1 Fetch, decode, execute} *)

let[@inline] offset_from base o = Int64.add base (Int64.of_int o)
let[@inline] address t rs1 o = offset_from (get t rs1) o

(** Execute an instruction that doesn't jump; the caller advances [pc]. *)
let execute_straight t (i : Instr.t) ~pc ~bits =
  match i with
  | Lui { rd; imm } -> set t rd (sext32 (Int64.of_int (imm lsl 12)))
  | Auipc { rd; imm } ->
      set t rd (Int64.add pc (sext32 (Int64.of_int (imm lsl 12))))
  | Load { op; rd; rs1; offset } ->
      set t rd (load_value t op (address t rs1 offset))
  | Store { op; rs1; rs2; offset } ->
      Memory.store t.mem (address t rs1 offset) (store_size op) (get t rs2)
  | Op_imm { op; rd; rs1; imm } -> set t rd (imm_alu op (get t rs1) imm)
  | Shift_imm { op; rd; rs1; shamt } ->
      set t rd (shift_alu op (get t rs1) shamt)
  | Op { op; rd; rs1; rs2 } -> set t rd (alu op (get t rs1) (get t rs2))
  | Unary { op; rd; rs1 } -> set t rd (unary op (get t rs1))
  | Fence _ | Fence_i -> () (* the decode cache is always coherent *)
  | Ecall -> t.ecall t
  | Ebreak -> raise (Trap Breakpoint)
  | Csr { op; rd; src; csr } ->
      let v, writes =
        match src with
        | Rs1 r -> (get t r, (r :> int) <> 0 || op = Csrrw)
        | Uimm u -> (Int64.of_int u, u <> 0 || op = Csrrw)
      in
      (* csrrw with rd = zero must not read (no read side effects) *)
      let old = if op = Csrrw && (rd :> int) = 0 then 0L else csr_read t csr in
      (if writes then
         let nv =
           match op with
           | Csrrw -> v
           | Csrrs -> Int64.logor old v
           | Csrrc -> Int64.logand old (Int64.lognot v)
         in
         csr_write t csr nv);
      set t rd old
  | Amo { op; width; rd; rs1; rs2; _ } -> amo t ~op ~width ~rd ~rs1 ~rs2
  | Fload { fmt = D; rd; rs1; offset } ->
      setf t rd (Memory.load t.mem (address t rs1 offset) 8)
  | Fload { fmt = S; rd; rs1; offset } ->
      set_s t rd (Int64.to_int32 (Memory.load t.mem (address t rs1 offset) 4))
  | Fstore { fmt; rs1; rs2; offset } ->
      Memory.store t.mem (address t rs1 offset)
        (if fmt = D then 8 else 4)
        (getf t rs2)
  | Farith _ | Fsqrt _ | Fsign _ | Fma _ | Fcmp _ | Fclass _ | Fcvt_to_int _
  | Fcvt_of_int _ | Fcvt_fmt _ | Fmv_to_int _ | Fmv_of_int _ ->
      fp_exec t ~bits i
  | Jal _ | Jalr _ | Branch _ -> assert false

(* Each case sets [pc] itself: a shared [let next_pc = match ...] would box
   the int64 in every case. *)
let execute t (i : Instr.t) ~len ~bits =
  let pc = pc t in
  let next = offset_from pc len in
  (match i with
  | Jal { rd; offset } ->
      set t rd next;
      set_pc t (offset_from pc offset)
  | Jalr { rd; rs1; offset } ->
      let target = Int64.logand (address t rs1 offset) (-2L) in
      set t rd next;
      set_pc t target
  | Branch { op; rs1; rs2; offset } ->
      set_pc t
        (if branch_taken op (get t rs1) (get t rs2) then offset_from pc offset
         else next)
  | _ ->
      execute_straight t i ~pc ~bits;
      set_pc t next);
  t.instret <- t.instret + 1

(** Execute one instruction. Raises [Trap] on an exception, leaving [pc] at the
    faulting instruction. *)
let step ?trace t =
  let pc = pc t in
  let page = Memory.page_no pc and off = Memory.offset pc in
  match Decode_cache.fetch t.decoded t.mem ~page ~off with
  | exception Decode_cache.Illegal bits ->
      raise (Trap (Illegal_instruction bits))
  | { instr; len; bits } ->
      (match trace with Some f -> f pc instr | None -> ());
      execute t instr ~len ~bits

type outcome =
  | Exited of int
  | Trapped of { trap : trap; pc : int64 }
  | Out_of_fuel

(** Run until the program exits, traps, or executes [fuel] instructions. *)
let run ?(fuel = max_int) ?trace t =
  let rec loop n =
    match t.exit_code with
    | Some code -> Exited code
    | None when n >= fuel -> Out_of_fuel
    | None -> (
        match step ?trace t with
        | () -> loop (n + 1)
        | exception Trap trap -> Trapped { trap; pc = pc t })
  in
  loop 0
