(** A single RV64GC hart. What [ecall] does depends on its {!env}: {!Os}
    emulates Linux system calls, and a system-mode machine traps. *)

open Instr

(** Raise an illegal-instruction trap; [tval] holds the instruction bits. *)
let illegal bits = Trap.raise_trap Illegal_instruction (Int64.of_int bits)

(** The environment an [ecall] reaches (ROADMAP 1.5). *)
type env =
  | Emulated of (t -> unit)
      (** The simulator handles the call itself, as {!Os} emulates Linux system
          calls for a user-mode process. *)
  | Bare
      (** No environment: [ecall] raises a trap, and any trap stops the run. For
          tests and bare-metal programs that report through HTIF. *)
  | System
      (** A whole machine: every trap, [ecall] included, is delivered to the
          guest's own trap handler (mtvec/stvec), as on real hardware. *)

(* Registers live in [Bytes] rather than an [int64 array]: an array stores
   each int64 as a separate heap block, so every register write allocated.
   [Bytes.get_int64_le]/[set_int64_le] compile to plain memory accesses. *)
and t = {
  x : Bytes.t;  (** 32 little-endian int64s; [x0] is never written *)
  f : Bytes.t;  (** 32 raw bit patterns; singles are NaN-boxed *)
  pc_cell : Bytes.t;  (** the program counter, one int64; see {!pc} *)
  hartid : int;  (** this hart's number, read by the [mhartid] CSR *)
  bus : Bus.t;
  mem : Memory.t;  (** the bus's RAM, which code is fetched from *)
  decoded : Decode_cache.t;
  mutable fflags : int;
  mutable frm : int;
  mutable instret : int;  (** instructions retired; 63 bits never overflow *)
  mutable exit_code : int option;
  mutable env : env;  (** what [ecall] does *)
  csrs : Csr_file.t;  (** privilege level and privileged CSRs *)
}

(** A hart on [bus] (by default a user-mode bus: RAM everywhere). *)
let create ?(hartid = 0) ?(bus = Bus.user ()) () =
  let mem = Bus.ram bus and decoded = Decode_cache.create () in
  (* harts sharing the bus each keep a decode cache: chain the callbacks *)
  let others = mem.on_code_write in
  mem.on_code_write <-
    (fun n ->
      others n;
      Decode_cache.invalidate decoded n);
  {
    x = Bytes.make (32 * 8) '\000';
    f = Bytes.make (32 * 8) '\000';
    pc_cell = Bytes.make 8 '\000';
    hartid;
    bus;
    mem;
    decoded;
    fflags = 0;
    frm = 0;
    instret = 0;
    exit_code = None;
    env = Bare;
    csrs = Csr_file.create ~hartid;
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

(** Write an FP register from an instruction: FP state becomes dirty. *)
let setf t (r : Freg.t) v =
  Csr_file.set_fp_dirty t.csrs;
  set_f t (r :> int) v

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
  let b = t.bus in
  match op with
  | Lb -> sext ~bits:8 (Bus.load b addr 1)
  | Lh -> sext ~bits:16 (Bus.load b addr 2)
  | Lw -> sext32 (Bus.load b addr 4)
  | Ld -> Bus.load b addr 8
  | Lbu -> Bus.load b addr 1
  | Lhu -> Bus.load b addr 2
  | Lwu -> Bus.load b addr 4

let store_size = function Sb -> 1 | Sh -> 2 | Sw -> 4 | Sd -> 8

(* {1 CSRs} *)

(* An unimplemented CSR is an illegal instruction, as on hardware. *)
(* The FP CSRs and the counters are hart state; everything else privileged
   lives in [Csr_file]. Counters: cycle = time = instret (no timing model;
   real time comes with the timer device, ROADMAP 4.1). *)

let csr_read t ~bits csr =
  match csr with
  | 0x001 -> Int64.of_int t.fflags
  | 0x002 -> Int64.of_int t.frm
  | 0x003 -> Int64.of_int ((t.frm lsl 5) lor t.fflags)
  | 0xc00 | 0xc01 | 0xc02 | 0xb00 | 0xb02 -> Int64.of_int t.instret
  | _ -> (
      match Csr_file.read t.csrs csr with Some v -> v | None -> illegal bits)

let csr_write t ~bits csr v64 =
  let v = Int64.to_int v64 in
  let fp_csr f =
    f ();
    Csr_file.set_fp_dirty t.csrs
  in
  match csr with
  | 0x001 -> fp_csr (fun () -> t.fflags <- v land 0x1f)
  | 0x002 -> fp_csr (fun () -> t.frm <- v land 0x7)
  | 0x003 ->
      fp_csr (fun () ->
          t.fflags <- v land 0x1f;
          t.frm <- (v lsr 5) land 0x7)
  | 0xb00 | 0xb02 ->
      (* the written value is what the next instruction reads, so undo the
         increment this instruction is about to make *)
      t.instret <- v - 1
  | _ ->
      if Csr_file.read t.csrs csr = None then illegal bits
      else Csr_file.write t.csrs csr v64

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
    match rm_of t rm with Some rm -> rm | None -> illegal bits
  in
  let flag (v, fl) =
    if fl <> 0 then (
      t.fflags <- t.fflags lor fl;
      Csr_file.set_fp_dirty t.csrs);
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
    Trap.raise_trap
      (if op = Lr then Load_address_misaligned else Store_address_misaligned)
      addr;
  Bus.check_atomic t.bus addr size ~is_load:(op = Lr);
  let norm = match width with Word -> sext32 | Dword -> Fun.id in
  let unsigned = match width with Word -> zext32 | Dword -> Fun.id in
  match op with
  | Lr ->
      set t rd (norm (Memory.load t.mem addr size));
      Bus.reserve t.bus ~hartid:t.hartid addr
  | Sc ->
      let ok = Bus.reservation t.bus ~hartid:t.hartid = Some addr in
      if ok then Bus.store t.bus ~hartid:t.hartid addr size (get t rs2);
      Bus.cancel_reservation t.bus ~hartid:t.hartid;
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
      Bus.store t.bus ~hartid:t.hartid addr size v;
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
      Bus.store t.bus ~hartid:t.hartid (address t rs1 offset) (store_size op)
        (get t rs2)
  | Op_imm { op; rd; rs1; imm } -> set t rd (imm_alu op (get t rs1) imm)
  | Shift_imm { op; rd; rs1; shamt } ->
      set t rd (shift_alu op (get t rs1) shamt)
  | Op { op; rd; rs1; rs2 } -> set t rd (alu op (get t rs1) (get t rs2))
  | Unary { op; rd; rs1 } -> set t rd (unary op (get t rs1))
  | Fence _ | Fence_i -> () (* the decode cache is always coherent *)
  | Ecall -> (
      match t.env with
      | Emulated handle -> handle t
      | Bare | System -> Trap.raise_trap (Trap.ecall_from t.csrs.priv) 0L)
  | Ebreak -> Trap.raise_trap Breakpoint pc
  | Csr { op; rd; src; csr } ->
      let v, writes =
        match src with
        | Rs1 r -> (get t r, (r :> int) <> 0 || op = Csrrw)
        | Uimm u -> (Int64.of_int u, u <> 0 || op = Csrrw)
      in
      if not (Csr_file.check t.csrs csr ~write:writes) then illegal bits;
      (* csrrw with rd = zero must not read (no read side effects) *)
      let old =
        if op = Csrrw && (rd :> int) = 0 then 0L else csr_read t ~bits csr
      in
      (if writes then
         let nv =
           match op with
           | Csrrw -> v
           | Csrrs -> Int64.logor old v
           | Csrrc -> Int64.logand old (Int64.lognot v)
         in
         csr_write t ~bits csr nv);
      set t rd old
  | Amo { op; width; rd; rs1; rs2; _ } -> amo t ~op ~width ~rd ~rs1 ~rs2
  | Fload _ | Fstore _ | Farith _ | Fsqrt _ | Fsign _ | Fma _ | Fcmp _
  | Fclass _ | Fcvt_to_int _ | Fcvt_of_int _ | Fcvt_fmt _ | Fmv_to_int _
  | Fmv_of_int _
    when not (Csr_file.fp_enabled t.csrs) ->
      illegal bits (* mstatus.FS = Off *)
  | Fload { fmt = D; rd; rs1; offset } ->
      setf t rd (Bus.load t.bus (address t rs1 offset) 8)
  | Fload { fmt = S; rd; rs1; offset } ->
      set_s t rd (Int64.to_int32 (Bus.load t.bus (address t rs1 offset) 4))
  | Fstore { fmt; rs1; rs2; offset } ->
      Bus.store t.bus ~hartid:t.hartid (address t rs1 offset)
        (if fmt = D then 8 else 4)
        (getf t rs2)
  | Farith _ | Fsqrt _ | Fsign _ | Fma _ | Fcmp _ | Fclass _ | Fcvt_to_int _
  | Fcvt_of_int _ | Fcvt_fmt _ | Fmv_to_int _ | Fmv_of_int _ ->
      fp_exec t ~bits i
  | Wfi ->
      (* Without interrupts there is nothing to wait for, so wfi is a nop, as
         the spec allows. U-mode may not use it, nor S-mode when TW is set. *)
      let c = t.csrs in
      if c.priv = U || (c.priv = S && Csr_file.has c Csr_file.tw_bit) then
        illegal bits
  | Sfence_vma _ ->
      (* No TLB yet (ROADMAP 3.5), so nothing to flush. *)
      let c = t.csrs in
      if c.priv = U || (c.priv = S && Csr_file.has c Csr_file.tvm_bit) then
        illegal bits
  | Jal _ | Jalr _ | Branch _ | Mret | Sret -> assert false

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
  | Mret ->
      if t.csrs.priv <> M then illegal bits else set_pc t (Csr_file.mret t.csrs)
  | Sret ->
      let c = t.csrs in
      if c.priv = U || (c.priv = S && Csr_file.has c Csr_file.tsr_bit) then
        illegal bits
      else set_pc t (Csr_file.sret c)
  | _ ->
      execute_straight t i ~pc ~bits;
      set_pc t next);
  t.instret <- t.instret + 1

(** Execute one instruction. Raises [Trap] on an exception, leaving [pc] at the
    faulting instruction. *)
let step_or_raise ?trace t =
  let pc = pc t in
  let checked = not t.bus.all_ram in
  if checked then Bus.check_fetch t.bus pc;
  let page = Memory.page_no pc and off = Memory.offset pc in
  match Decode_cache.fetch t.decoded t.mem ~page ~off with
  | exception Decode_cache.Illegal bits ->
      (* a 4-byte instruction whose second half is outside RAM read zeros
         there: that's an access fault, not an illegal instruction *)
      if checked && bits land 3 = 3 then Bus.check_fetch t.bus (Int64.add pc 2L);
      illegal bits
  | { instr; len; bits } ->
      (* the second half of a 4-byte instruction may fall outside RAM *)
      if checked && len = 4 then Bus.check_fetch t.bus (Int64.add pc 2L);
      (match trace with Some f -> f pc instr | None -> ());
      execute t instr ~len ~bits

let delivers_traps t =
  match t.env with System -> true | Emulated _ | Bare -> false

(** Deliver [trap], raised by the instruction at pc, to the guest's handler. The
    trapping instruction didn't retire. *)
let deliver t (trap : Trap.t) =
  set_pc t
    (Csr_file.take_trap t.csrs ~code:(Trap.code trap.cause) ~interrupt:false
       ~tval:trap.tval ~pc:(pc t))

(** Execute one instruction. A trap is delivered to the guest in a [System]
    environment; otherwise [Trap.Trap] is raised, with [pc] left at the faulting
    instruction. *)
let step ?trace t =
  match step_or_raise ?trace t with
  | () -> ()
  | exception Trap.Trap trap when delivers_traps t -> deliver t trap

type outcome =
  | Exited of int
  | Trapped of { trap : Trap.t; pc : int64 }
  | Out_of_fuel

(** Run until the program exits, traps, or takes [fuel] steps. Returns the
    outcome and the number of steps taken. A step is an executed instruction or
    a trap delivered to the guest, so this count, unlike [instret], always
    advances and can't be changed by the program. *)
let run_steps ?(fuel = max_int) ?trace t =
  let rec loop n =
    match t.exit_code with
    | Some code -> (Exited code, n)
    | None when n >= fuel -> (Out_of_fuel, n)
    | None -> (
        match step_or_raise ?trace t with
        | () -> loop (n + 1)
        | exception Trap.Trap trap ->
            if delivers_traps t then (
              deliver t trap;
              loop (n + 1))
            else (Trapped { trap; pc = pc t }, n))
  in
  loop 0

(** Run until the program exits, traps, or takes [fuel] steps. *)
let run ?fuel ?trace t = fst (run_steps ?fuel ?trace t)
