open Riscv
open Instr
module G = QCheck2.Gen
module T = QCheck2.Test

let ( let+ ) = G.( let+ )
let ( and+ ) = G.( and+ )

(* {1 A tiny harness for example-based tests} *)

let failures = ref 0

let check name cond =
  if not cond then (
    incr failures;
    Printf.printf "FAIL: %s\n%!" name)

let check_eq ~pp name expected actual =
  if expected <> actual then (
    incr failures;
    Printf.printf "FAIL: %s\n  expected: %s\n  actual:   %s\n%!" name
      (pp expected) (pp actual))

let section name = Printf.printf "--- %s\n%!" name

(* {1 Helpers} *)

let assemble ?rvc src =
  match Assembler.assemble ?rvc src with
  | Ok img -> img
  | Error errs ->
      failwith
        (String.concat "; "
           (List.map
              (fun { Assembler.line; msg } -> Printf.sprintf "%d: %s" line msg)
              errs))

let run_image ?(fuel = 10_000_000) img =
  let buf = Buffer.create 256 in
  let hart = (Os.boot ~io:(Os.buffer_io buf) img).hart in
  let outcome = Hart.run ~fuel hart in
  (Buffer.contents buf, outcome, hart)

let run_source ?rvc ?fuel src = run_image ?fuel (assemble ?rvc src)

let pp_outcome = function
  | Hart.Exited c -> Printf.sprintf "exit %d" c
  | Hart.Trapped { trap; pc } ->
      Format.asprintf "trap %a at 0x%Lx" Trap.pp trap pc
  | Hart.Out_of_fuel -> "out of fuel"

let encode_exn i =
  match Encode.encode i with
  | Ok w -> w
  | Error e -> failwith (Instr.to_string i ^ ": " ^ e)

(** Equivalent up to the two canonicalisations compression may perform. *)
let rvc_equivalent original expanded =
  original = expanded
  ||
  match (original, expanded) with
  | ( Op_imm { op = Addi; rd; rs1; imm = 0 },
      Op { op = Add; rd = rd'; rs1 = z; rs2 } ) ->
      rd = rd' && z = Reg.zero && rs1 = rs2
  | ( Op { op = (Add | Xor | Or | And | Addw) as op; rd; rs1; rs2 },
      Op { op = op'; rd = rd'; rs1 = rs1'; rs2 = rs2' } ) ->
      (* commuted *)
      op = op' && rd = rd' && rs2 = rd && rs1' = rd && rs2' = rs1
  | _ -> false

(* {1 Reference models (independent of the implementation)} *)

(** Unsigned 64x64 -> 128-bit product via 16-bit limbs: [(hi, lo)]. *)
let mul128_ref (a : int64) (b : int64) =
  let limbs v =
    Array.init 4 (fun k ->
        Int64.to_int (Int64.shift_right_logical v (16 * k)) land 0xffff)
  in
  let la = limbs a and lb = limbs b in
  let r = Array.make 9 0 in
  for x = 0 to 3 do
    for y = 0 to 3 do
      r.(x + y) <- r.(x + y) + (la.(x) * lb.(y))
    done
  done;
  for k = 0 to 7 do
    r.(k + 1) <- r.(k + 1) + (r.(k) lsr 16);
    r.(k) <- r.(k) land 0xffff
  done;
  let word lo_limb =
    List.fold_left
      (fun acc k ->
        Int64.logor (Int64.shift_left acc 16) (Int64.of_int r.(lo_limb + k)))
      0L [ 3; 2; 1; 0 ]
  in
  (word 4, word 0)

let neg128 (hi, lo) =
  let lo' = Int64.neg lo in
  let hi' = Int64.lognot hi in
  ((if lo = 0L then Int64.succ hi' else hi'), lo')

(** Signed-by-magnitude product, then negate: an independent route to
    mulh/mulhsu. *)
let mulh_ref ~a_signed ~b_signed a b =
  let neg_a = a_signed && Int64.compare a 0L < 0
  and neg_b = b_signed && Int64.compare b 0L < 0 in
  let mag v neg = if neg then Int64.neg v else v in
  let p = mul128_ref (mag a neg_a) (mag b neg_b) in
  fst (if neg_a <> neg_b then neg128 p else p)

let w32 f a b = Int64.of_int32 (f (Int64.to_int32 a) (Int64.to_int32 b))

let clz_ref v =
  let rec go k =
    if k < 0 then 64
    else if Int64.logand (Int64.shift_right_logical v k) 1L = 1L then 63 - k
    else go (k - 1)
  in
  go 63

let cpop_ref v =
  let n = ref 0 in
  for k = 0 to 63 do
    if Int64.logand (Int64.shift_right_logical v k) 1L = 1L then incr n
  done;
  !n

(* {1 Generators} *)

let int64_interesting =
  G.oneof
    [
      G.int64;
      G.map Int64.of_int (G.int_range (-5000) 5000);
      G.map Int64.of_int32 G.int32;
      G.oneof_list
        [ 0L; 1L; -1L; Int64.max_int; Int64.min_int; 0x7fffffffL; 0x80000000L; -0x80000000L;
          0xffffffffL; 0x7ffff800L; 0x7fffffffffff800L; 0x800L; -0x801L; 0xfffffffffffff800L ];
      (let+ shift = G.int_range 0 63 and+ v = G.int_range (-2048) 2047 in
       Int64.shift_left (Int64.of_int v) shift);
    ]
  [@@ocamlformat "disable"]

let float_interesting =
  G.oneof
    [
      G.map Int64.float_of_bits G.int64;
      G.float;
      G.map float_of_int (G.int_range (-100) 100);
      G.oneof_list
        [ 0.; -0.; 1.; -1.; 0.5; 2.5; -2.5; Float.infinity; Float.neg_infinity; Float.nan;
          Float.max_float; Float.min_float; 4.9e-324; 1e308; 3.4028234663852886e38; 1.1754943508222875e-38 ];
    ]
  [@@ocamlformat "disable"]

let single_bits = G.map Int32.bits_of_float float_interesting
let double_bits = G.map Int64.bits_of_float float_interesting

(* {1 Properties} *)

let count = 2000

let prop_encode_decode =
  T.make ~name:"decode (encode i) = Some i" ~count:20_000 ~print:Gen_instr.print
    (Gen_instr.instr ()) (fun i -> Decode.decode (encode_exn i) = Some i)

let prop_decode_strict =
  (* every word the decoder accepts re-encodes to itself (fences excepted:
     their unused fields are ignored), so the decoder isn't lenient *)
  let opcodes =
    [ 0x37; 0x17; 0x6f; 0x67; 0x63; 0x03; 0x23; 0x13; 0x1b; 0x33; 0x3b; 0x73; 0x2f; 0x07;
                  0x27; 0x53; 0x43; 0x47; 0x4b; 0x4f ] [@ocamlformat "disable"]
  in
  let word =
    let+ hi = G.int_range 0 ((1 lsl 25) - 1) and+ op = G.oneof_list opcodes in
    (hi lsl 7) lor op
  in
  T.make ~name:"decode w = Some i ==> encode i = w" ~count:50_000
    ~print:(Printf.sprintf "0x%08x") word (fun w ->
      match Decode.decode w with
      | None -> true
      | Some i -> Encode.encode i = Ok w)

let prop_compress_sound =
  T.make ~name:"expand (compress i) is equivalent to i" ~count:20_000
    ~print:Gen_instr.print
    (G.oneof [ Gen_instr.compressible; Gen_instr.instr () ])
    (fun i ->
      match Compressed.compress i with
      | None -> true
      | Some c -> (
          c land 0xffff = c
          && Compressed.is_compressed c
          &&
          match Compressed.expand c with
          | Some e -> rvc_equivalent i e
          | None -> false))

let prop_asm_roundtrip =
  T.make ~name:"assemble (disassemble i) = i" ~count:3000 ~print:Gen_instr.print
    (Gen_instr.instr ~fence_nonempty:true ()) (fun i ->
      let img = assemble ("_start: " ^ Instr.to_string i) in
      let text = (List.hd img.segments).data in
      Decode.decode (Int32.to_int (Bytes.get_int32_le text 0) land 0xffffffff)
      = Some i)

let prop_li =
  T.make ~name:"li materialises any int64 in <= 8 instructions" ~count:5000
    ~print:Int64.to_string int64_interesting (fun v ->
      let rd = Reg.of_int 10 in
      let seq = Assembler.li_sequence rd v in
      let hart = Hart.create () in
      List.iter (fun i -> Hart.execute hart i ~len:4 ~bits:0) seq;
      List.length seq <= 8 && Hart.get hart rd = v)

let pair64 = G.pair int64_interesting int64_interesting
let print_pair (a, b) = Printf.sprintf "(%Ld, %Ld)" a b

let prop_mulh =
  T.make ~name:"mulh/mulhu/mulhsu match a 16-bit-limb reference" ~count
    ~print:print_pair pair64 (fun (a, b) ->
      Hart.alu Mulhu a b = mulh_ref ~a_signed:false ~b_signed:false a b
      && Hart.alu Mulh a b = mulh_ref ~a_signed:true ~b_signed:true a b
      && Hart.alu Mulhsu a b = mulh_ref ~a_signed:true ~b_signed:false a b
      && Hart.alu Mul a b = snd (mul128_ref a b))

let prop_div =
  T.make ~name:"div/rem: a = q*b + r with |r| < |b| and sign(r) = sign(a)"
    ~count ~print:print_pair pair64 (fun (a, b) ->
      let q = Hart.alu Div a b and r = Hart.alu Rem a b in
      let qu = Hart.alu Divu a b and ru = Hart.alu Remu a b in
      let signed_ok =
        if b = 0L then q = -1L && r = a
        else if a = Int64.min_int && b = -1L then q = a && r = 0L
        else
          Int64.add (Int64.mul q b) r = a
          && Int64.unsigned_compare (Int64.abs r) (Int64.abs b)
             < 0 (* abs min_int = 2^63 unsigned *)
          && (r = 0L || Int64.compare r 0L < 0 = (Int64.compare a 0L < 0))
      in
      let unsigned_ok =
        if b = 0L then qu = -1L && ru = a
        else
          Int64.add (Int64.mul qu b) ru = a && Int64.unsigned_compare ru b < 0
      in
      signed_ok && unsigned_ok)

let prop_word_ops =
  T.make ~name:"*W ops match Int32 arithmetic" ~count ~print:print_pair pair64
    (fun (a, b) ->
      let sh = Int64.to_int b land 31 in
      Hart.alu Addw a b = w32 Int32.add a b
      && Hart.alu Subw a b = w32 Int32.sub a b
      && Hart.alu Mulw a b = w32 Int32.mul a b
      && Hart.alu Sllw a b = w32 (fun x _ -> Int32.shift_left x sh) a b
      && Hart.alu Srlw a b = w32 (fun x _ -> Int32.shift_right_logical x sh) a b
      && Hart.alu Sraw a b = w32 (fun x _ -> Int32.shift_right x sh) a b
      && (Int64.to_int32 b = 0l
         || (Int64.to_int32 a = Int32.min_int && Int64.to_int32 b = -1l)
         || Hart.alu Divw a b = w32 Int32.div a b
            && Hart.alu Remw a b = w32 Int32.rem a b))

let prop_zbb =
  T.make ~name:"Zbb/Zbs ops match naive references" ~count ~print:print_pair
    pair64 (fun (a, b) ->
      let n = Int64.to_int b land 63 in
      let bit = Int64.shift_left 1L n in
      Hart.unary Clz a = Int64.of_int (clz_ref a)
      && Hart.unary Cpop a = Int64.of_int (cpop_ref a)
      && Hart.unary Ctz a
         = Int64.of_int
             (if a = 0L then 64 else 63 - clz_ref (Int64.logand a (Int64.neg a)))
      && Hart.alu Ror (Hart.alu Rol a b) b = a
      && Hart.alu Rol a b
         = Int64.logor (Int64.shift_left a n)
             (if n = 0 then 0L else Int64.shift_right_logical a (64 - n))
      && Hart.unary Rev8 (Hart.unary Rev8 a) = a
      && Hart.alu Bset a b = Int64.logor a bit
      && Hart.alu Bclr a b = Int64.logand a (Int64.lognot bit)
      && Hart.alu Binv a b = Int64.logxor a bit
      && (Hart.alu Bext a b = if Int64.logand a bit = 0L then 0L else 1L)
      && Hart.alu Sh2add a b = Int64.add (Int64.mul a 4L) b
      && (Hart.alu Max a b = if Int64.compare a b >= 0 then a else b)
      && Hart.alu Minu a b = if Int64.unsigned_compare a b <= 0 then a else b)

let prop_memory =
  let addr =
    G.oneof
      [ G.map (fun k -> Int64.of_int (4096 * k - 3)) (G.int_range 1 4);  (* page-crossing *)
        G.map Int64.of_int (G.int_range 0 100_000); G.int64 ] [@ocamlformat "disable"]
  in
  T.make ~name:"memory: load after store returns the stored bytes" ~count
    ~print:(fun (a, v, s) -> Printf.sprintf "addr=0x%Lx v=%Ld size=%d" a v s)
    (G.triple addr G.int64 (G.oneof_list [ 1; 2; 4; 8 ]))
    (fun (addr, v, size) ->
      let m = Memory.create () in
      let neighbour = Int64.add addr (Int64.of_int size) in
      Memory.store m neighbour 1 0x5aL;
      Memory.store m addr size v;
      let mask =
        if size = 8 then -1L else Int64.pred (Int64.shift_left 1L (8 * size))
      in
      Memory.load m addr size = Int64.logand v mask
      && Memory.load m neighbour 1 = 0x5aL)

let fpair = G.pair double_bits double_bits

let print_fpair (a, b) =
  Printf.sprintf "(%h, %h)" (Int64.float_of_bits a) (Int64.float_of_bits b)

let native_bits r =
  if Float.is_nan r then Fpu.canonical_nan_d else Int64.bits_of_float r

let prop_fp_rne =
  T.make ~name:"binary64 RNE arithmetic matches the host FPU" ~count:5000
    ~print:print_fpair fpair (fun (a, b) ->
      let x = Int64.float_of_bits a and y = Int64.float_of_bits b in
      let ours op = fst (Fpu.arith Fpu.double Rne op a b) in
      ours Fadd = native_bits (x +. y)
      && ours Fsub = native_bits (x -. y)
      && ours Fmul = native_bits (x *. y)
      && ours Fdiv = native_bits (x /. y)
      && fst (Fpu.fsqrt Fpu.double Rne a) = native_bits (Float.sqrt x))

let prop_fp_directed =
  T.make
    ~name:
      "directed rounding brackets the exact result (RDN <= RNE <= RUP, 1 ulp \
       apart)" ~count:5000 ~print:print_fpair fpair (fun (a, b) ->
      List.for_all
        (fun op ->
          let r rm = Fpu.arith Fpu.double rm op a b in
          let f rm = Int64.float_of_bits (fst (r rm)) in
          let rne = f Rne
          and rdn = f Rdn
          and rup = f Rup
          and rtz = f Rtz
          and rmm = f Rmm in
          if Float.is_nan rne then true
          else
            let inexact = snd (r Rne) land Fpu.nx <> 0 in
            rdn <= rne && rne <= rup
            && (rdn = rup || Float.succ rdn = rup)
            && inexact = (rdn <> rup)
            && (rtz = rdn || rtz = rup)
            && Float.abs rtz <= Float.abs rne
            && (rmm = rdn || rmm = rup))
        [ Fadd; Fsub; Fmul; Fdiv ])

(** FMA against an exact integer model: operands are [i * 2^k] with small [i]
    and [k], so [a*b + c] scaled by [2^-E] is an exact [int64]. *)
(* Doubles concentrated around the FPU fast-path limits (2^-900, 2^900, and
   their square roots), with short mantissas too, so exact results occur. *)
let double_near_limits =
  let mantissa =
    G.oneof [ G.map float_of_int (G.int_range 1 (1 lsl 53)); G.map float_of_int (G.int_range 1 64) ]
  and exponent =
    G.oneof [ G.int_range (-1130) 1023; G.map (fun d -> -953 + d) (G.int_range (-8) 8);
              G.map (fun d -> 847 + d) (G.int_range (-8) 8); G.map (fun d -> -503 + d) (G.int_range (-8) 8);
              G.map (fun d -> 397 + d) (G.int_range (-8) 8); G.int_range (-60) 0 ]
  in
  G.oneof
    [ float_interesting;
      (let+ m = mantissa and+ e = exponent and+ neg = G.bool in
       Float.ldexp (if neg then -.m else m) e) ]
  [@@ocamlformat "disable"]

let same_approx (x : Fpu.approx) (y : Fpu.approx) =
  Int64.bits_of_float x.r = Int64.bits_of_float y.r
  && x.err = y.err && x.tie = y.tie && x.scale = y.scale

let prop_fpu_fast_paths =
  T.make ~name:"FPU fast paths agree with the exact mul/div/sqrt" ~count:20_000
    ~print:(fun (a, b) -> Printf.sprintf "(%h, %h)" a b)
    (G.pair double_near_limits double_near_limits) (fun (a, b) ->
      same_approx (Fpu.mul a b) (Fpu.mul_exact a b)
      && same_approx (Fpu.div a b) (Fpu.div_exact a b)
      && same_approx (Fpu.sqrt (Float.abs a)) (Fpu.sqrt_exact (Float.abs a)))

(* Everything a trapping instruction must leave unchanged. All-zero pages
   are dropped: allocating one isn't visible. *)
let arch_state (hart : Hart.t) =
  let sorted tbl f =
    List.sort compare (Hashtbl.fold (fun k v acc -> f k v @ acc) tbl [])
  in
  let zero_page = Bytes.make Memory.page_size '\000' in
  ( (Bytes.copy hart.x, Bytes.copy hart.f, Hart.pc hart, hart.instret),
    (hart.fflags, hart.frm, Bus.reservation hart.bus ~hartid:0),
    Csr_file.to_list hart.csrs,
    sorted hart.mem.pages (fun n (p : Memory.page) ->
        if Bytes.equal p.bytes zero_page then [] else [ (n, Bytes.copy p.bytes) ]) )

let prop_precise_traps =
  let reg_value =
    G.oneof [ int64_interesting; G.map (fun n -> Int64.of_int (0x20000 + n)) (G.int_range 0 64) ]
  in
  let gen =
    let+ i = Gen_instr.instr ()
    and+ regs = G.array_size (G.return 32) reg_value
    and+ fregs = G.array_size (G.return 32) double_bits
    and+ frm = G.int_range 0 7
    and+ fs = G.int_range 0 3
    and+ reserved = G.bool in
    (i, regs, fregs, (frm, fs), reserved)
  in
  T.make ~name:"a trapping instruction changes no architectural state" ~count:5000
    ~print:(fun (i, _, _, (frm, fs), _) -> Printf.sprintf "%s (frm=%d, fs=%d)" (Instr.to_string i) frm fs)
    gen (fun (i, regs, fregs, (frm, fs), reserved) ->
      let hart = Hart.create () in
      Array.iteri (Hart.set_x hart) regs;
      Array.iteri (Hart.set_f hart) fregs;
      Hart.set_pc hart 0x10000L;
      hart.frm <- frm;
      Csr_file.set_fs hart.csrs fs;
      if reserved then Bus.reserve hart.bus ~hartid:0 (Hart.get_x hart 10);
      Memory.store hart.mem 0x20000L 8 0x1122334455667788L;
      let before = arch_state hart in
      match Hart.execute hart i ~len:4 ~bits:(encode_exn i) with
      | () -> true
      | exception Trap.Trap _ -> arch_state hart = before)
  [@@ocamlformat "disable"]

(* Interrupting a run with a snapshot and resuming it in a fresh process
   ends in exactly the state of an uninterrupted run. *)
let prop_snapshot_resume =
  let examples =
    lazy
      (Sys.readdir "../examples" |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".s")
      |> List.sort compare
      |> List.map (fun f ->
             (f, assemble (In_channel.with_open_bin ("../examples/" ^ f) In_channel.input_all))))
  in
  let boot img = Os.boot ~io:(Os.buffer_io (Buffer.create 64)) img in
  T.make ~name:"snapshot, restore and resume = uninterrupted run" ~count:100
    ~print:(fun (n, k) -> Printf.sprintf "example %d, snapshot after %d" n k)
    (G.pair (G.int_bound 9) (G.int_bound 3000))
    (fun (n, k) ->
      let _, img = List.nth (Lazy.force examples) n in
      let whole = boot img in
      ignore (Os.run whole);
      let first = boot img in
      ignore (Os.run ~fuel:k first);
      let snap = Snapshot.save first.machine in
      let second = boot img in
      Snapshot.restore second.machine snap = Ok ()
      && Snapshot.save second.machine = snap
      && (ignore (Os.run second);
          Snapshot.save second.machine = Snapshot.save whole.machine))
  [@@ocamlformat "disable"]

(* WARL: whatever is written, reading gives a legal value, and writing that
   value back changes nothing. *)
let prop_csr_warl =
  let implemented =
    List.filter_map
      (fun (n, _) -> if Csr_file.read (Csr_file.create ~hartid:0) n <> None then Some n else None)
      Csr.table
  in
  T.make ~name:"CSR writes are legalised idempotently" ~count:3000
    ~print:(fun (n, v, w) -> Printf.sprintf "csr 0x%x <- 0x%Lx, then 0x%Lx" n v w)
    G.(triple (oneof_list implemented) int64_interesting int64_interesting)
    (fun (n, v, w) ->
      let t = Csr_file.create ~hartid:0 in
      Csr_file.write t (if n = Csr.mstatus then Csr.mideleg else Csr.mstatus) w;
      Csr_file.write t n v;
      let once = Csr_file.read t n in
      Option.iter (Csr_file.write t n) once;
      Csr_file.read t n = once)
  [@@ocamlformat "disable"]

let prop_fma_exact =
  let operand bits lo hi =
    G.pair (G.int_range (-(1 lsl bits)) (1 lsl bits)) (G.int_range lo hi)
  in
  let gen =
    G.triple (operand 20 (-5) 5) (operand 20 (-5) 5) (operand 40 (-10) 10)
  in
  let print ((ia, ea), (ib, eb), (ic, ec)) =
    Printf.sprintf "%d*2^%d * %d*2^%d + %d*2^%d" ia ea ib eb ic ec
  in
  T.make ~name:"fmadd.d rounds the exact a*b+c correctly in all modes (with NX)"
    ~count:5000 ~print gen (fun ((ia, ea), (ib, eb), (ic, ec)) ->
      let scale = min (ea + eb) ec in
      let exact = (ia * (ib lsl (ea + eb - scale))) + (ic lsl (ec - scale)) in
      let a = Float.ldexp (float ia) ea
      and b = Float.ldexp (float ib) eb
      and c = Float.ldexp (float ic) ec in
      let bits = Int64.bits_of_float in
      let ours rm = Fpu.fma Fpu.double rm Fmadd (bits a) (bits b) (bits c) in
      let rne = Float.fma a b c in
      let scaled f = Int64.to_int (Int64.of_float (Float.ldexp f (-scale))) in
      let diff = compare exact (scaled rne) in
      let up = if diff > 0 then Float.succ rne else rne
      and down = if diff < 0 then Float.pred rne else rne in
      let tie =
        diff <> 0
        && 2 * abs (exact - scaled rne)
           = abs (scaled (if diff > 0 then up else down) - scaled rne)
      in
      let expect = function
        | Rne | Dyn -> rne
        | Rdn -> down
        | Rup -> up
        | Rtz -> if rne > 0. then down else up
        | Rmm ->
            if tie && Float.abs (if diff > 0 then up else down) > Float.abs rne
            then if diff > 0 then up else down
            else rne
      in
      List.for_all
        (fun rm ->
          let result, flags = ours rm in
          result = bits (expect rm) && flags land Fpu.nx <> 0 = (diff <> 0))
        [ Rne; Rtz; Rdn; Rup; Rmm ])

let prop_fp_single =
  T.make
    ~name:
      "binary32 ops (via round-to-odd) equal host double-then-single rounding"
    ~count:5000
    ~print:(fun (a, b) ->
      Printf.sprintf "(%h, %h)" (Int32.float_of_bits a) (Int32.float_of_bits b))
    (G.pair single_bits single_bits)
    (fun (a, b) ->
      let x = Int32.float_of_bits a and y = Int32.float_of_bits b in
      let host r =
        if Float.is_nan r then Fpu.canonical_nan_s else Int32.bits_of_float r
      in
      let ours op = fst (Fpu.arith Fpu.single Rne op a b) in
      Fpu.is_nan_s a || Fpu.is_nan_s b
      || ours Fadd = host (x +. y)
         && ours Fsub = host (x -. y)
         && ours Fmul = host (x *. y)
         && ours Fdiv = host (x /. y)
         && fst (Fpu.fsqrt Fpu.single Rne a) = host (Float.sqrt x))

let prop_fp_convert =
  T.make ~name:"fcvt.l.d rtz = truncation; fcvt.d.l rne = Int64.to_float"
    ~count:5000
    ~print:(fun (f, v) -> Printf.sprintf "(%h, %Ld)" f v)
    (G.pair float_interesting int64_interesting)
    (fun (f, v) ->
      let in_range = Float.abs f < 9.2e18 in
      let to_int_ok =
        (not in_range)
        || fst (Fpu.to_int_bits Fpu.double Rtz I64 (Int64.bits_of_float f))
           = Int64.of_float f
      in
      to_int_ok
      && fst (Fpu.of_int Fpu.double Rne I64 v)
         = Int64.bits_of_float (Int64.to_float v))

let prop_shortest_float =
  T.make ~name:"printed doubles parse back to the same value" ~count:5000
    ~print:(Printf.sprintf "%h") float_interesting (fun f ->
      Float.is_nan f
      ||
      let s = Fpu.string_of_double f in
      s = "inf" || s = "-inf" || float_of_string s = f)

(** Random straight-line programs behave identically with and without RVC. *)
let prop_rvc_programs =
  let regs = [ "a0"; "a1"; "a2"; "a3"; "s0"; "s1"; "t0"; "t1" ] in
  let r = G.oneof_list regs in
  let line =
    G.oneof
      [
        (let+ op = G.oneof_list [ "addi"; "andi"; "addiw"; "xori"; "slti" ]
         and+ d = r
         and+ s = r
         and+ imm = G.int_range (-40) 40 in
         Printf.sprintf "%s %s, %s, %d" op d
           (if imm mod 2 = 0 then d else s)
           imm);
        (let+ op =
           G.oneof_list
             [ "add"; "sub"; "and"; "or"; "xor"; "addw"; "subw"; "mul"; "sltu" ]
         and+ d = r
         and+ s = r
         and+ t = r in
         Printf.sprintf "%s %s, %s, %s" op d
           (if String.length t mod 2 = 0 then d else s)
           t);
        (let+ op = G.oneof_list [ "slli"; "srli"; "srai" ]
         and+ d = r
         and+ sh = G.int_range 0 63 in
         Printf.sprintf "%s %s, %s, %d" op d d sh);
        (let+ d = r and+ v = int64_interesting in
         Printf.sprintf "li %s, %Ld" d v);
        (let+ d = r and+ s = r in
         Printf.sprintf "mv %s, %s" d s);
        (let+ d = r and+ off = G.int_range 0 15 in
         Printf.sprintf "sd %s, %d(sp)\nld %s, %d(sp)" d (off * 8) d (off * 8));
        (let+ d = r and+ v = G.int_range (-32) 31 in
         Printf.sprintf "lui %s, %d" d (v land 0xfffff));
      ]
  in
  T.make ~name:"random programs: plain and RVC builds end in the same state"
    ~count:300 ~print:(String.concat "\n")
    (G.list_size (G.int_range 1 40) line)
    (fun lines ->
      let src =
        "addi sp, sp, -128\n" ^ String.concat "\n" lines
        ^ "\nli a7, 93\necall\n"
      in
      let regs rvc =
        let _, outcome, hart = run_source ~rvc src in
        (outcome, List.init 32 (Hart.get_x hart))
      in
      regs false = regs true)

(* The profile counts every execution exactly once, per pc and per mnemonic. *)
let prop_profile =
  let gen =
    let+ instrs = G.array_size (G.return 8) (Gen_instr.instr ())
    and+ pcs = G.list (G.int_bound 7) in
    (instrs, pcs)
  in
  T.make ~name:"profile counts each execution once" ~count:300 gen
    (fun (instrs, pcs) ->
      let p = Stats.create_profile () in
      List.iter (fun n -> Stats.record p (Int64.of_int (4 * n)) instrs.(n)) pcs;
      let total = List.fold_left (fun acc (_, n) -> acc + n) 0 in
      let hot = Stats.hot_pcs p in
      let occurrences n = List.length (List.filter (( = ) n) pcs) in
      total (Stats.by_mnemonic p) = List.length pcs
      && List.for_all
           (fun (pc, (e : Stats.entry)) ->
             e.count = occurrences (Int64.to_int pc / 4))
           hot
      && List.sort (fun a b -> compare b a) (List.map (fun (_, e) -> e.Stats.count) hot)
         = List.map (fun (_, e) -> e.Stats.count) hot)

let properties =
  [ prop_profile; prop_csr_warl; prop_fpu_fast_paths; prop_precise_traps; prop_snapshot_resume; prop_encode_decode; prop_decode_strict; prop_compress_sound; prop_asm_roundtrip; prop_li; prop_mulh;
    prop_div; prop_word_ops; prop_zbb; prop_memory; prop_fp_rne; prop_fp_directed; prop_fma_exact; prop_fp_single;
    prop_fp_convert; prop_shortest_float; prop_rvc_programs ] [@ocamlformat "disable"]

(* {1 Example-based tests} *)

let test_rvc_exhaustive () =
  section "RVC: every 16-bit parcel";
  let legal = ref 0 in
  for h = 0 to 0xffff do
    if Compressed.is_compressed h then
      match Compressed.expand h with
      | None -> ()
      | Some i -> (
          incr legal;
          (match Encode.encode i with
          | Ok _ -> ()
          | Error e ->
              check
                (Printf.sprintf "expansion of 0x%04x encodes (%s)" h e)
                false);
          match Compressed.compress i with
          | Some c ->
              check
                (Printf.sprintf "recompress 0x%04x" h)
                (match Compressed.expand c with
                | Some e -> rvc_equivalent i e
                | None -> false)
          | None -> ())
  done;
  Printf.printf "  %d legal compressed encodings\n" !legal;
  check "c.nop is 0x0001"
    (Compressed.expand 0x0001
    = Some (Op_imm { op = Addi; rd = Reg.zero; rs1 = Reg.zero; imm = 0 }));
  check "0x0000 is illegal" (Compressed.expand 0x0000 = None)

let test_examples () =
  section "golden examples (plain, RVC, via ELF)";
  let dir = "../examples" in
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".s")
    |> List.sort compare
  in
  check "ten examples" (List.length files = 10);
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let src = In_channel.with_open_bin path In_channel.input_all in
      let expected =
        In_channel.with_open_bin
          (Filename.chop_suffix path ".s" ^ ".expected")
          In_channel.input_all
      in
      List.iter
        (fun (mode, img) ->
          let out, outcome, _ = run_image img in
          check_eq ~pp:Fun.id
            (Printf.sprintf "%s [%s] output" f mode)
            expected out;
          check_eq ~pp:pp_outcome
            (Printf.sprintf "%s [%s] exit" f mode)
            (Hart.Exited 0) outcome)
        [
          ("plain", assemble src);
          ("rvc", assemble ~rvc:true src);
          ( "elf",
            match Elf.read (Elf.write (assemble ~rvc:true src)) with
            | Ok { image; _ } -> image
            | Error e -> failwith e );
        ];
      Printf.printf "  %s ok\n%!" f)
    files

let test_assembler_errors () =
  section "assembler diagnostics";
  let errors src =
    match Assembler.assemble src with Ok _ -> [] | Error es -> es
  in
  let has_error ~line ~substr src =
    List.exists
      (fun { Assembler.line = l; msg } ->
        l = line
        &&
        let n = String.length substr in
        let rec find k =
          k + n <= String.length msg
          && (String.sub msg k n = substr || find (k + 1))
        in
        find 0)
      (errors src)
  in
  check "undefined symbol, with line number"
    (has_error ~line:2 ~substr:"undefined symbol nowhere" "nop\nj nowhere\n");
  check "immediate out of range"
    (has_error ~line:1 ~substr:"out of range" "addi a0, a0, 5000");
  check "branch out of range"
    (has_error ~line:1 ~substr:"branch offset"
       "beq a0, a1, far\n.space 8192\nfar: nop");
  check "unknown instruction"
    (has_error ~line:1 ~substr:"unknown instruction" "frobnicate a0");
  check "wrong register class"
    (has_error ~line:1 ~substr:"FP register" "fadd.d fa0, a0, fa1");
  check "duplicate label"
    (has_error ~line:2 ~substr:"duplicate label" "x: nop\nx: nop");
  check "all errors reported" (List.length (errors "bogus\nnop\nbogus2\n") = 2);
  check "local labels resolve" (errors "1: j 1f\n1: j 1b\n" = [])

(* The decode cache must never run stale code: a store over an instruction
   that already ran (here, its second half on the next page) is seen. *)
let test_decode_cache () =
  section "decode cache coherence";
  let addi imm =
    Int64.of_int (encode_exn (Op_imm { op = Addi; rd = Reg.a0; rs1 = Reg.a0; imm }))
  in
  let exit_code src =
    match run_source src with
    | _, Hart.Exited c, _ -> c
    | _, o, _ -> failwith (pp_outcome o)
  in
  check_eq ~pp:string_of_int "store over an executed instruction in .text" 6
    (exit_code
       (Printf.sprintf
          "li a0, 0\n\
           call f\n\
           la t0, f\n\
           li t1, %Ld\n\
           sw t1, 0(t0)\n\
           fence.i\n\
           call f\n\
           li a7, 93\n\
           ecall\n\
           f: addi a0, a0, 1\n\
           ret" (addi 5)));
  check_eq ~pp:string_of_int "instruction crossing a page, second half rewritten" 6
    (exit_code
       (Printf.sprintf
          ".data\n\
           buf: .space 8192\n\
           .text\n\
           la s0, buf\n\
           li t0, 4094\n\
           add s0, s0, t0\n\
           li t1, %Ld\n\
           sh t1, 0(s0)\n\
           srli t1, t1, 16\n\
           sh t1, 2(s0)\n\
           li t1, 0x8082\n\
           sh t1, 4(s0)\n\
           li a0, 0\n\
           jalr ra, 0(s0)\n\
           li t1, %Ld\n\
           srli t1, t1, 16\n\
           sh t1, 2(s0)\n\
           fence.i\n\
           jalr ra, 0(s0)\n\
           li a7, 93\n\
           ecall" (addi 1) (addi 5)))

(* A device with eight 8-byte registers, for bus tests. *)
module Scratch_device = struct
  type t = Bytes.t

  let name = "scratch"
  let size = 64L
  let read t ~offset ~size = Int64.logand (Bytes.get_int64_le t (Int64.to_int offset))
      (if size = 8 then -1L else Int64.pred (Int64.shift_left 1L (8 * size)))
  let write t ~offset ~size v =
    for i = 0 to size - 1 do
      Bytes.set_uint8 t (Int64.to_int offset + i) (Int64.to_int (Int64.shift_right_logical v (8 * i)) land 0xff)
    done
  let tick _ ~now:_ = ()
  let save = Bytes.to_string
  let restore t s = Bytes.blit_string s 0 t 0 64
end
[@@ocamlformat "disable"]

let test_bus () =
  section "bus (system mode)";
  let ram_base = 0x8000_0000L and ram_size = 0x10_0000L in
  let ram_end = Int64.add ram_base ram_size in
  let run_system ?(setup = fun _ -> ()) src =
    let img = assemble src in
    let bus = Bus.system ~ram_base ~ram_size in
    Bus.attach bus ~base:0x1000_0000L
      (Device.pack (module Scratch_device) (Bytes.make 64 '\000'));
    let hart = Hart.create ~bus () in
    (* the assembler links at 0x10000: move the code into RAM *)
    List.iter
      (fun (seg : Image.segment) ->
        Memory.blit_bytes hart.mem (Int64.add ram_base (Int64.sub seg.vaddr 0x10000L)) seg.data)
      img.segments;
    Hart.set_pc hart ram_base;
    setup hart;
    (Hart.run ~fuel:1000 hart, hart)
  in
  let trapped name ~cause ~tval (outcome, _) =
    check_eq ~pp:pp_outcome name
      (Hart.Trapped { trap = { cause; tval }; pc = 0L })
      (match outcome with
      | Hart.Trapped t -> Hart.Trapped { t with pc = 0L }
      | o -> o)
  in
  trapped "load outside RAM and devices faults" ~cause:Load_access_fault ~tval:0x40L
    (run_system "li t0, 0x40\nld a0, 0(t0)");
  trapped "fetch outside RAM faults" ~cause:Instruction_access_fault ~tval:0x1000_0000L
    (run_system "li t0, 0x10000000\njr t0");
  let outcome, hart =
    run_system
      ~setup:(fun h -> Memory.store h.mem (Int64.sub ram_end 8L) 8 0x1111L)
      (Printf.sprintf "li t0, 0x%Lx\nli t1, -1\nsd t1, 0(t0)" (Int64.sub ram_end 4L))
  in
  trapped "store crossing the end of RAM faults" ~cause:Store_access_fault
    ~tval:(Int64.sub ram_end 4L) (outcome, hart);
  check "... and writes nothing"
    (Memory.load hart.mem (Int64.sub ram_end 8L) 8 = 0x1111L);
  trapped "4-byte instruction straddling the end of RAM faults at its second half"
    ~cause:Instruction_access_fault ~tval:(Int64.sub ram_end 0L)
    (run_system ~setup:(fun h ->
         Memory.store h.mem (Int64.sub ram_end 2L) 2 0x0013L (* low half of nop *))
       (Printf.sprintf "li t0, 0x%Lx\njr t0" (Int64.sub ram_end 2L)));
  trapped "AMO on a device faults" ~cause:Store_access_fault ~tval:0x1000_0008L
    (run_system "li t0, 0x10000008\namoadd.d a0, a1, (t0)");
  let outcome, hart =
    run_system
      "li t0, 0x10000000\nli t1, 0x123456789\nsd t1, 8(t0)\nlw a0, 8(t0)\nlbu a1, 12(t0)\nebreak"
  in
  ignore outcome;
  check "device registers are read and written through the bus"
    (Hart.get_x hart 10 = 0x23456789L && Hart.get_x hart 11 = 1L);
  let bus = Bus.system ~ram_base ~ram_size in
  let dev = Device.pack (module Scratch_device) (Bytes.make 64 '\000') in
  check "a device may not overlap RAM"
    (match Bus.attach bus ~base:(Int64.sub ram_base 32L) dev with
    | () -> false
    | exception Invalid_argument _ -> true);
  check "Bus.within at the top of the address space"
    (Bus.within ~base:(-16L) ~len:16L (-4L) 4
    && (not (Bus.within ~base:(-16L) ~len:16L (-4L) 8))
    && Bus.within ~base:(-16L) ~len:16L (-16L) 8
    && not (Bus.within ~base:(-16L) ~len:16L 0L 1))

(* Records the time of every tick. *)
module Tick_log = struct
  type t = int list ref

  let name = "tick-log"
  let size = 8L
  let read _ ~offset:_ ~size:_ = 0L
  let write _ ~offset:_ ~size:_ _ = ()
  let tick t ~now = t := now :: !t
  let save t = String.concat "," (List.map string_of_int !t)
  let restore t s =
    t := if s = "" then [] else List.map int_of_string (String.split_on_char ',' s)
end

let test_machine () =
  section "machine";
  let load_code bus src =
    let img = assemble src in
    List.iter
      (fun (seg : Image.segment) -> Memory.blit_bytes (Bus.ram bus) seg.vaddr seg.data)
      img.segments
  in
  let m = Machine.create ~harts:2 (Bus.user ()) in
  load_code m.bus "csrr a1, 0xf14\nloop: addi a0, a0, 1\nj loop";
  Array.iter (fun h -> Hart.set_pc h 0x10000L) m.harts;
  let outcome = Machine.run ~fuel:25 ~slice:10 m in
  check_eq ~pp:pp_outcome "two harts: fuel runs out" Hart.Out_of_fuel outcome;
  check_eq ~pp:(fun (a, b, c) -> Printf.sprintf "%d %d %d" a b c)
    "harts take turns in slices: 10 + 10 + 5" (15, 10, 25)
    ((Machine.hart m 0).instret, (Machine.hart m 1).instret, m.now);
  check "each hart reads its own mhartid"
    (Hart.get_x (Machine.hart m 0) 11 = 0L && Hart.get_x (Machine.hart m 1) 11 = 1L);
  let bus = Bus.system ~ram_base:0x8000_0000L ~ram_size:0x1000L in
  let log = ref [] in
  Bus.attach bus ~base:0x1000_0000L (Device.pack (module Tick_log) log);
  let m = Machine.create bus in
  Memory.store (Bus.ram bus) 0x8000_0000L 4 0x0000006fL (* j . *);
  Hart.set_pc (Machine.hart m 0) 0x8000_0000L;
  ignore (Machine.run ~fuel:30 ~slice:10 m);
  check_eq ~pp:(fun l -> String.concat "," (List.map string_of_int l))
    "devices tick after each round, with the virtual time" [ 10; 20; 30 ] (List.rev !log);
  (* every hart's decode cache sees writes, not just the last one created *)
  let m = Machine.create ~harts:2 (Bus.user ()) in
  load_code m.bus "addi a0, a0, 1\nebreak";
  let h0 = Machine.hart m 0 in
  Hart.set_pc h0 0x10000L;
  Hart.step h0;
  Memory.store (Bus.ram m.bus) 0x10000L 4
    (Int64.of_int (encode_exn (Op_imm { op = Addi; rd = Reg.a0; rs1 = Reg.a0; imm = 5 })));
  Hart.set_pc h0 0x10000L;
  Hart.step h0;
  check_eq ~pp:Int64.to_string "hart 0 sees code rewritten after hart 1 was created" 6L
    (Hart.get_x h0 10);
  let bus = Bus.system ~ram_base:0x8000_0000L ~ram_size:0x1000L in
  let state = Bytes.make 64 '\000' in
  Bus.attach bus ~base:0x1000_0000L (Device.pack (module Scratch_device) state);
  let m = Machine.create bus in
  Bytes.set_int64_le state 8 0xabcdL;
  let snap = Snapshot.save m in
  Bytes.fill state 0 64 '\000';
  check "snapshots restore device state"
    (Snapshot.restore m snap = Ok () && Bytes.get_int64_le state 8 = 0xabcdL);
  check "a corrupt snapshot is rejected"
    (Result.is_error (Snapshot.restore m ("X" ^ String.sub snap 1 (String.length snap - 1))));
  check "a truncated snapshot is rejected"
    (Result.is_error (Snapshot.restore m (String.sub snap 0 (String.length snap - 3))));
  check "a snapshot from a different machine is rejected"
    (Result.is_error (Snapshot.restore (Machine.create ~harts:2 (Bus.user ())) snap));
  (* hart 0: lr.d then sc.d at 0x20000; hart 1 stores in between *)
  let sc_result ~other_store =
    let img =
      assemble
        "li t0, 0x20000\nlr.d a0, (t0)\nreserved: li a1, 7\nsc.d a2, a1, (t0)\n\
         done: j done\nother: sd a1, 0(t5)"
    in
    let sym name = List.assoc name img.symbols in
    let m = Machine.create ~harts:2 (Bus.user ()) in
    List.iter
      (fun (seg : Image.segment) -> Memory.blit_bytes (Bus.ram m.bus) seg.vaddr seg.data)
      img.segments;
    let h0 = Machine.hart m 0 and h1 = Machine.hart m 1 in
    Hart.set_pc h0 img.entry;
    while Hart.pc h0 <> sym "reserved" do Hart.step h0 done;
    (match other_store with
     | `None -> ()
     | `Own -> Bus.store m.bus ~hartid:0 0x20000L 8 5L
     | `Other addr ->
         Hart.set_x h1 30 addr;
         Hart.set_pc h1 (sym "other");
         Hart.step h1);
    while Hart.pc h0 <> sym "done" do Hart.step h0 done;
    Hart.get_x h0 12
  in
  check_eq ~pp:Int64.to_string "sc succeeds when nobody else stored" 0L (sc_result ~other_store:`None);
  check_eq ~pp:Int64.to_string "sc fails after another hart stores to the reserved address" 1L
    (sc_result ~other_store:(`Other 0x20004L));
  check_eq ~pp:Int64.to_string "another hart's store elsewhere leaves the reservation" 0L
    (sc_result ~other_store:(`Other 0x20040L));
  check_eq ~pp:Int64.to_string "the hart's own store leaves its reservation" 0L
    (sc_result ~other_store:`Own);
  (* a hart stuck in a trap loop: mtvec = 0 holds an illegal instruction.
     Delivered traps retire nothing, so fuel must count steps, not instret. *)
  let m = Machine.create (Bus.user ()) in
  (Machine.hart m 0).env <- System;
  check_eq ~pp:pp_outcome "fuel runs out even when every step traps" Hart.Out_of_fuel
    (Machine.run ~fuel:100 ~slice:30 m);
  check_eq ~pp:string_of_int "... after exactly the given number of steps" 100 m.now;
  let m = Machine.create (Bus.user ()) in
  load_code m.bus "ecall";
  Hart.set_pc (Machine.hart m 0) 0x10000L;
  check_eq ~pp:pp_outcome "without an emulated environment, ecall traps"
    (Hart.Trapped { trap = { cause = Ecall_from_m; tval = 0L }; pc = 0x10000L })
    (Machine.run ~fuel:10 m)
[@@ocamlformat "disable"]

(* Run [src] on one hart that delivers traps to its own handlers, until it
   settles in a [spin: j spin] loop. *)
let run_system_program src =
  let img = assemble src in
  let m = Machine.create (Bus.user ()) in
  List.iter
    (fun (seg : Image.segment) -> Memory.blit_bytes (Bus.ram m.bus) seg.vaddr seg.data)
    img.segments;
  let hart = Machine.hart m 0 in
  hart.env <- System;
  Hart.set_pc hart img.entry;
  ignore (Machine.run ~fuel:200 m);
  (hart, fun name -> List.assoc name img.symbols)

let test_csr_file () =
  section "privileged CSRs";
  let module C = Csr_file in
  let t = C.create ~hartid:0 in
  check "reset: M-mode, FPU off, 64-bit U and S"
    (t.C.priv = M && C.fs t = 0 && C.read t Csr.mstatus = Some C.xl_fields);
  C.write t Csr.mstatus (Int64.shift_left 2L 11);
  check "MPP = 2 (reserved) reads as U" (Int64.logand t.mstatus C.mpp_field = 0L);
  C.write t Csr.mstatus C.fs_field;
  check "SD is set exactly when FS is Dirty" (Int64.logand t.mstatus C.sd_bit <> 0L);
  C.write t Csr.sstatus 0L;
  check "sstatus is a view of mstatus" (C.fs t = 0 && Int64.logand t.mstatus C.sd_bit = 0L);
  C.write t Csr.mideleg C.ssip;
  C.write t Csr.mie (-1L);
  check "sie shows only delegated interrupts" (C.read t Csr.sie = Some C.ssip);
  C.write t Csr.satp (Int64.shift_left 8L 60);
  check "satp ignores unsupported modes (Sv39 comes in Phase 3)" (t.satp = 0L);
  let allowed priv n ~write = t.priv <- priv; C.check t n ~write in
  check "U-mode can't touch mstatus" (not (allowed U Csr.mstatus ~write:false));
  check "S-mode reads sstatus" (allowed S Csr.sstatus ~write:true);
  check "S-mode can't read mstatus" (not (allowed S Csr.mstatus ~write:false));
  check "writing a read-only CSR is illegal even in M" (not (allowed M Csr.mhartid ~write:true));
  check "fcsr needs the FPU on" (not (allowed M Csr.fcsr ~write:false));
  check "U-mode counters need mcounteren and scounteren"
    (not (allowed U Csr.cycle ~write:false));
  t.mcounteren <- 1L;
  check "... mcounteren alone is enough for S" (allowed S Csr.cycle ~write:false);
  check "... but not for U" (not (allowed U Csr.cycle ~write:false));
  t.scounteren <- 1L;
  check "... both enable U" (allowed U Csr.cycle ~write:false);
  t.mstatus <- Int64.logor t.mstatus C.tvm_bit;
  check "TVM forbids satp in S-mode" (not (allowed S Csr.satp ~write:false));
  let hart, _ = run_system_program "li t0, 100\ncsrw minstret, t0\ncsrr a0, minstret\nspin: j spin" in
  check_eq ~pp:Int64.to_string "the next instruction reads the minstret just written" 100L
    (Hart.get_x hart 10);
  let _, _, hart = run_source "fadd.d ft0, ft0, ft0\nebreak" in
  check "an FP write makes FS Dirty" (Csr_file.fs hart.csrs = 3);
  let hart = Hart.create () in
  Memory.store hart.mem 0x10000L 4 (Int64.of_int (encode_exn (Fmv_of_int { fmt = D; rd = Freg.of_int 0; rs1 = Reg.zero })));
  Hart.set_pc hart 0x10000L;
  check_eq ~pp:pp_outcome "an FP instruction with the FPU off is illegal"
    (Hart.Trapped { trap = { cause = Illegal_instruction; tval = 0xf2000053L }; pc = 0x10000L })
    (Hart.run ~fuel:1 hart)
;
  (* privileged instructions: run one on a fresh hart at a given privilege *)
  let run_priv ?(setup = fun _ -> ()) priv i =
    let hart = Hart.create () in
    Memory.store hart.mem 0x10000L 4 (Int64.of_int (encode_exn i));
    Hart.set_pc hart 0x10000L;
    hart.csrs.priv <- priv;
    setup hart.csrs;
    let outcome = Hart.run ~fuel:1 hart in
    (outcome, hart)
  in
  let illegal name ?setup priv i =
    let outcome, _ = run_priv ?setup priv i in
    check name
      (match outcome with
       | Hart.Trapped { trap = { cause = Illegal_instruction; _ }; _ } -> true
       | _ -> false)
  in
  let sfence = Sfence_vma { rs1 = Reg.zero; rs2 = Reg.zero } in
  let set_bit b (c : Csr_file.t) = c.mstatus <- Int64.logor c.mstatus b in
  illegal "mret in S-mode is illegal" S Mret;
  illegal "sret in U-mode is illegal" U Sret;
  illegal "sret in S-mode with TSR is illegal" S Sret ~setup:(set_bit Csr_file.tsr_bit);
  illegal "wfi in U-mode is illegal" U Wfi;
  illegal "wfi in S-mode with TW is illegal" S Wfi ~setup:(set_bit Csr_file.tw_bit);
  illegal "sfence.vma in U-mode is illegal" U sfence;
  illegal "sfence.vma in S-mode with TVM is illegal" S sfence ~setup:(set_bit Csr_file.tvm_bit);
  let _, hart =
    run_priv M Mret ~setup:(fun c ->
        c.mstatus <- Int64.logor c.mstatus (Int64.logor Csr_file.mpie_bit (Int64.shift_left 1L 11));
        c.mepc <- 0x4000L)
  in
  let c = hart.csrs in
  check "mret: to MPP (S), at mepc" (c.priv = S && Hart.pc hart = 0x4000L);
  check "mret: MIE <- MPIE, MPIE <- 1, MPP <- U"
    (Csr_file.has c Csr_file.mie_bit && Csr_file.has c Csr_file.mpie_bit
     && Int64.logand c.mstatus Csr_file.mpp_field = 0L);
  let _, hart =
    run_priv S Sret ~setup:(fun c ->
        c.mstatus <- Int64.logor c.mstatus Csr_file.mprv_bit;
        c.sepc <- 0x5000L)
  in
  let c = hart.csrs in
  check "sret: to SPP (U), at sepc, SPIE <- 1, MPRV cleared"
    (c.priv = U && Hart.pc hart = 0x5000L && Csr_file.has c Csr_file.spie_bit
     && not (Csr_file.has c Csr_file.mprv_bit));
  let outcome, hart = run_priv S Wfi in
  check "wfi in S-mode is a nop" (outcome = Hart.Out_of_fuel && Hart.pc hart = 0x10004L)
[@@ocamlformat "disable"]

let test_traps () =
  section "trap delivery";
  let x hart n = Hart.get_x hart n in
  let hart, sym =
    run_system_program
      "la t0, handler\ncsrw mtvec, t0\nli a0, 0\n\
       call_site: ecall\naddi a0, a0, 100\nspin: j spin\n\
       handler: csrr a1, mcause\ncsrr a2, mepc\naddi t1, a2, 4\ncsrw mepc, t1\n\
       addi a0, a0, 1\nmret"
  in
  check_eq ~pp:(fun (a, b, c) -> Printf.sprintf "%Ld %Ld %Lx" a b c)
    "M-mode ecall: handler sees cause 11 and mepc, mret resumes after it"
    (101L, 11L, sym "call_site") (x hart 10, x hart 11, x hart 12);
  let hart, sym =
    run_system_program
      "la t0, mhandler\ncsrw mtvec, t0\nla t0, shandler\ncsrw stvec, t0\n\
       li t0, 0x100\ncsrw medeleg, t0\nla t0, user\ncsrw mepc, t0\nmret\n\
       user: ecall\n\
       bad: csrr a0, mstatus\n\
       spin: j spin\n\
       shandler: csrr s0, scause\ncsrr s1, sepc\ncsrr s2, sstatus\n\
       addi t1, s1, 4\ncsrw sepc, t1\nsret\n\
       mhandler: csrr s3, mcause\ncsrr s4, mtval\ncsrr s5, mstatus\ncsrr s6, mepc\n\
       mspin: j mspin"
  in
  check_eq ~pp:(fun (a, b, c) -> Printf.sprintf "%Ld %Lx %Ld" a b c)
    "a delegated U-mode ecall reaches S-mode with SPP = U"
    (8L, sym "user", 0L) (x hart 8, x hart 9, Int64.logand (x hart 18) 0x100L);
  check_eq ~pp:(fun (a, b, c, d) -> Printf.sprintf "%Ld %Lx %Ld %Lx" a b c d)
    "an undelegated U-mode fault reaches M-mode with MPP = U and the bits in mtval"
    (2L, 0x30002573L, 0L, sym "bad")
    (x hart 19, x hart 20, Int64.logand (x hart 21) 0x1800L, x hart 22);
  check "... and the hart is in M-mode" (hart.csrs.priv = M);
  let hart, sym =
    run_system_program
      "la t0, handler\nori t0, t0, 1\ncsrw mtvec, t0\necall\nspin: j spin\n\
       .align 6\nhandler: csrr a0, mcause\nhspin: j hspin"
  in
  check "vectored mtvec: exceptions still go to the base address"
    (x hart 10 = 11L && Hart.pc hart = sym "hspin");
  let hart, _ =
    run_system_program
      "la t0, handler\ncsrw mtvec, t0\nli t0, 0x800\ncsrw mstatus, t0\n\
       la t0, super\ncsrw mepc, t0\nmret\nsuper: ecall\nspin: j spin\n\
       handler: csrr a0, mcause\ncsrr a1, mstatus\nhspin: j hspin"
  in
  check "an S-mode ecall has cause 9 and MPP = S"
    (x hart 10 = 9L && Int64.logand (x hart 11) 0x1800L = 0x800L)
[@@ocamlformat "disable"]

let test_semantics () =
  section "semantics";
  let regs_after src =
    let _, _, hart = run_source src in
    hart
  in
  let hart =
    regs_after "li t0, 5\naddi zero, t0, 1\nmv a0, zero\nli a7, 93\necall"
  in
  check "x0 is hard-wired to zero" ((Hart.get_x hart 0) = 0L);
  let _, outcome, _ = run_source ".word 0" in
  check "all-zero word is an illegal instruction"
    (match outcome with
    | Hart.Trapped { trap = { cause = Illegal_instruction; tval = 0L }; pc = 0x10000L } -> true
    | _ -> false);
  let _, outcome, _ = run_source "ebreak" in
  check "ebreak traps"
    (match outcome with
    | Hart.Trapped { trap = { cause = Breakpoint; tval = 0x10000L }; _ } -> true
    | _ -> false);
  let _, outcome, _ = run_source "li a0, 0x20001\namoadd.w a1, a1, (a0)" in
  check "misaligned AMO traps"
    (match outcome with
    | Hart.Trapped { trap = { cause = Store_address_misaligned; tval = 0x20001L }; _ } -> true
    | _ -> false);
  let _, outcome, _ = run_source "csrr a0, 0x7c0" in
  check "unknown CSR traps"
    (match outcome with
    | Hart.Trapped { trap = { cause = Illegal_instruction; tval = 0x7c002573L }; _ } -> true
    | _ -> false);
  let _, outcome, _ = run_source "loop: j loop" ~fuel:1000 in
  check "fuel limit" (outcome = Hart.Out_of_fuel);
  let htif =
    Os.boot
      ~io:(Os.buffer_io (Buffer.create 16))
      (assemble
         "li t0, 7\n\
          la t1, tohost\n\
          sd t0, 0(t1)\n\
          spin: j spin\n\
          .data\n\
          tohost: .dword 0")
  in
  check "HTIF tohost write (3 << 1 | 1) exits with status 3"
    (Os.run ~fuel:100_000 htif = Hart.Exited 3);
  let hart =
    regs_after
      "li t0, 3\n\
       fcvt.s.w ft0, t0\n\
       fmv.x.d a0, ft0\n\
       fmv.x.w a1, ft0\n\
       fmv.d.x ft1, zero\n\
       fadd.s ft2, ft1, ft0\n\
       fmv.x.w a2, ft2\n\
       li a7, 10\n\
       ecall"
  in
  check "singles are NaN-boxed in FP registers"
    (Int64.shift_right_logical (Hart.get_x hart 10) 32 = 0xffffffffL);
  check "fmv.x.w sign-extends the raw bits" ((Hart.get_x hart 11) = 0x40400000L);
  check "unboxed single operand reads as canonical NaN"
    ((Hart.get_x hart 12) = 0x7fc00000L);
  let hart =
    regs_after
      "li t0, 2\n\
       fsrm t0\n\
       li t1, 7\n\
       fcvt.d.l ft0, t1\n\
       li t2, 2\n\
       fcvt.d.l ft1, t2\n\
       fdiv.d ft2, ft0, ft1\n\
       fcvt.l.d a0, ft2\n\
       frrm a1\n\
       li a7, 10\n\
       ecall"
  in
  check "dynamic rounding mode from frm (rdn: 3.5 -> 3)"
    ((Hart.get_x hart 10) = 3L && (Hart.get_x hart 11) = 2L);
  let out, _, _ =
    run_source
      "li a0, 1\n\
       la a1, m\n\
       li a2, 3\n\
       li a7, 64\n\
       ecall\n\
       li a7, 214\n\
       li a0, 0\n\
       ecall\n\
       mv s0, a0\n\
       li a0, 0x100000\n\
       add a0, a0, s0\n\
       ecall\n\
       sub a0, a0, s0\n\
       li a7, 1\n\
       ecall\n\
       li a7, 10\n\
       ecall\n\
       .data\n\
       m: .ascii \"ok\\n\""
  in
  check_eq ~pp:Fun.id "write + brk syscalls" "ok\n1048576" out;
  let img =
    assemble ~rvc:true "_start: nop\nfoo: ret\n.data\nbar: .dword foo"
  in
  match Elf.read (Elf.write img) with
  | Ok f ->
      check "ELF round trip keeps the entry point" (f.image.entry = img.entry);
      check "ELF round trip keeps symbols"
        (List.assoc_opt "foo" f.image.symbols = List.assoc_opt "foo" img.symbols);
      check "ELF round trip keeps segments"
        (List.map
           (fun (s : Image.segment) -> (s.vaddr, s.data))
           f.image.segments
        = List.map (fun (s : Image.segment) -> (s.vaddr, s.data)) img.segments)
  | Error e -> check ("ELF read: " ^ e) false

(** FPU cases where riscvsim once disagreed with Spike (found by
    test/spike); each row is (description, op, a, b, c, rm, result, flags). *)
let spike_fpu_regressions =
  [ ( "fmadd.d: product far below c's ulp still sets NX and rounds up",
      Fmadd, 0x1e60000000000000L, 0x1aff8b1aecaabda7L, 0x7761f8528ef497f3L, Rup,
      0x7761f8528ef497f4L, Fpu.nx );
    ( "fnmadd.d: overflowing product with c below its ulp sets OF under RDN",
      Fnmadd, 0x6c81b5e4eca24a0cL, 0xe8a9b915ec868722L, 0x931df157a95ce1f0L, Rdn,
      0x7fefffffffffffffL, Fpu.ofl lor Fpu.nx );
    ( "fnmsub.d: finite product, overflowing sum rounds to max_float under RTZ",
      Fnmsub, 0xcf3cfdbff162e068L, 0x6f139457560b6b8eL, 0x7fefffffffffffffL, Rtz,
      0x7fefffffffffffffL, Fpu.ofl lor Fpu.nx ) ]
[@@ocamlformat "disable"]

let test_fpu_regressions () =
  section "FPU regressions found by Spike differential testing";
  List.iter
    (fun (what, op, a, b, c, rm, result, flags) ->
      let r, f = Fpu.fma Fpu.double rm op a b c in
      check_eq ~pp:(fun (r, f) -> Printf.sprintf "0x%Lx flags=0x%x" r f) what (result, flags) (r, f))
    spike_fpu_regressions

let () =
  test_rvc_exhaustive ();
  test_fpu_regressions ();
  test_assembler_errors ();
  test_semantics ();
  test_decode_cache ();
  test_bus ();
  test_machine ();
  test_csr_file ();
  test_traps ();
  test_examples ();
  section "properties (QCheck2)";
  let qcheck_failures =
    QCheck_base_runner.run_tests ~colors:false ~verbose:true properties
  in
  if !failures > 0 || qcheck_failures > 0 then (
    Printf.printf "\n%d example failures, %d property failures\n" !failures
      qcheck_failures;
    exit 1)
  else print_endline "\nall tests passed"
