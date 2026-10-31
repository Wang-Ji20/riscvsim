(* Random differential testing against Spike, the RISC-V reference simulator.

   Each test is a random straight-line program: random initial integer and
   FP registers, then a few hundred random instructions from the whole
   RV64GC + Zba/Zbb/Zbs set (no control flow; memory accesses go through x31
   into a scratch area). It ends by storing x1..x30, f0..f31 and fflags to
   a signature region and halting through HTIF. The same ELF runs on Spike
   (+signature) and on riscvsim, and the two signatures must be identical.

   Environment: SPIKE (path to spike), SPIKE_DIFF_COUNT (programs, default
   200), SPIKE_DIFF_LENGTH (instructions each, default 200),
   SPIKE_DIFF_SEED. *)

open Riscv
open Instr
module G = QCheck2.Gen

let ( let+ ) = G.( let+ )
let ( and+ ) = G.( and+ )
let cc = "riscv64-suse-linux-gcc"
let march = "rv64gc_zba_zbb_zbs"

let env_int name default =
  Option.fold ~none:default ~some:int_of_string (Sys.getenv_opt name)

let spike =
  Option.value (Sys.getenv_opt "SPIKE")
    ~default:
      (Filename.concat (Sys.getcwd ())
         "../../../../../riscv-deps/spike/bin/spike")

(* {1 Program generation} *)

let base = Reg.of_int 31 (* scratch base: never written by the body *)
let int_regs = 30 (* x1..x30 are state *)

(* Scratch layout (offsets from x31): FP initial values at 0..255, random
   memory for loads/stores at 256..511, the signature from 512. *)
let mem_lo = 256
let mem_hi = 512
let sig_offset = 512

(** Integer destinations exclude x31, the scratch base. *)
let dest r = if Reg.to_int r = 31 then Reg.of_int 30 else r

let size_of_load = function
  | Lb | Lbu -> 1
  | Lh | Lhu -> 2
  | Lw | Lwu -> 4
  | Ld -> 8

let size_of_store = function Sb -> 1 | Sh -> 2 | Sw -> 4 | Sd -> 8

(** An aligned offset into the load/store area for an access of [size]. *)
let mem_offset size seed =
  mem_lo + (abs seed mod ((mem_hi - mem_lo) / size) * size)

let with_default_rm (i : Instr.t) =
  let rm = Instr.default_rm i in
  match i with
  | Fcvt_of_int c -> Fcvt_of_int { c with rm }
  | Fcvt_fmt c -> Fcvt_fmt { c with rm }
  | i -> i

(** Turn any generated instruction into one that is safe in straight-line code:
    no control flow or traps, memory through x31, and CSR access only to fflags.
*)
let make_safe (i : Instr.t) : Instr.t option =
  match i with
  | Jal _ | Jalr _ | Branch _ | Ecall | Ebreak -> None
  | Load { op; rd; offset; _ } ->
      Some
        (Load
           {
             op;
             rd = dest rd;
             rs1 = base;
             offset = mem_offset (size_of_load op) offset;
           })
  | Store { op; rs2; offset; _ } ->
      Some
        (Store
           {
             op;
             rs1 = base;
             rs2;
             offset = mem_offset (size_of_store op) offset;
           })
  | Fload { fmt; rd; offset; _ } ->
      Some
        (Fload
           {
             fmt;
             rd;
             rs1 = base;
             offset = mem_offset (if fmt = D then 8 else 4) offset;
           })
  | Fstore { fmt; rs2; offset; _ } ->
      Some
        (Fstore
           {
             fmt;
             rs1 = base;
             rs2;
             offset = mem_offset (if fmt = D then 8 else 4) offset;
           })
  | Amo a -> Some (Amo { a with rd = dest a.rd; rs1 = base })
  | Csr c -> Some (Csr { c with rd = dest c.rd; csr = 0x001 (* fflags *) })
  | Lui l -> Some (Lui { l with rd = dest l.rd })
  | Auipc l -> Some (Auipc { l with rd = dest l.rd })
  | Op_imm o -> Some (Op_imm { o with rd = dest o.rd })
  | Shift_imm o -> Some (Shift_imm { o with rd = dest o.rd })
  | Op o -> Some (Op { o with rd = dest o.rd })
  | Unary o -> Some (Unary { o with rd = dest o.rd })
  | Fcmp o -> Some (Fcmp { o with rd = dest o.rd })
  | Fclass o -> Some (Fclass { o with rd = dest o.rd })
  | Fcvt_to_int o -> Some (Fcvt_to_int { o with rd = dest o.rd })
  | Fmv_to_int o -> Some (Fmv_to_int { o with rd = dest o.rd })
  | (Fcvt_of_int _ | Fcvt_fmt _) when Instr.default_rm i <> Dyn ->
      (* always-exact conversions: GNU as accepts no rounding-mode operand *)
      Some (with_default_rm i)
  | Fence _ | Fence_i | Farith _ | Fsqrt _ | Fsign _ | Fma _ | Fcvt_of_int _
  | Fcvt_fmt _ | Fmv_of_int _ ->
      Some i

let int_value =
  G.oneof
    [ G.int64; G.map Int64.of_int (G.int_range (-100) 100); G.map Int64.of_int32 G.int32;
      G.oneof_list [ 0L; -1L; Int64.min_int; Int64.max_int; 0x80000000L; 0xffffffffL ] ]
  [@@ocamlformat "disable"]

let fp_value =
  let doubles =
    [ 0.; -0.; 1.; -1.5; 0.1; Float.infinity; Float.neg_infinity; Float.max_float; Float.min_float;
      4.9e-324; 1e308; 3.4028234663852886e38; 1.1754943508222875e-38 ]
  in
  G.oneof
    [ G.map Int64.bits_of_float (G.oneof_list doubles);
      G.map Int64.bits_of_float G.float;
      G.int64 (* raw bits: NaNs, improperly boxed singles *);
      G.map Fpu.box_s (G.map Int32.bits_of_float G.float) (* NaN-boxed single *);
      G.oneof_list [ Fpu.canonical_nan_d; 0x7ff4000000000000L (* sNaN *); Fpu.box_s 0x7f800001l ] ]
  [@@ocamlformat "disable"]

type program = {
  frm : int;
  ints : int64 list;
  fps : int64 list;
  mem : int64 list;
  body : Instr.t list;
}

let program ~length =
  let+ frm = G.int_range 0 4
  and+ ints = G.list_size (G.return int_regs) int_value
  and+ fps = G.list_size (G.return 32) fp_value
  and+ mem = G.list_size (G.return ((mem_hi - mem_lo) / 8)) int_value
  and+ body =
    G.list_size (G.return length) (Gen_instr.instr ~fence_nonempty:true ())
  in
  { frm; ints; fps; mem; body = List.filter_map make_safe body }

let assembly p =
  let line fmt = Printf.ksprintf (fun s -> s ^ "\n") fmt in
  let dwords vs = String.concat "" (List.map (line "    .dword 0x%Lx") vs) in
  String.concat ""
    ([ "    .section .text.init\n    .globl _start\n_start:\n";
       line "    li t0, 0x6000"; line "    csrs mstatus, t0       # FPU on (Spike boots with FS=off)";
       line "    csrwi frm, %d" p.frm; line "    csrwi fflags, 0"; line "    la x31, scratch" ]
    @ List.init 32 (fun k -> line "    fld f%d, %d(x31)" k (8 * k))
    @ List.mapi (fun k v -> line "    li x%d, 0x%Lx" (k + 1) v) p.ints
    @ [ "    # --- random body ---\n" ]
    @ List.map (fun i -> line "    %s" (Instr.to_string i)) p.body
    @ [ "    # --- signature ---\n" ]
    @ List.init int_regs (fun k -> line "    sd x%d, %d(x31)" (k + 1) (sig_offset + (8 * k)))
    @ List.init 32 (fun k -> line "    fsd f%d, %d(x31)" k (sig_offset + (8 * (int_regs + k))))
    @ [ line "    frflags x1"; line "    sd x1, %d(x31)" (sig_offset + (8 * (int_regs + 32)));
        line "    li t0, 1"; line "    la t1, tohost"; line "    sd t0, 0(t1)"; "1:  j 1b\n";
        "    .section .tohost,\"aw\",@progbits\n    .align 6\n    .globl tohost\ntohost: .dword 0\n";
        "    .globl fromhost\nfromhost: .dword 0\n";
        "    .data\n    .align 6\nscratch:\n"; dwords p.fps; dwords p.mem;
        "    .globl begin_signature\nbegin_signature:\n";
        line "    .space %d" (8 * (int_regs + 32 + 2)); "    .globl end_signature\nend_signature:\n" ])
  [@@ocamlformat "disable"]

(* {1 Running} *)

let scratch_dir =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "riscvsim-spike-%d" (Unix.getpid ()))

let read_file path = In_channel.with_open_bin path In_channel.input_all
let quote = Filename.quote

let build tag p =
  let src = Filename.concat scratch_dir (Printf.sprintf "t%s.S" tag) in
  let elf = Filename.chop_suffix src ".S" ^ ".elf" in
  Out_channel.with_open_bin src (fun oc -> output_string oc (assembly p));
  let cmd =
    Printf.sprintf
      "%s -march=%s -mabi=lp64d -nostdlib -static -T link.ld %s -o %s" cc march
      (quote src) (quote elf)
  in
  if Sys.command (cmd ^ " >/dev/null 2>&1") <> 0 then
    Error (Printf.sprintf "%s does not assemble" src)
  else Ok elf

let on_spike elf =
  let sig_file = elf ^ ".spike.sig" in
  let cmd =
    Printf.sprintf
      "timeout 60 %s --isa=%s +signature=%s +signature-granularity=4 %s \
       >/dev/null 2>&1"
      (quote spike) march (quote sig_file) (quote elf)
  in
  if Sys.command cmd = 0 && Sys.file_exists sig_file then
    Ok (read_file sig_file)
  else Error "spike failed"

let on_riscvsim elf =
  match Elf.read (read_file elf) with
  | Error e -> Error e
  | Ok { image; _ } -> (
      let os = Os.boot ~io:(Os.buffer_io (Buffer.create 16)) image in
      match Os.run ~fuel:10_000_000 os with
      | Cpu.Exited 0 ->
          Option.to_result ~none:"no signature" (Os.signature os.cpu image)
      | Cpu.Trapped { trap; pc } ->
          Error (Format.asprintf "%a at 0x%Lx" Cpu.pp_trap trap pc)
      | _ -> Error "did not halt")

(** The signature as named 64-bit state slots: x1..x30, f0..f31, fflags. *)
let slots signature =
  let words =
    String.split_on_char '\n' (String.trim signature) |> Array.of_list
  in
  let name k =
    if k < int_regs then Reg.abi_name (Reg.of_int (k + 1))
    else if k < int_regs + 32 then Freg.abi_name (Freg.of_int (k - int_regs))
    else "fflags"
  in
  List.init
    (min (Array.length words / 2) (int_regs + 33))
    (fun k -> (name k, words.((2 * k) + 1) ^ words.(2 * k)))

let differences spike ours =
  List.filter_map
    (fun ((name, a), (_, b)) ->
      if a <> b then Some (Printf.sprintf "%s: spike %s, riscvsim %s" name a b)
      else None)
    (List.combine (slots spike) (slots ours))

type verdict =
  | Same of string  (** the common final state *)
  | Differs of { spike : string; ours : string }
  | Broken of string

let check tag p =
  match build tag p with
  | Error e -> Broken e
  | Ok elf -> (
      match (on_spike elf, on_riscvsim elf) with
      | Ok a, Ok b -> if a = b then Same a else Differs { spike = a; ours = b }
      | Error e, _ -> Broken ("spike: " ^ e)
      | _, Error e -> Broken ("riscvsim: " ^ e))

let truncate p n = { p with body = List.filteri (fun k _ -> k < n) p.body }

(** The first instruction after which the states differ: binary search for the
    shortest differing prefix (assumes divergence persists). *)
let first_divergence tag p =
  let differs n =
    match check (Printf.sprintf "%s-%d" tag n) (truncate p n) with
    | Differs _ -> true
    | _ -> false
  in
  let rec search lo hi =
    (* differs hi, not (differs lo) *)
    if hi - lo <= 1 then hi
    else
      let mid = (lo + hi) / 2 in
      if differs mid then search lo mid else search mid hi
  in
  search 0 (List.length p.body)

(** Values (before the instruction) of the registers it names. *)
let operand_values tag p n culprit =
  match check (Printf.sprintf "%s-%d-in" tag n) (truncate p (n - 1)) with
  | Broken e -> [ e ]
  | Same state | Differs { ours = state; _ } ->
      let state = slots state in
      let separators =
        String.map
          (fun c -> if c = ',' || c = '(' || c = ')' then ' ' else c)
          culprit
      in
      let register_name tok =
        match (Reg.of_string tok, Freg.of_string tok) with
        | Some r, _ -> Some (Reg.abi_name r)
        | None, Some f -> Some (Freg.abi_name f)
        | None, None -> None
      in
      String.split_on_char ' ' separators
      |> List.sort_uniq compare
      |> List.filter_map register_name
      |> List.cons "fflags"
      |> List.filter_map (fun name ->
          Option.map (Printf.sprintf "%s=%s" name) (List.assoc_opt name state))

let report k p =
  let tag = string_of_int k in
  let n = first_divergence tag p in
  let culprit = Instr.to_string (List.nth p.body (n - 1)) in
  let after =
    match check (Printf.sprintf "%s-%d" tag n) (truncate p n) with
    | Differs d -> differences d.spike d.ours
    | _ -> []
  in
  Printf.sprintf
    "first divergence at instruction %d: %s\n\
    \      inputs (frm=%d): %s\n\
    \      after: %s"
    n culprit p.frm
    (String.concat " " (operand_values tag p n culprit))
    (String.concat "; " after)

let () =
  if
    Sys.command (cc ^ " --version >/dev/null 2>&1") <> 0
    || not (Sys.file_exists spike)
  then
    print_endline
      "spike diff: needs riscv64-suse-linux-gcc and spike (set SPIKE), skipping"
  else (
    Unix.mkdir scratch_dir 0o755;
    let count = env_int "SPIKE_DIFF_COUNT" 200
    and length = env_int "SPIKE_DIFF_LENGTH" 200 in
    let seed = env_int "SPIKE_DIFF_SEED" 2026 in
    let rand = Random.State.make [| seed |] in
    let failures = ref 0 and instructions = ref 0 in
    for k = 1 to count do
      let p = G.generate1 ~rand (program ~length) in
      instructions := !instructions + List.length p.body;
      match check (string_of_int k) p with
      | Same _ -> ()
      | Broken e ->
          incr failures;
          Printf.printf "  program %d: %s\n%!" k e
      | Differs _ ->
          incr failures;
          if !failures <= 10 then
            Printf.printf "  program %d: %s\n%!" k (report k p)
    done;
    Printf.printf
      "spike diff: %d programs, %d random instructions, seed %d: %d mismatches\n"
      count !instructions seed !failures;
    if !failures = 0 then ignore (Sys.command ("rm -rf " ^ quote scratch_dir))
    else exit 1)
