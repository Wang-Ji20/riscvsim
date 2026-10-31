(* Differential test: our encoder vs clang's RISC-V assembler.

   Random instructions are rendered with our disassembler syntax and
   assembled by clang, and clang's .text bytes are compared with ours.
   Pass 1 uses `.option norvc` (32-bit encodings). Pass 2 uses `.option rvc`
   with RVC-friendly instructions, which checks that we compress exactly
   when clang does, to the same 16-bit encoding. Skipped if clang can't
   target riscv64. *)

open Riscv

let march = "rv64gc_zba_zbb_zbs"

let have_clang () =
  Sys.command "clang --print-targets 2>/dev/null | grep -q riscv64" = 0

let clang_text lines ~rvc =
  let dir = Filename.get_temp_dir_name () in
  let src = Filename.temp_file ~temp_dir:dir "xcheck" ".s" in
  let obj = Filename.chop_suffix src ".s" ^ ".o" in
  Out_channel.with_open_bin src (fun oc ->
      Printf.fprintf oc ".option %s\n%s\n"
        (if rvc then "rvc" else "norvc")
        (String.concat "\n" lines));
  let cmd =
    Printf.sprintf
      "clang -target riscv64 -march=%s -mno-relax -c %s -o %s 2>%s.err" march
      (Filename.quote src) (Filename.quote obj) (Filename.quote src)
  in
  if Sys.command cmd <> 0 then
    failwith
      (Printf.sprintf "clang failed:\n%s"
         (In_channel.with_open_bin (src ^ ".err") In_channel.input_all));
  let elf = In_channel.with_open_bin obj In_channel.input_all in
  List.iter Sys.remove [ src; obj; src ^ ".err" ];
  match Elf.read elf with
  | Ok f ->
      (List.find (fun (s : Elf.section) -> s.sname = ".text") f.sections).sdata
  | Error e -> failwith e

(** CSRs whose top two bits are 11 are read-only; clang rejects writes. *)
let clang_accepts (i : Instr.t) =
  match i with
  | Csr { op; src; csr; _ } ->
      let writes =
        match (op, src) with
        | Csrrw, _ -> true
        | _, Rs1 r -> Reg.to_int r <> 0
        | _, Uimm u -> u <> 0
      in
      not (writes && csr lsr 10 = 3)
  | _ -> true

let generate gen n seed =
  let rand = Random.State.make [| seed |] in
  List.init n (fun _ -> QCheck2.Gen.generate1 ~rand gen)
  |> List.filter clang_accepts

let ours_32 i = match Encode.encode i with Ok w -> w | Error e -> failwith e

let check_32 instrs =
  let text = clang_text (List.map Instr.to_string instrs) ~rvc:false in
  let bad = ref 0 in
  List.iteri
    (fun k i ->
      let theirs =
        Int32.to_int (String.get_int32_le text (4 * k)) land 0xffffffff
      in
      let ours = ours_32 i in
      if theirs <> ours then (
        incr bad;
        if !bad <= 20 then
          Printf.printf "  32-bit mismatch: %-40s ours %08x clang %08x\n"
            (Instr.to_string i) ours theirs))
    instrs;
  !bad

let check_rvc instrs =
  let text = clang_text (List.map Instr.to_string instrs) ~rvc:true in
  let bad = ref 0 and compressed = ref 0 in
  let _ =
    List.fold_left
      (fun off i ->
        let parcel = String.get_uint16_le text off in
        let theirs, len =
          if Compressed.is_compressed parcel then (parcel, 2)
          else (Int32.to_int (String.get_int32_le text off) land 0xffffffff, 4)
        in
        let ours =
          match Compressed.compress i with Some c -> c | None -> ours_32 i
        in
        if len = 2 then incr compressed;
        if theirs <> ours then (
          incr bad;
          if !bad <= 20 then
            Printf.printf "  RVC mismatch: %-36s ours %s clang %s\n"
              (Instr.to_string i)
              (Printf.sprintf (if ours < 0x10000 then "%04x" else "%08x") ours)
              (Printf.sprintf (if len = 2 then "%04x" else "%08x") theirs));
        off + len)
      0 instrs
  in
  Printf.printf "  (%d of %d instructions compressed by clang)\n" !compressed
    (List.length instrs);
  !bad

let () =
  if not (have_clang ()) then
    print_endline "xcheck: clang with riscv64 support not found, skipping"
  else
    let all = generate (Gen_instr.instr ~fence_nonempty:true ()) 6000 42 in
    Printf.printf "xcheck: %d random instructions vs clang (-march=%s)\n%!"
      (List.length all) march;
    let bad32 = check_32 all in
    let rvc = generate Gen_instr.compressible 4000 7 in
    Printf.printf
      "xcheck: %d RVC-candidate instructions vs clang with .option rvc\n%!"
      (List.length rvc);
    let bad_rvc = check_rvc rvc in
    if bad32 + bad_rvc > 0 then (
      Printf.printf "xcheck: %d 32-bit and %d RVC mismatches\n" bad32 bad_rvc;
      exit 1)
    else print_endline "xcheck: all encodings identical to clang"
