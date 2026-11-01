open Riscv
open Cmdliner

let read_file path = In_channel.with_open_bin path In_channel.input_all

(** Load a program: ELF files as they are, anything else as assembly. *)
let load_image ~rvc path =
  let src = read_file path in
  if Elf.is_elf src then
    match Elf.read src with
    | Ok { etype = 2 | 3; image; _ } -> Ok image
    | Ok _ -> Error (path ^ ": not an executable (relocatable object?)")
    | Error e -> Error (path ^ ": " ^ e)
  else
    match Assembler.assemble ~rvc src with
    | Ok img -> Ok img
    | Error errs ->
        Error
          (String.concat "\n"
             (List.map
                (fun { Assembler.line; msg } ->
                  Printf.sprintf "%s:%d: error: %s" path line msg)
                errs))

let dump_regs (hart : Hart.t) =
  let open Printf in
  eprintf "pc = 0x%016Lx  instret = %d\n" (Hart.pc hart) hart.instret;
  for r = 0 to 31 do
    eprintf "%-4s = 0x%016Lx%s"
      (Reg.abi_name (Reg.of_int r))
      (Hart.get_x hart r)
      (if r mod 4 = 3 then "\n" else "  ")
  done;
  for r = 0 to 31 do
    let v = Hart.get_f hart r in
    if v <> 0L then
      eprintf "%-4s = 0x%016Lx (%s)\n"
        (Freg.abi_name (Freg.of_int r))
        v
        (Fpu.string_of_double (Int64.float_of_bits v))
  done;
  if hart.fflags <> 0 then eprintf "fflags = 0x%02x\n" hart.fflags

(** Combine the optional per-instruction hooks into one. *)
let combine_hooks hooks =
  match List.filter_map Fun.id hooks with
  | [] -> None
  | [ h ] -> Some h
  | hs -> Some (fun pc i -> List.iter (fun h -> h pc i) hs)

let run path args rvc trace regs fuel strace signature stats profile =
  match load_image ~rvc path with
  | Error e ->
      prerr_endline e;
      1
  | Ok img -> (
      let os =
        Os.boot ~argv:(path :: args)
          ~env:(Array.to_list (Unix.environment ()))
          ~exe:(Filename.concat (Sys.getcwd ()) path)
          ?strace:(if strace then Some prerr_string else None)
          img
      in
      let hart = os.hart in
      let trace =
        if trace then
          Some
            (fun pc i ->
              Option.iter (Printf.eprintf "%s:\n") (Image.symbol_at img pc);
              Printf.eprintf "  %8Lx: %s\n%!" pc (Instr.to_string i))
        else None
      in
      let prof = if profile then Some (Stats.create_profile ()) else None in
      let hook = combine_hooks [ trace; Option.map Stats.record prof ] in
      let before = Stats.sample hart.instret in
      let outcome = Os.run ?fuel ?trace:hook os in
      let summary = Stats.diff before (Stats.sample hart.instret) in
      if stats then Format.eprintf "%a%!" Stats.pp_summary summary;
      Option.iter (Format.eprintf "%a%!" (Stats.pp_profile ?top:None)) prof;
      if regs then dump_regs hart;
      Option.iter
        (fun file ->
          match Os.signature hart img with
          | Some s ->
              Out_channel.with_open_bin file (fun oc -> output_string oc s)
          | None ->
              prerr_endline "riscvsim: no begin_signature/end_signature symbols")
        signature;
      match outcome with
      | Exited code -> code
      | Trapped { trap; pc } ->
          Format.eprintf "riscvsim: %a at pc 0x%Lx@." Trap.pp trap pc;
          if not regs then dump_regs hart;
          134
      | Out_of_fuel ->
          Printf.eprintf "riscvsim: instruction limit reached (%d executed)\n"
            hart.instret;
          124)

let asm path rvc output listing =
  match load_image ~rvc path with
  | Error e ->
      prerr_endline e;
      1
  | Ok img ->
      if listing then Format.printf "%a%!" Disasm.image img;
      let out =
        Option.value output ~default:(Filename.remove_extension path ^ ".elf")
      in
      Out_channel.with_open_bin out (fun oc -> output_string oc (Elf.write img));
      0

let disasm path rvc =
  match load_image ~rvc path with
  | Error e ->
      prerr_endline e;
      1
  | Ok img ->
      Format.printf "%a%!" Disasm.image img;
      0

let file =
  Arg.(
    required
    & pos 0 (some file) None
    & info [] ~docv:"FILE" ~doc:"Assembly source (.s) or RV64 ELF executable.")

let rvc =
  Arg.(
    value & flag
    & info [ "c"; "rvc" ]
        ~doc:"Compress eligible instructions (RVC) when assembling.")

let run_cmd =
  let trace =
    Arg.(
      value & flag
      & info [ "t"; "trace" ] ~doc:"Print every executed instruction to stderr.")
  in
  let regs =
    Arg.(
      value & flag
      & info [ "r"; "regs" ]
          ~doc:"Dump registers to stderr when the program stops.")
  in
  let fuel =
    Arg.(
      value
      & opt (some int) None
      & info [ "fuel" ] ~docv:"N" ~doc:"Stop after $(docv) instructions.")
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info [] ~docv:"ARGS" ~doc:"Arguments passed to the program.")
  in
  let strace =
    Arg.(value & flag & info [ "strace" ] ~doc:"Log every syscall to stderr.")
  in
  let signature =
    Arg.(
      value
      & opt (some string) None
      & info [ "signature" ] ~docv:"FILE"
          ~doc:
            "After the run, write the memory between $(b,begin_signature) and \
             $(b,end_signature) to $(docv) (riscv-arch-test format).")
  in
  let stats =
    Arg.(
      value & flag
      & info [ "stats" ]
          ~doc:
            "Print the instruction count, run time, speed (MIPS) and \
             allocation per instruction to stderr. Costs nothing.")
  in
  let profile =
    Arg.(
      value & flag
      & info [ "profile" ]
          ~doc:
            "Count executions per instruction and print the most executed \
             mnemonics and addresses to stderr. Slows the run down.")
  in
  Cmd.v
    (Cmd.info "run"
       ~doc:"Assemble (if needed) and run a program as a Linux process.")
    Term.(
      const run $ file $ args $ rvc $ trace $ regs $ fuel $ strace $ signature
      $ stats $ profile)

let asm_cmd =
  let output =
    Arg.(
      value
      & opt (some string) None
      & info [ "o" ] ~docv:"OUT" ~doc:"Output ELF path.")
  in
  let listing =
    Arg.(
      value & flag & info [ "l"; "list" ] ~doc:"Print a disassembly listing.")
  in
  Cmd.v
    (Cmd.info "asm" ~doc:"Assemble to an RV64 ELF executable.")
    Term.(const asm $ file $ rvc $ output $ listing)

let disasm_cmd =
  Cmd.v
    (Cmd.info "disasm" ~doc:"Disassemble a program's code.")
    Term.(const disasm $ file $ rvc)

let () =
  let info =
    Cmd.info "riscvsim" ~version:"0.1"
      ~doc:"An RV64GC simulator, assembler and disassembler."
  in
  exit (Cmd.eval' (Cmd.group info [ run_cmd; asm_cmd; disasm_cmd ]))
