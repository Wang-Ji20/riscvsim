(* OCaml on riscvsim (ROADMAP 0.7), run with `dune build @ocaml`.

   Each guest/ocaml/*.ml is compiled to bytecode by the host's ocamlc, then
   run twice: by the host's ocamlrun, and by a riscv64 ocamlrun (built with
   guest/ocamlrun/build.sh) running on riscvsim. Output and exit status must
   match exactly. The riscv64 binary is taken from $OCAMLRUN_RISCV (default
   /tmp/ocamlrun.rv); if it's missing, the test skips. *)

open Riscv

let guest_dir = "../../guest/ocaml"

let ocamlrun_riscv =
  Option.value (Sys.getenv_opt "OCAMLRUN_RISCV") ~default:"/tmp/ocamlrun.rv"

let args = [ "a"; "b c" ]
let read_file path = In_channel.with_open_bin path In_channel.input_all

let host_run cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let out = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED c -> (out, c)
  | _ -> (out, -1)

let sim_run bytecode =
  match Elf.read (read_file ocamlrun_riscv) with
  | Error e -> (e, -1)
  | Ok { image; _ } -> (
      let buf = Buffer.create 4096 in
      let argv = "ocamlrun" :: bytecode :: args in
      let os = Os.boot ~io:(Os.buffer_io buf) ~argv ~exe:ocamlrun_riscv image in
      match Cpu.run ~fuel:5_000_000_000 os.cpu with
      | Cpu.Exited c -> (Buffer.contents buf, c)
      | Cpu.Trapped { trap; pc } ->
          ( Buffer.contents buf
            ^ Format.asprintf "<%a at 0x%Lx>" Cpu.pp_trap trap pc,
            -1 )
      | Cpu.Out_of_fuel -> (Buffer.contents buf ^ "<out of fuel>", -1))

let check program =
  let src = Filename.concat guest_dir (program ^ ".ml") in
  let bytecode =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "riscvsim-%s-%d.byte" program (Unix.getpid ()))
  in
  let quote = Filename.quote in
  let compiled, _ =
    host_run (Printf.sprintf "ocamlc -o %s %s" (quote bytecode) (quote src))
  in
  if compiled <> "" then failwith compiled;
  let expected =
    host_run
      (String.concat " " ("ocamlrun" :: List.map quote (bytecode :: args)))
  in
  let started = Unix.gettimeofday () in
  let actual = sim_run bytecode in
  Sys.remove bytecode;
  let ok = actual = expected in
  Printf.printf "  %-8s %s (%.1f s on riscvsim)\n%!" program
    (if ok then "identical" else "MISMATCH")
    (Unix.gettimeofday () -. started);
  if not ok then
    Printf.printf "expected (status %d):\n%s\ngot (status %d):\n%s\n"
      (snd expected) (fst expected) (snd actual) (fst actual);
  ok

let () =
  if not (Sys.file_exists ocamlrun_riscv) then
    Printf.printf
      "ocaml guest: %s not found (build it with guest/ocamlrun/build.sh), \
       skipping\n"
      ocamlrun_riscv
  else
    let programs =
      Sys.readdir guest_dir |> Array.to_list
      |> List.filter_map (fun f ->
          if Filename.check_suffix f ".ml" then
            Some (Filename.chop_suffix f ".ml")
          else None)
      |> List.sort compare
    in
    print_endline "ocaml guest: host ocamlrun vs riscv64 ocamlrun on riscvsim";
    if not (List.for_all check programs) then exit 1
