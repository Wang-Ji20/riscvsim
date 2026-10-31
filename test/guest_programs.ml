(* Three-way differential test of real C programs (guest/tests/*.c).

   Each program is built three ways:
     rvlibc  riscv64, linked against our own freestanding libc (guest/libc)
     glibc   riscv64, statically linked against the distribution's glibc
     host    native x86-64, the oracle
   The two riscv64 builds run on riscvsim and must print exactly what the
   host build prints and exit with the same status. This checks the
   simulator's ISA and Linux ABI emulation and our libc together.
   Skipped when the cross compiler is missing. *)

open Riscv

let cross_cc = "riscv64-suse-linux-gcc"
let guest_dir = "../guest"
let args = [ "one"; "two words" ]
let env = [ "RISCVSIM_TEST=yes" ]
let command_ok cmd = Sys.command (cmd ^ " >/dev/null 2>&1") = 0
let quote = Filename.quote

let scratch =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "riscvsim-guest-%d" (Unix.getpid ()))

let read_file path = In_channel.with_open_bin path In_channel.input_all

let compile ~cc ~flags ~sources ~out =
  let cmd =
    String.concat " "
      ((cc :: flags) @ List.map quote sources @ [ "-o"; quote out ])
  in
  if not (command_ok cmd) then failwith ("compile failed: " ^ cmd)

let rvlibc_flags () =
  let gcc_include = String.trim (In_channel.input_all (Unix.open_process_in (cross_cc ^ " -print-file-name=include"))) in
  [ "-O2"; "-static"; "-nostdlib"; "-nostdinc"; "-ffreestanding"; "-fno-tree-loop-distribute-patterns";
    "-isystem"; quote gcc_include; "-isystem"; quote (Filename.concat guest_dir "libc/include") ]
  [@@ocamlformat "disable"]

let libc_sources =
  List.map
    (Filename.concat (Filename.concat guest_dir "libc"))
    [ "crt0.S"; "core.c"; "stdio.c" ]

(** Run a host binary in [scratch], capturing stdout+stderr and the status. *)
let run_host exe =
  let cmd =
    Printf.sprintf "cd %s && env %s %s %s 2>&1" (quote scratch)
      (String.concat " " env) (quote exe)
      (String.concat " " (List.map quote args))
  in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED c -> (out, c)
  | _ -> (out, -1)

(** Run a riscv64 ELF on riscvsim, in [scratch], with the same argv/env. *)
let run_sim exe =
  match Elf.read (read_file exe) with
  | Error e -> (e, -1)
  | Ok { image; _ } -> (
      let buf = Buffer.create 4096 in
      let cwd = Sys.getcwd () in
      Sys.chdir scratch;
      let os = Os.boot ~io:(Os.buffer_io buf) ~argv:(exe :: args) ~env image in
      let outcome = Cpu.run ~fuel:2_000_000_000 os.cpu in
      Sys.chdir cwd;
      match outcome with
      | Cpu.Exited c -> (Buffer.contents buf, c)
      | Cpu.Trapped { trap; pc } ->
          ( Buffer.contents buf
            ^ Format.asprintf "\n<trap: %a at 0x%Lx>" Cpu.pp_trap trap pc,
            -1 )
      | Cpu.Out_of_fuel -> (Buffer.contents buf ^ "\n<out of fuel>", -1))

let test_program name =
  let src = Filename.concat guest_dir ("tests/" ^ name ^ ".c") in
  let exe kind = Filename.concat scratch (name ^ "." ^ kind) in
  compile ~cc:cross_cc ~flags:(rvlibc_flags ())
    ~sources:(libc_sources @ [ src; "-lgcc" ])
    ~out:(exe "rvlibc");
  compile ~cc:cross_cc ~flags:[ "-O2"; "-static" ] ~sources:[ src ]
    ~out:(exe "glibc");
  compile ~cc:"cc" ~flags:[ "-O2" ] ~sources:[ src ] ~out:(exe "host");
  let expected = run_host (exe "host") in
  List.filter_map
    (fun kind ->
      let actual = run_sim (exe kind) in
      if actual = expected then None
      else
        Some
          (Printf.sprintf
             "%s [%s]: expected (status %d)\n%s\ngot (status %d)\n%s" name kind
             (snd expected) (fst expected) (snd actual) (fst actual)))
    [ "rvlibc"; "glibc" ]

let () =
  if not (command_ok (cross_cc ^ " --version") && command_ok "cc --version")
  then
    print_endline "guest programs: riscv64 cross compiler not found, skipping"
  else (
    Unix.mkdir scratch 0o755;
    let programs =
      Sys.readdir (Filename.concat guest_dir "tests")
      |> Array.to_list
      |> List.filter_map (fun f ->
          if Filename.check_suffix f ".c" then
            Some (Filename.chop_suffix f ".c")
          else None)
      |> List.sort compare
    in
    let failures = List.concat_map test_program programs in
    ignore (Sys.command ("rm -rf " ^ quote scratch));
    List.iter print_endline failures;
    Printf.printf
      "guest programs: %d programs x (rvlibc, glibc) vs host, %d mismatches\n"
      (List.length programs) (List.length failures);
    if failures <> [] then exit 1)
