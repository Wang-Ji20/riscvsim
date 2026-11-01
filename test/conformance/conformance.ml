(* Official RISC-V conformance suites, run with `dune build @conformance`.

   - riscv-tests (riscv-software-src): self-checking tests, built twice:
     against our user-mode environment (riscv-tests-env/riscv_test.h), run
     as a Linux process, and against the suite's own "p" environment, run
     on a system-mode machine (RAM at 0x80000000, traps delivered to the
     test's handlers) like Spike does. The privileged tests (rv64mi, rv64si)
     need the latter. A test passes when it exits with status 0, through
     exit(2) or HTIF.
   - riscv-arch-test 2.7.4 (riscv-non-isa): built against our model
     (arch-test-model/model_test.h) and run in system mode. A test passes
     when its signature region matches the reference signature shipped
     with the suite.

   The suites aren't vendored. Point RISCV_TESTS and RISCV_ARCH_TEST at
   checkouts, by default ../riscv-deps/{riscv-tests,arch-test-2.7.4} next
   to the project. Missing suites or a missing cross compiler skip. *)

open Riscv

let cc = "riscv64-suse-linux-gcc"

type expectation = Exit_zero | Signature of string  (** reference file *)

type mode = User_process | System_machine

type test = { name : string; source : string; flags : string list; mode : mode; expect : expectation }

(** Tests that can't pass for reasons outside the simulator's scope. *)
let skipped =
  [ ("jalr-01", "binutils >= 2.40 rejects the `la x0, ...` this 2018 test uses");
    ("rv64si-dirty", "needs Sv39 virtual memory (ROADMAP phase 3)");
    ("rv64si-icache-alias", "needs Sv39 virtual memory (ROADMAP phase 3)");
    ("rv64mi-illegal", "needs interrupts (PLAN 2g)") ]
[@@ocamlformat "disable"]

(* {1 Locating the suites} *)

let project_root =
  (* dune runs us in _build/default/test/conformance *)
  Filename.concat (Sys.getcwd ()) "../../../.."

let suite_dir var default =
  let dir = Option.value (Sys.getenv_opt var) ~default:(Filename.concat project_root default) in
  if Sys.file_exists dir then Some dir else None

let files_with_suffix suffix dir =
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f suffix)
    |> List.sort compare
    |> List.map (Filename.concat dir)

let basename_no_ext f = Filename.remove_extension (Filename.basename f)
let common_flags = [ "-static"; "-nostdlib"; "-mno-relax"; "-Wl,--no-relax" ]

(* {1 Suite definitions} *)

let user_suites = [ "rv64ui"; "rv64um"; "rv64ua"; "rv64uf"; "rv64ud"; "rv64uc"; "rv64uzba"; "rv64uzbb"; "rv64uzbs" ]

(** riscv-tests in [dirs], built for [mode]: our user-mode environment, or
    the suite's own bare-metal "p" environment and linker script. *)
let riscv_tests ~mode dirs root =
  let isa = Filename.concat root "isa" in
  let env_flags =
    match mode with
    | User_process -> [ "-I"; "riscv-tests-env" ]
    | System_machine ->
        let p = Filename.concat root "env/p" in
        [ "-I"; p; "-T"; Filename.concat p "link.ld" ]
  in
  dirs
  |> List.concat_map (fun dir ->
         files_with_suffix ".S" (Filename.concat isa dir)
         |> List.map (fun source ->
                {
                  name = dir ^ "-" ^ basename_no_ext source;
                  source;
                  flags =
                    [ "-march=rv64gc_zba_zbb_zbs"; "-mabi=lp64d"; "-I"; Filename.concat isa "macros/scalar" ]
                    @ env_flags @ common_flags;
                  mode;
                  expect = Exit_zero;
                }))

(** The -march for an arch-test file, from its RVTEST_ISA("RV64IFD") line. *)
let march_of source =
  let text = In_channel.with_open_bin source In_channel.input_all in
  let re = Str.regexp {|RVTEST_ISA("\([A-Za-z0-9_]+\)")|} in
  ignore (Str.search_forward re text 0);
  String.lowercase_ascii (Str.matched_group 1 text) ^ "_zicsr_zifencei"

let arch_tests root =
  let suite = Filename.concat root "riscv-test-suite" in
  [ "I"; "M"; "C"; "D"; "Zifencei" ]
  |> List.concat_map (fun ext ->
         let dir = Filename.concat suite ("rv64i_m/" ^ ext) in
         files_with_suffix ".S" (Filename.concat dir "src")
         |> List.map (fun source ->
                let name = basename_no_ext source in
                let march = march_of source in
                {
                  name;
                  source;
                  flags =
                    [ "-march=" ^ march; (if String.contains march 'd' then "-mabi=lp64d" else "-mabi=lp64");
                      "-DXLEN=64"; "-DFLEN=64"; "-DTEST_CASE_1=True";
                      "-I"; Filename.concat suite "env"; "-I"; "arch-test-model";
                      "-T"; "arch-test-model/link.ld" ]
                    @ common_flags;
                  mode = System_machine;
                  expect = Signature (Filename.concat dir ("references/" ^ name ^ ".reference_output"));
                }))
  [@@ocamlformat "disable"]

(* {1 Running} *)

type result = Pass | Fail of string | Skip of string

let scratch = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "riscvsim-conformance-%d" (Unix.getpid ()))

let compile t =
  let out = Filename.concat scratch t.name in
  let cmd = String.concat " " ((cc :: t.flags) @ [ Filename.quote t.source; "-o"; Filename.quote out; "2>/dev/null" ]) in
  if Sys.command cmd = 0 then Ok out else Error "does not build"

let fuel = 500_000_000

let run_process image =
  let os = Os.boot ~io:(Os.buffer_io (Buffer.create 64)) image in
  (os.hart, Os.run ~fuel os)

let simulate t elf =
  match Elf.read (In_channel.with_open_bin elf In_channel.input_all) with
  | Error e -> Error e
  | Ok { image; _ } ->
      let hart, outcome = match t.mode with User_process -> run_process image | System_machine -> Bare_metal.run ~fuel image in
      Ok (image, hart, outcome)

let check t elf =
  match simulate t elf with
  | Error e -> Fail e
  | Ok (image, hart, outcome) -> (
      match (t.expect, outcome) with
      | Exit_zero, Hart.Exited 0 -> Pass
      | Exit_zero, Hart.Exited n -> Fail (Printf.sprintf "test case %d failed" n)
      | Signature ref, Hart.Exited 0 -> (
          match Bare_metal.signature hart image with
          | Some s when s = In_channel.with_open_bin ref In_channel.input_all -> Pass
          | Some _ -> Fail "signature differs from the reference"
          | None -> Fail "no signature symbols")
      | _, Hart.Trapped { trap; pc } -> Fail (Format.asprintf "%a at 0x%Lx" Trap.pp trap pc)
      | _, (Hart.Exited _ | Hart.Out_of_fuel) -> Fail "did not exit cleanly")

let run_test t =
  match List.assoc_opt t.name skipped with
  | Some why -> Skip why
  | None -> ( match compile t with Ok elf -> check t elf | Error e -> Fail e)

let run_suite title tests =
  let results = List.map (fun t -> (t, run_test t)) tests in
  let count p = List.length (List.filter (fun (_, r) -> p r) results) in
  List.iter
    (fun (t, r) ->
      match r with
      | Fail why -> Printf.printf "  FAIL %s: %s\n" t.name why
      | Skip why -> Printf.printf "  skip %s: %s\n" t.name why
      | Pass -> ())
    results;
  Printf.printf "%s: %d passed, %d failed, %d skipped\n%!" title
    (count (( = ) Pass))
    (count (function Fail _ -> true | _ -> false))
    (count (function Skip _ -> true | _ -> false));
  count (function Fail _ -> true | _ -> false)

let () =
  if Sys.command (cc ^ " --version >/dev/null 2>&1") <> 0 then print_endline "conformance: no riscv64 cross compiler, skipping"
  else (
    Unix.mkdir scratch 0o755;
    let suites =
      [ ("riscv-tests (user mode)", "RISCV_TESTS", "../riscv-deps/riscv-tests", riscv_tests ~mode:User_process user_suites);
        ( "riscv-tests (system mode)", "RISCV_TESTS", "../riscv-deps/riscv-tests",
          riscv_tests ~mode:System_machine (user_suites @ [ "rv64mi"; "rv64si" ]) );
        ("riscv-arch-test 2.7.4", "RISCV_ARCH_TEST", "../riscv-deps/arch-test-2.7.4", arch_tests) ]
    in
    let failures =
      List.fold_left
        (fun acc (title, var, default, tests) ->
          match suite_dir var default with
          | Some dir -> acc + run_suite title (tests dir)
          | None -> Printf.printf "%s: not found (set %s), skipping\n" title var; acc)
        0 suites
    in
    ignore (Sys.command ("rm -rf " ^ Filename.quote scratch));
    if failures > 0 then exit 1)
