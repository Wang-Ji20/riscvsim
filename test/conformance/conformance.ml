(* Official RISC-V conformance suites, run with `dune build @conformance`.

   - riscv-tests (riscv-software-src): self-checking tests, built against
     our user-mode environment (riscv-tests-env/riscv_test.h). A test passes
     when it exits with status 0.
   - riscv-arch-test 2.7.4 (riscv-non-isa): built against our model
     (arch-test-model/model_test.h). A test passes when its signature
     region matches the reference signature shipped with the suite.

   The suites aren't vendored. Point RISCV_TESTS and RISCV_ARCH_TEST at
   checkouts, by default ../riscv-deps/{riscv-tests,arch-test-2.7.4} next
   to the project. Missing suites or a missing cross compiler skip. *)

open Riscv

let cc = "riscv64-suse-linux-gcc"

type expectation = Exit_zero | Signature of string  (** reference file *)

type test = { name : string; source : string; flags : string list; expect : expectation }

(** Tests that can't pass for reasons outside the simulator's scope. *)
let skipped =
  [ ("jalr-01", "binutils >= 2.40 rejects the `la x0, ...` this 2018 test uses");
    ("cebreak-01", "expects a machine-mode trap handler (ROADMAP phase 2)") ]
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

let riscv_tests root =
  let isa = Filename.concat root "isa" in
  [ "rv64ui"; "rv64um"; "rv64ua"; "rv64uf"; "rv64ud"; "rv64uc"; "rv64uzba"; "rv64uzbb"; "rv64uzbs" ]
  |> List.concat_map (fun dir ->
         files_with_suffix ".S" (Filename.concat isa dir)
         |> List.map (fun source ->
                {
                  name = dir ^ "-" ^ basename_no_ext source;
                  source;
                  flags =
                    [ "-march=rv64gc_zba_zbb_zbs"; "-mabi=lp64d"; "-I"; "riscv-tests-env"; "-I";
                      Filename.concat isa "macros/scalar" ]
                    @ common_flags;
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
                      "-Wl,-e,rvtest_init"; "-DXLEN=64"; "-DFLEN=64"; "-DTEST_CASE_1=True";
                      "-I"; Filename.concat suite "env"; "-I"; "arch-test-model" ]
                    @ common_flags;
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

let simulate elf =
  match Elf.read (In_channel.with_open_bin elf In_channel.input_all) with
  | Error e -> Error e
  | Ok { image; _ } ->
      let os = Os.boot ~io:(Os.buffer_io (Buffer.create 64)) image in
      Ok (image, os.cpu, Cpu.run ~fuel:500_000_000 os.cpu)

let check t elf =
  match simulate elf with
  | Error e -> Fail e
  | Ok (image, cpu, outcome) -> (
      match (t.expect, outcome) with
      | Exit_zero, Cpu.Exited 0 -> Pass
      | Exit_zero, Cpu.Exited n -> Fail (Printf.sprintf "test case %d failed" n)
      | Signature ref, Cpu.Exited 0 -> (
          match Os.signature cpu image with
          | Some s when s = In_channel.with_open_bin ref In_channel.input_all -> Pass
          | Some _ -> Fail "signature differs from the reference"
          | None -> Fail "no signature symbols")
      | _, Cpu.Trapped { trap; pc } -> Fail (Format.asprintf "%a at 0x%Lx" Trap.pp trap pc)
      | _, (Cpu.Exited _ | Cpu.Out_of_fuel) -> Fail "did not exit cleanly")

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
      [ ("riscv-tests", "RISCV_TESTS", "../riscv-deps/riscv-tests", riscv_tests);
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
