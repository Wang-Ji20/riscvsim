(* Benchmark harness (docs/perf/method.md).

   Runs every benchmark once to warm up, then [--runs] times, each run in a
   forked child process so that peak memory is per run. Checks each run's
   output against the benchmark's .expected file, then prints a Markdown
   table of medians and, with [--csv], appends one row per benchmark. *)

open Riscv

type bench = {
  name : string;
  load : unit -> (Image.t, string) result;
  expected : string;
}

type run = { summary : Stats.summary; peak_kb : int; output : string }

let read_file path = In_channel.with_open_bin path In_channel.input_all

(** {1 The benchmark set} *)

let assemble ~rvc path () =
  Assembler.assemble ~rvc (read_file path)
  |> Result.map_error (fun errs ->
      Printf.sprintf "%s: %d assembler errors" path (List.length errs))

(** Each micro benchmark in [dir], plain and, for the listed ones, with
    compressed (RVC) instructions too. *)
let micro_benchmarks dir =
  let rvc_variants = [ "int_loop"; "calls" ] in
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".s")
  |> List.sort compare
  |> List.concat_map (fun file ->
      let base = Filename.chop_suffix file ".s" in
      let path = Filename.concat dir file in
      let expected = read_file (Filename.concat dir (base ^ ".expected")) in
      let make ~rvc name = { name; load = assemble ~rvc path; expected } in
      make ~rvc:false base
      ::
      (if List.mem base rvc_variants then [ make ~rvc:true (base ^ "/rvc") ]
       else []))

(** {1 Running} *)

(** Peak resident set size of this process, in KiB (Linux only). *)
let peak_kb () =
  try
    In_channel.with_open_text "/proc/self/status" (fun ic ->
        let rec find () =
          match In_channel.input_line ic with
          | None -> 0
          | Some l -> (
              match Scanf.sscanf_opt l "VmHWM: %d kB" Fun.id with
              | Some kb -> kb
              | None -> find ())
        in
        find ())
  with Sys_error _ -> 0

let run_here (img : Image.t) =
  let buf = Buffer.create 256 in
  let os = Os.boot ~io:(Os.buffer_io buf) ~argv:[ "bench" ] img in
  let before = Stats.sample os.cpu.instret in
  let outcome = Os.run os in
  let summary = Stats.diff before (Stats.sample os.cpu.instret) in
  let output =
    match outcome with
    | Cpu.Exited _ -> Buffer.contents buf
    | Cpu.Trapped { trap; pc } ->
        Format.asprintf "%s<trap: %a at 0x%Lx>" (Buffer.contents buf)
          Cpu.pp_trap trap pc
    | Cpu.Out_of_fuel -> Buffer.contents buf ^ "<out of fuel>"
  in
  { summary; peak_kb = peak_kb (); output }

(** Run in a forked child, so each run starts with a fresh heap and its own peak
    memory figure. The child sends its result back through a pipe. *)
let run_isolated img =
  let rd, wr = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
      Unix.close rd;
      let oc = Unix.out_channel_of_descr wr in
      Marshal.to_channel oc (run_here img : run) [];
      close_out oc;
      Unix._exit 0
  | pid ->
      Unix.close wr;
      let ic = Unix.in_channel_of_descr rd in
      let (r : run) = Marshal.from_channel ic in
      close_in ic;
      ignore (Unix.waitpid [] pid);
      r

(** {1 Results} *)

type result = {
  bench : string;
  instructions : int64;
  median : float;
  min : float;
  max : float;
  words_per_instr : float;
  peak_kb : int;
}

let median l =
  let a = Array.of_list (List.sort compare l) in
  let n = Array.length a in
  if n mod 2 = 1 then a.(n / 2) else (a.((n / 2) - 1) +. a.(n / 2)) /. 2.

let mips r = Int64.to_float r.instructions /. r.median /. 1e6

let measure ~runs b =
  match b.load () with
  | Error e -> Error e
  | Ok img ->
      let check (r : run) =
        if r.output <> b.expected then
          failwith
            (Printf.sprintf "%s: wrong output %S, expected %S" b.name r.output
               b.expected)
      in
      check (run_isolated img) (* warm-up *);
      let rs = List.init runs (fun _ -> run_isolated img) in
      List.iter check rs;
      let times = List.map (fun r -> r.summary.Stats.seconds) rs in
      let first = List.hd rs in
      Ok
        {
          bench = b.name;
          instructions = first.summary.instructions;
          median = median times;
          min = List.fold_left Float.min infinity times;
          max = List.fold_left Float.max 0. times;
          words_per_instr = Stats.words_per_instr first.summary;
          peak_kb = List.fold_left (fun m (r : run) -> max m r.peak_kb) 0 rs;
        }

let geomean l =
  exp (List.fold_left (fun acc x -> acc +. log x) 0. l /. float (List.length l))

let print_table results =
  print_endline
    "| benchmark | instructions | median s | min–max s | MIPS | words/instr | \
     peak MiB |";
  print_endline "|---|---:|---:|---:|---:|---:|---:|";
  List.iter
    (fun r ->
      Printf.printf "| %s | %Ld | %.3f | %.3f–%.3f | %.2f | %.1f | %.1f |\n"
        r.bench r.instructions r.median r.min r.max (mips r) r.words_per_instr
        (float r.peak_kb /. 1024.))
    results;
  Printf.printf "| **geometric mean** | | | | **%.2f** | | |\n%!"
    (geomean (List.map mips results))

(** {1 Metadata and CSV} *)

let command_line cmd =
  try
    let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
    let out = In_channel.input_all ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Some (String.trim out)
    | _ -> None
  with Unix.Unix_error _ -> None

let commit () =
  match command_line "git rev-parse --short HEAD" with
  | None -> "unknown"
  | Some c -> (
      match command_line "git status --porcelain --untracked-files=no" with
      | Some "" | None -> c
      | Some _ -> c ^ "-dirty")

let cpu_model () =
  match command_line "grep -m1 'model name' /proc/cpuinfo" with
  | Some l -> (
      match String.index_opt l ':' with
      | Some i -> String.trim (String.sub l (i + 1) (String.length l - i - 1))
      | None -> l)
  | None -> "unknown"

let csv_header =
  "date,commit,benchmark,instructions,median_s,min_s,max_s,mips,words_per_instr,peak_kb,cpu,ocaml"

let append_csv path results =
  let fresh = not (Sys.file_exists path) in
  let date =
    let t = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
  in
  let commit = commit () and cpu = cpu_model () in
  Out_channel.with_open_gen [ Open_append; Open_creat; Open_text ] 0o644 path
    (fun oc ->
      if fresh then output_string oc (csv_header ^ "\n");
      List.iter
        (fun r ->
          Printf.fprintf oc
            "%s,%s,%s,%Ld,%.4f,%.4f,%.4f,%.3f,%.2f,%d,\"%s\",%s\n" date commit
            r.bench r.instructions r.median r.min r.max (mips r)
            r.words_per_instr r.peak_kb cpu Sys.ocaml_version)
        results)

(** {1 Command line} *)

let () =
  let runs = ref 5 and csv = ref None and filter = ref "" in
  let dir = ref (if Sys.file_exists "micro" then "micro" else "bench/micro") in
  Arg.parse
    [
      ("--runs", Arg.Set_int runs, "N  measured runs per benchmark (default 5)");
      ( "--csv",
        Arg.String (fun f -> csv := Some f),
        "FILE  append results to FILE" );
      ( "--filter",
        Arg.Set_string filter,
        "S  only benchmarks whose name contains S" );
      ("--dir", Arg.Set_string dir, "DIR  directory of micro benchmarks");
    ]
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    "run.exe [--runs N] [--csv FILE] [--filter S]";
  let contains s sub =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
    in
    go 0
  in
  let benches =
    List.filter (fun b -> contains b.name !filter) (micro_benchmarks !dir)
  in
  let results =
    List.filter_map
      (fun b ->
        Printf.eprintf "running %s...\n%!" b.name;
        match measure ~runs:!runs b with
        | Ok r -> Some r
        | Error e ->
            Printf.eprintf "skipped %s: %s\n%!" b.name e;
            None)
      benches
  in
  print_table results;
  Option.iter (fun path -> append_csv path results) !csv
