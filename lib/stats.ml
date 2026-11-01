(** Run statistics for performance work (docs/perf/method.md).

    A {!summary} costs nothing during the run: it reads the clock, the
    instruction counter and the GC counters before and after. A {!profile}
    counts executions per [pc] through the trace hook, so it slows the run down;
    it answers "what is the guest doing?", not "how fast are we?". *)

(** {1 Summary} *)

type sample = { time : float; instret : int64; words : float }

let sample instret =
  { time = Unix.gettimeofday (); instret; words = Gc.minor_words () }

type summary = {
  instructions : int64;
  seconds : float;
  alloc_words : float;  (** minor-heap words allocated *)
}

let diff before after =
  {
    instructions = Int64.sub after.instret before.instret;
    seconds = after.time -. before.time;
    alloc_words = after.words -. before.words;
  }

let mips s =
  if s.seconds <= 0. then 0.
  else Int64.to_float s.instructions /. s.seconds /. 1e6

let words_per_instr s =
  if s.instructions = 0L then 0.
  else s.alloc_words /. Int64.to_float s.instructions

let pp_summary ppf s =
  Format.fprintf ppf
    "instructions: %Ld@\n\
     time:         %.3f s@\n\
     speed:        %.2f MIPS@\n\
     allocation:   %.2f words/instruction@\n"
    s.instructions s.seconds (mips s) (words_per_instr s)

(** {1 Profile} *)

type entry = { instr : Instr.t; mutable count : int }
type profile = (int64, entry) Hashtbl.t

let create_profile () : profile = Hashtbl.create 4096

(** The trace hook: count one execution of [i] at [pc]. *)
let record (p : profile) pc i =
  match Hashtbl.find_opt p pc with
  | Some e -> e.count <- e.count + 1
  | None -> Hashtbl.add p pc { instr = i; count = 1 }

(** The mnemonic of an instruction: the first word of its disassembly. *)
let mnemonic i =
  let s = Instr.to_string i in
  match String.index_opt s ' ' with Some n -> String.sub s 0 n | None -> s

let sorted_desc l = List.sort (fun (_, a) (_, b) -> compare b a) l

(** Execution counts per mnemonic, most frequent first. *)
let by_mnemonic (p : profile) =
  let tbl = Hashtbl.create 64 in
  Hashtbl.iter
    (fun _ e ->
      let m = mnemonic e.instr in
      let n = Option.value ~default:0 (Hashtbl.find_opt tbl m) in
      Hashtbl.replace tbl m (n + e.count))
    p;
  sorted_desc (List.of_seq (Hashtbl.to_seq tbl))

(** The most executed instructions, as [(pc, entry)], most frequent first. *)
let hot_pcs (p : profile) =
  Hashtbl.to_seq p |> List.of_seq
  |> List.sort (fun (_, a) (_, b) -> compare b.count a.count)

let rec take n = function x :: tl when n > 0 -> x :: take (n - 1) tl | _ -> []

let pp_profile ?(top = 15) ppf (p : profile) =
  let total = Hashtbl.fold (fun _ e acc -> acc + e.count) p 0 in
  let pct n = 100. *. float n /. float (max total 1) in
  Format.fprintf ppf "distinct pcs: %d@\n@\nmost executed mnemonics:@\n"
    (Hashtbl.length p);
  List.iter
    (fun (m, n) -> Format.fprintf ppf "  %-12s %12d  %5.1f%%@\n" m n (pct n))
    (take top (by_mnemonic p));
  Format.fprintf ppf "@\nhottest instructions:@\n";
  List.iter
    (fun (pc, e) ->
      Format.fprintf ppf "  %10Lx %12d  %5.1f%%  %s@\n" pc e.count (pct e.count)
        (Instr.to_string e.instr))
    (take top (hot_pcs p))
