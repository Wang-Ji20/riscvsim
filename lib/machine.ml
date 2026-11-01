(** A whole machine: harts sharing one {!Bus} and its devices, driven by a
    virtual clock (ROADMAP 1.1).

    Harts run in turn, [slice] steps at a time; after each round the devices are
    ticked with the current time. Time is counted in steps (executed
    instructions and delivered traps), so a run is deterministic and replays
    exactly. *)

type t = {
  bus : Bus.t;
  harts : Hart.t array;
  mutable now : int;  (** virtual time: steps taken so far, by all harts *)
}

(** A machine with [harts] harts (default 1), numbered from 0, on [bus]. *)
let create ?(harts = 1) bus =
  if harts < 1 then invalid_arg "Machine.create: no harts";
  {
    bus;
    harts = Array.init harts (fun hartid -> Hart.create ~hartid ~bus ());
    now = 0;
  }

let hart t n = t.harts.(n)

(** Run one hart for up to [fuel] steps, advancing the clock. Returns the
    outcome and the steps taken. *)
let run_hart ?trace t hart ~fuel =
  let outcome, steps = Hart.run_steps ~fuel ?trace hart in
  t.now <- t.now + steps;
  (outcome, steps)

(** Run until a hart exits or traps, [poll] reports an exit code, or [fuel]
    steps (over all harts) have been taken. [poll] is checked and the devices
    are ticked after every round of slices. *)
let run ?(fuel = max_int) ?(slice = 10_000) ?(poll = fun () -> None) ?trace t =
  let n = Array.length t.harts in
  let rec round remaining i =
    if i = n then (
      Bus.tick t.bus ~now:t.now;
      match poll () with
      | Some code -> Hart.Exited code
      | None -> if remaining = 0 then Hart.Out_of_fuel else round remaining 0)
    else
      let budget = min slice remaining in
      if budget = 0 then round 0 (i + 1)
      else
        match run_hart ?trace t t.harts.(i) ~fuel:budget with
        | Hart.Out_of_fuel, steps -> round (remaining - steps) (i + 1)
        | stopped, _ -> stopped
  in
  round fuel 0
