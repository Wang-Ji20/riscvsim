(** A whole machine: harts sharing one {!Bus} and its devices, driven by a
    virtual clock (ROADMAP 1.1).

    Harts run in turn, [slice] instructions at a time; after each round the
    devices are ticked with the current time. Time is counted in executed
    instructions, so a run is deterministic and replays exactly. *)

type t = {
  bus : Bus.t;
  harts : Hart.t array;
  mutable now : int;  (** virtual time: instructions executed so far *)
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

(** Run one hart for up to [fuel] instructions, advancing the clock. *)
let run_hart ?trace t hart ~fuel =
  let before = hart.Hart.instret in
  let outcome = Hart.run ~fuel ?trace hart in
  t.now <- t.now + (hart.instret - before);
  outcome

(** Run until a hart exits or traps, [poll] reports an exit code, or [fuel]
    instructions (over all harts) have run. [poll] is checked and the devices
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
        let hart = t.harts.(i) in
        let before = hart.instret in
        match run_hart ?trace t hart ~fuel:budget with
        | Hart.Out_of_fuel -> round (remaining - (hart.instret - before)) (i + 1)
        | stopped -> stopped
  in
  round fuel 0
