(** Decoded instructions, kept per memory page so each instruction is decoded
    once rather than on every execution (docs/perf/plan.md, O4).

    Each cached page has one slot per 2-byte parcel, since an instruction can
    start at any even address. {!Memory} calls {!invalidate} on the first write
    to a cached page, so self-modifying code and newly loaded code are always
    seen. That is stricter than RISC-V requires (stale instructions are allowed
    until [fence.i]), so [fence.i] needs no work. *)

type entry = { instr : Instr.t; len : int; bits : int }

exception Illegal of int
(** the raw bits of an illegal instruction *)

(* A page that can't be cached has [no_slots], compared physically. An
   [option] here would allocate on every fetch. *)
let no_slots : entry option array = [||]

type t = {
  pages : (int, entry option array) Hashtbl.t;
  mutable last_no : int;  (** the most recently used page, to skip hashing *)
  mutable last : entry option array;
}

let slots_per_page = Memory.page_size / 2
let create () = { pages = Hashtbl.create 64; last_no = -1; last = no_slots }

let invalidate t n =
  Hashtbl.remove t.pages n;
  if t.last_no = n then (
    t.last_no <- -1;
    t.last <- no_slots)

(** The slots of page [n], creating them on first use. A page that was never
    written reads as zero and isn't worth caching: it gets [no_slots]. *)
let page_slow t mem n =
  let slots =
    match Hashtbl.find_opt t.pages n with
    | Some slots -> slots
    | None when Memory.mark_code mem n ->
        let slots = Array.make slots_per_page None in
        Hashtbl.replace t.pages n slots;
        slots
    | None -> no_slots
  in
  t.last_no <- n;
  t.last <- slots;
  slots

let[@inline] page t mem n = if n = t.last_no then t.last else page_slow t mem n

(** Decode the instruction at [pc] straight from memory. *)
let decode_at mem pc =
  let lo = Int64.to_int (Memory.load mem pc 2) in
  if Compressed.is_compressed lo then (Compressed.expand lo, 2, lo)
  else
    let w =
      lo lor (Int64.to_int (Memory.load mem (Int64.add pc 2L) 2) lsl 16)
    in
    (Decode.decode w, 4, w)

(** Decode, and cache unless the instruction crosses into the next page: a write
    to that page wouldn't invalidate it. *)
let fill mem pc slots ~off =
  match decode_at mem pc with
  | None, _, bits -> raise (Illegal bits)
  | Some instr, len, bits ->
      let e = { instr; len; bits } in
      if slots != no_slots && off + len <= Memory.page_size then
        slots.(off lsr 1) <- Some e;
      e

(** The instruction at byte [off] of page [page]. Raises [Illegal] if it is
    illegal. The address comes as two ints rather than an int64, which a call
    would box; a hit doesn't allocate at all. *)
let fetch t mem ~page:n ~off =
  let slots = page t mem n in
  let miss () =
    let pc =
      Int64.(logor (shift_left (of_int n) Memory.page_bits) (of_int off))
    in
    fill mem pc slots ~off
  in
  if slots == no_slots then miss ()
  else match slots.(off lsr 1) with Some e -> e | None -> miss ()
