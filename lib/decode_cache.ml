(** Decoded instructions, kept per memory page so each instruction is decoded
    once rather than on every execution (docs/perf/plan.md, O4).

    Each cached page has one slot per 2-byte parcel, since an instruction can
    start at any even address. {!Memory} calls {!invalidate} on the first write
    to a cached page, so self-modifying code and newly loaded code are always
    seen. That is stricter than RISC-V requires (stale instructions are allowed
    until [fence.i]), so [fence.i] needs no work. *)

type entry = { instr : Instr.t; len : int; bits : int }

type t = {
  pages : (int, entry option array) Hashtbl.t;
  mutable last_no : int;  (** the most recently used page, to skip hashing *)
  mutable last : entry option array;
}

let slots_per_page = Memory.page_size / 2
let create () = { pages = Hashtbl.create 64; last_no = -1; last = [||] }

let invalidate t n =
  Hashtbl.remove t.pages n;
  if t.last_no = n then (
    t.last_no <- -1;
    t.last <- [||])

(** The slots of page [n], creating them on first use. [None] when the page
    can't be cached because it was never written. *)
let page t mem n =
  if n = t.last_no then Some t.last
  else
    let found =
      match Hashtbl.find_opt t.pages n with
      | Some slots -> Some slots
      | None when Memory.mark_code mem n ->
          let slots = Array.make slots_per_page None in
          Hashtbl.replace t.pages n slots;
          Some slots
      | None -> None
    in
    Option.iter
      (fun slots ->
        t.last_no <- n;
        t.last <- slots)
      found;
    found

(** Decode the instruction at [pc] straight from memory. *)
let decode_at mem pc =
  let lo = Int64.to_int (Memory.load mem pc 2) in
  if Compressed.is_compressed lo then (Compressed.expand lo, 2, lo)
  else
    let w =
      lo lor (Int64.to_int (Memory.load mem (Int64.add pc 2L) 2) lsl 16)
    in
    (Decode.decode w, 4, w)

(** The instruction at [pc], or [Error bits] if it is illegal. An instruction
    that crosses into the next page is not cached, because a write to that page
    wouldn't invalidate it. *)
let fetch t mem pc =
  let n = Memory.page_no pc and off = Memory.offset pc in
  let slots = page t mem n in
  let slot = off lsr 1 in
  match slots with
  | Some s when Option.is_some s.(slot) -> Ok (Option.get s.(slot))
  | _ -> (
      match decode_at mem pc with
      | None, _, bits -> Error bits
      | Some instr, len, bits ->
          let e = { instr; len; bits } in
          (match slots with
          | Some s when off + len <= Memory.page_size -> s.(slot) <- Some e
          | _ -> ());
          Ok e)
