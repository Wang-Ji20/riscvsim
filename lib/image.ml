(** A loadable program: what the assembler produces, what ELF files are read
    into and written from, and what the CPU loads. *)

type perm = { r : bool; w : bool; x : bool }

type segment = {
  name : string;
  vaddr : int64;
  data : Bytes.t;  (** initialised contents *)
  mem_size : int;  (** >= [Bytes.length data]; the rest is zero-filled *)
  perm : perm;
}

type phdrs = { addr : int64; num : int; entsize : int }
(** Where the ELF program headers are mapped, for the [AT_PHDR] auxv entry that
    libc startup code reads. *)

type t = {
  entry : int64;
  segments : segment list;
  symbols : (string * int64) list;
  phdrs : phdrs option;
}

(** Highest address used by any segment: the initial program break. *)
let end_address t =
  List.fold_left
    (fun acc s ->
      let e = Int64.add s.vaddr (Int64.of_int s.mem_size) in
      if Int64.unsigned_compare e acc > 0 then e else acc)
    0L t.segments

(** Symbol at exactly [addr], if any (used for traces). *)
let symbol_at t addr =
  List.find_map
    (fun (name, a) -> if Int64.equal a addr then Some name else None)
    t.symbols
