(** Memory-mapped devices (ROADMAP 1.2, Phase 4).

    A device implements {!S} over its own state. {!pack} turns it into a {!t},
    so the {!Bus} can hold devices of different types in one table. Offsets are
    relative to the device's base address. *)

module type S = sig
  type t

  val name : string

  val size : int64
  (** Bytes of address space the device occupies. *)

  val read : t -> offset:int64 -> size:int -> int64
  (** Read [size] (1, 2, 4 or 8) bytes, zero-extended. Raise [Trap.Trap] with a
      load access fault to reject the access. *)

  val write : t -> offset:int64 -> size:int -> int64 -> unit
  (** Write the low [size] bytes of the value. Raise [Trap.Trap] with a store
      access fault to reject the access. *)

  val tick : t -> now:int -> unit
  (** Advance the device to virtual time [now] (in instructions). *)

  val save : t -> string
  (** The device's state, for a {!Snapshot}. *)

  val restore : t -> string -> unit
  (** Restore what {!save} produced. *)
end

type t = {
  name : string;
  size : int64;
  read : offset:int64 -> size:int -> int64;
  write : offset:int64 -> size:int -> int64 -> unit;
  tick : now:int -> unit;
  save : unit -> string;
  restore : string -> unit;
}

let pack (type s) (module D : S with type t = s) (state : s) =
  {
    name = D.name;
    size = D.size;
    read = D.read state;
    write = D.write state;
    tick = D.tick state;
    save = (fun () -> D.save state);
    restore = D.restore state;
  }
