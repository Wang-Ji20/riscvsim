(** A register namespace: 32 registers with ABI names plus numeric names
    [<prefix>0] .. [<prefix>31]. Instantiated for integer and FP registers. *)

module type S = sig
  type t = private int

  val of_int : int -> t
  (** Raises [Invalid_argument] unless [0 <= n < 32]. *)

  val to_int : t -> int
  val of_string : string -> t option
  val abi_name : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val all : t list
end

module Make (N : sig
  val prefix : string
  val abi : string array
  val aliases : (string * int) list
end) : S = struct
  type t = int

  let of_int n =
    if n < 0 || n > 31 then invalid_arg (Printf.sprintf "register %d" n);
    n

  let to_int r = r
  let equal = Int.equal
  let abi_name r = N.abi.(r)
  let pp ppf r = Format.pp_print_string ppf (abi_name r)
  let all = List.init 32 Fun.id

  let table =
    let tbl = Hashtbl.create 80 in
    Array.iteri (fun i name -> Hashtbl.replace tbl name i) N.abi;
    List.iter (fun i -> Hashtbl.replace tbl (N.prefix ^ string_of_int i) i) all;
    List.iter (fun (name, i) -> Hashtbl.replace tbl name i) N.aliases;
    tbl

  let of_string s = Hashtbl.find_opt table (String.lowercase_ascii s)
end
