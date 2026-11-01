(** HTIF ("host-target interface") exits, as Spike implements them: a bare-metal
    program stops by storing [(code lsl 1) lor 1] to the 64-bit word at its
    [tohost] symbol. Simulators poll the word between slices of instructions
    rather than on every store. *)

(** The exit code requested at [tohost], if any. *)
let exit_code mem ~tohost =
  let v = Memory.load mem tohost 8 in
  if Int64.logand v 1L = 1L then
    Some (Int64.to_int (Int64.shift_right_logical v 1) land 0xff)
  else None

(** The [tohost] address of a program image, if it has one. *)
let tohost (img : Image.t) = List.assoc_opt "tohost" img.symbols
