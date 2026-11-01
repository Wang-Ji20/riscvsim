(** Privilege levels, from least to most privileged (ROADMAP 2.1). Their numbers
    are what [mstatus.MPP]/[SPP] and CSR addresses encode. *)

type t = U | S | M

let to_int = function U -> 0 | S -> 1 | M -> 3
let of_int = function 0 -> Some U | 1 -> Some S | 3 -> Some M | _ -> None
let to_string = function U -> "U" | S -> "S" | M -> "M"
let ( >= ) a b = Int.compare (to_int a) (to_int b) >= 0
