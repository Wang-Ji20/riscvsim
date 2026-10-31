(** Integer registers [x0]..[x31]. *)

include Regfile_names.Make (struct
  let prefix = "x"

  let abi =
    [| "zero"; "ra"; "sp"; "gp"; "tp"; "t0"; "t1"; "t2";
       "s0"; "s1"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5";
       "a6"; "a7"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7";
       "s8"; "s9"; "s10"; "s11"; "t3"; "t4"; "t5"; "t6" |]
  [@@ocamlformat "disable"]

  let aliases = [ ("fp", 8) ]
end)

let zero = of_int 0
let ra = of_int 1
let sp = of_int 2
let gp = of_int 3
let a0 = of_int 10
let a1 = of_int 11
let a2 = of_int 12
let a7 = of_int 17
