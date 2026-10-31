(** Floating-point registers [f0]..[f31]. *)

include Regfile_names.Make (struct
  let prefix = "f"

  let abi =
    [| "ft0"; "ft1"; "ft2"; "ft3"; "ft4"; "ft5"; "ft6"; "ft7";
       "fs0"; "fs1"; "fa0"; "fa1"; "fa2"; "fa3"; "fa4"; "fa5";
       "fa6"; "fa7"; "fs2"; "fs3"; "fs4"; "fs5"; "fs6"; "fs7";
       "fs8"; "fs9"; "fs10"; "fs11"; "ft8"; "ft9"; "ft10"; "ft11" |]
  [@@ocamlformat "disable"]

  let aliases = []
end)
