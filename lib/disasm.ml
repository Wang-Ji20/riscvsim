(** Disassembly of code bytes, handling the mix of 16- and 32-bit instructions.
*)

type line = { addr : int64; bits : int; len : int; instr : Instr.t option }

let decode_bytes ~vaddr (b : Bytes.t) : line list =
  let n = Bytes.length b in
  let rec go off acc =
    if off + 2 > n then List.rev acc
    else
      let lo = Bytes.get_uint16_le b off in
      let addr = Int64.add vaddr (Int64.of_int off) in
      if Compressed.is_compressed lo then
        go (off + 2)
          ({ addr; bits = lo; len = 2; instr = Compressed.expand lo } :: acc)
      else if off + 4 > n then List.rev acc
      else
        let w = lo lor (Bytes.get_uint16_le b (off + 2) lsl 16) in
        go (off + 4)
          ({ addr; bits = w; len = 4; instr = Decode.decode w } :: acc)
  in
  go 0 []

let pp_line ?(symbols = []) ppf { addr; bits; len; instr } =
  (match
     List.find_map
       (fun (n, a) -> if Int64.equal a addr then Some n else None)
       symbols
   with
  | Some name -> Format.fprintf ppf "%s:@\n" name
  | None -> ());
  let hex =
    if len = 2 then Printf.sprintf "    %04x" bits
    else Printf.sprintf "%08x" bits
  in
  Format.fprintf ppf "  %8Lx:  %s  %s" addr hex
    (match instr with Some i -> Instr.to_string i | None -> "<illegal>")

(** Disassemble the executable segments of an image. *)
let image ppf (img : Image.t) =
  List.iter
    (fun (s : Image.segment) ->
      if s.perm.x then
        List.iter
          (fun l -> Format.fprintf ppf "%a@\n" (pp_line ~symbols:img.symbols) l)
          (decode_bytes ~vaddr:s.vaddr s.data))
    img.segments
