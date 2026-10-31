(** Sparse, little-endian, byte-addressed memory over the full 64-bit address
    space. Pages are allocated on first write, and unwritten memory reads as
    zero. Misaligned accesses are allowed, including ones that cross page
    boundaries. *)

let page_bits = 12
let page_size = 1 lsl page_bits
let page_mask = page_size - 1

type t = { pages : (int, Bytes.t) Hashtbl.t }

let create () = { pages = Hashtbl.create 64 }

(* Page numbers fit in an OCaml int: 64 - 12 = 52 bits. *)
let page_no addr = Int64.to_int (Int64.shift_right_logical addr page_bits)
let offset addr = Int64.to_int addr land page_mask
let find_page t addr = Hashtbl.find_opt t.pages (page_no addr)

let get_page t addr =
  let n = page_no addr in
  match Hashtbl.find_opt t.pages n with
  | Some p -> p
  | None ->
      let p = Bytes.make page_size '\000' in
      Hashtbl.replace t.pages n p;
      p

let load_u8 t addr =
  match find_page t addr with
  | Some p -> Bytes.get_uint8 p (offset addr)
  | None -> 0

let store_u8 t addr v =
  Bytes.set_uint8 (get_page t addr) (offset addr) (v land 0xff)

(** [load t addr size] reads [size] (1, 2, 4 or 8) bytes, zero-extended. *)
let load t addr size : int64 =
  let off = offset addr in
  if off + size <= page_size then
    match find_page t addr with
    | None -> 0L
    | Some p -> (
        match size with
        | 1 -> Int64.of_int (Bytes.get_uint8 p off)
        | 2 -> Int64.of_int (Bytes.get_uint16_le p off)
        | 4 ->
            Int64.logand
              (Int64.of_int32 (Bytes.get_int32_le p off))
              0xffff_ffffL
        | 8 -> Bytes.get_int64_le p off
        | _ -> invalid_arg "Memory.load")
  else
    (* crosses a page boundary: assemble byte by byte *)
    let rec go i acc =
      if i < 0 then acc
      else
        let b = load_u8 t (Int64.add addr (Int64.of_int i)) in
        go (i - 1) Int64.(logor (shift_left acc 8) (of_int b))
    in
    go (size - 1) 0L

(** [store t addr size v] writes the low [size] bytes of [v]. *)
let store t addr size (v : int64) =
  let off = offset addr in
  if off + size <= page_size then
    let p = get_page t addr in
    match size with
    | 1 -> Bytes.set_uint8 p off (Int64.to_int v land 0xff)
    | 2 -> Bytes.set_uint16_le p off (Int64.to_int v land 0xffff)
    | 4 -> Bytes.set_int32_le p off (Int64.to_int32 v)
    | 8 -> Bytes.set_int64_le p off v
    | _ -> invalid_arg "Memory.store"
  else
    for i = 0 to size - 1 do
      store_u8 t
        (Int64.add addr (Int64.of_int i))
        (Int64.to_int (Int64.shift_right_logical v (8 * i)))
    done

let blit_bytes t addr (b : Bytes.t) =
  Bytes.iteri
    (fun i c -> store_u8 t (Int64.add addr (Int64.of_int i)) (Char.code c))
    b

let read_bytes t addr len =
  Bytes.init len (fun i ->
      Char.chr (load_u8 t (Int64.add addr (Int64.of_int i))))

(** Read a NUL-terminated string, up to [max] bytes. *)
let read_cstring ?(max = 1 lsl 20) t addr =
  let buf = Buffer.create 32 in
  let rec go i =
    if i < max then
      let c = load_u8 t (Int64.add addr (Int64.of_int i)) in
      if c <> 0 then (
        Buffer.add_char buf (Char.chr c);
        go (i + 1))
  in
  go 0;
  Buffer.contents buf
