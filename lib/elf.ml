(** Minimal ELF64 (little-endian, EM_RISCV) support.

    - [write]: an {!Image.t} as an executable, with PT_LOAD segments, section
      headers and a symbol table.
    - [read]: load segments (executables), or sections (relocatable objects),
      plus symbols. *)

let em_riscv = 243
let pt_load = 1l
let ef_rvc = 0x1
let ef_float_double = 0x4

(* {1 Writing} *)

let write (img : Image.t) : string =
  let segs = img.segments in
  let nseg = List.length segs in
  let ehsize = 64 and phentsize = 56 and shentsize = 64 in
  let buf = Buffer.create 4096 in
  let u8 n = Buffer.add_uint8 buf n
  and u16 n = Buffer.add_uint16_le buf n
  and u32 n = Buffer.add_int32_le buf (Int32.of_int n)
  and u64 n = Buffer.add_int64_le buf n in
  let pad_to off =
    while Buffer.length buf < off do
      u8 0
    done
  in
  (* string tables *)
  let strtab = Buffer.create 256 in
  Buffer.add_char strtab '\000';
  let add_str tbl s =
    let off = Buffer.length tbl in
    Buffer.add_string tbl s;
    Buffer.add_char tbl '\000';
    off
  in
  let shstrtab = Buffer.create 64 in
  Buffer.add_char shstrtab '\000';
  let seg_names =
    List.map (fun (s : Image.segment) -> add_str shstrtab s.name) segs
  in
  let symtab_name = add_str shstrtab ".symtab" in
  let strtab_name = add_str shstrtab ".strtab" in
  let shstrtab_name = add_str shstrtab ".shstrtab" in
  (* layout: headers, then each segment's data at a page-congruent offset *)
  let headers_end = ehsize + (nseg * phentsize) in
  let offsets, data_end =
    List.fold_left
      (fun (acc, off) (s : Image.segment) ->
        let page = 4096 in
        let want = Int64.to_int (Int64.rem s.vaddr (Int64.of_int page)) in
        let off = off + ((want - (off mod page) + page) mod page) in
        (off :: acc, off + Bytes.length s.data))
      ([], headers_end) segs
  in
  let offsets = List.rev offsets in
  (* symbol table (entry 0 is the null symbol) *)
  let section_index addr =
    let rec go k = function
      | [] -> 0xfff1 (* SHN_ABS *)
      | (s : Image.segment) :: rest ->
          let e = Int64.add s.vaddr (Int64.of_int s.mem_size) in
          if
            Int64.unsigned_compare addr s.vaddr >= 0
            && Int64.unsigned_compare addr e < 0
          then k
          else go (k + 1) rest
    in
    go 1 segs
  in
  let syms = Buffer.create 256 in
  Buffer.add_string syms (String.make 24 '\000');
  List.iter
    (fun (name, addr) ->
      Buffer.add_int32_le syms (Int32.of_int (add_str strtab name));
      Buffer.add_uint8 syms 0x10 (* STB_GLOBAL, STT_NOTYPE *);
      Buffer.add_uint8 syms 0;
      Buffer.add_uint16_le syms (section_index addr);
      Buffer.add_int64_le syms addr;
      Buffer.add_int64_le syms 0L)
    img.symbols;
  let symtab_off = (data_end + 7) land lnot 7 in
  let strtab_off = symtab_off + Buffer.length syms in
  let shstrtab_off = strtab_off + Buffer.length strtab in
  let shoff = (shstrtab_off + Buffer.length shstrtab + 7) land lnot 7 in
  let shnum = nseg + 4 in
  (* ELF header *)
  Buffer.add_string buf "\x7fELF";
  u8 2;
  u8 1;
  u8 1;
  u8 0;
  pad_to 16;
  u16 2 (* ET_EXEC *);
  u16 em_riscv;
  u32 1;
  u64 img.entry;
  u64 (Int64.of_int ehsize);
  u64 (Int64.of_int shoff);
  u32 (ef_rvc lor ef_float_double);
  u16 ehsize;
  u16 phentsize;
  u16 nseg;
  u16 shentsize;
  u16 shnum;
  u16 (shnum - 1);
  (* program headers *)
  List.iter2
    (fun (s : Image.segment) off ->
      Buffer.add_int32_le buf pt_load;
      u32
        ((if s.perm.x then 1 else 0)
        lor (if s.perm.w then 2 else 0)
        lor if s.perm.r then 4 else 0);
      u64 (Int64.of_int off);
      u64 s.vaddr;
      u64 s.vaddr;
      u64 (Int64.of_int (Bytes.length s.data));
      u64 (Int64.of_int s.mem_size);
      u64 4096L)
    segs offsets;
  List.iter2
    (fun (s : Image.segment) off ->
      pad_to off;
      Buffer.add_bytes buf s.data)
    segs offsets;
  pad_to symtab_off;
  Buffer.add_buffer buf syms;
  Buffer.add_buffer buf strtab;
  Buffer.add_buffer buf shstrtab;
  pad_to shoff;
  (* section headers *)
  let shdr ~name ~typ ~flags ~addr ~off ~size ~link ~info ~align ~entsize =
    u32 name;
    u32 typ;
    u64 flags;
    u64 addr;
    u64 (Int64.of_int off);
    u64 (Int64.of_int size);
    u32 link;
    u32 info;
    u64 (Int64.of_int align);
    u64 (Int64.of_int entsize)
  in
  shdr ~name:0 ~typ:0 ~flags:0L ~addr:0L ~off:0 ~size:0 ~link:0 ~info:0 ~align:0
    ~entsize:0;
  List.iteri
    (fun k (s : Image.segment) ->
      let nobits = Bytes.length s.data = 0 && s.mem_size > 0 in
      let flags =
        Int64.of_int
          (2
          (* ALLOC *) lor (if s.perm.w then 1 else 0)
          lor if s.perm.x then 4 else 0)
      in
      shdr ~name:(List.nth seg_names k)
        ~typ:(if nobits then 8 else 1)
        ~flags ~addr:s.vaddr ~off:(List.nth offsets k) ~size:s.mem_size ~link:0
        ~info:0 ~align:8 ~entsize:0)
    segs;
  shdr ~name:symtab_name ~typ:2 ~flags:0L ~addr:0L ~off:symtab_off
    ~size:(Buffer.length syms) ~link:(nseg + 2) ~info:1 ~align:8 ~entsize:24;
  shdr ~name:strtab_name ~typ:3 ~flags:0L ~addr:0L ~off:strtab_off
    ~size:(Buffer.length strtab) ~link:0 ~info:0 ~align:1 ~entsize:0;
  shdr ~name:shstrtab_name ~typ:3 ~flags:0L ~addr:0L ~off:shstrtab_off
    ~size:(Buffer.length shstrtab) ~link:0 ~info:0 ~align:1 ~entsize:0;
  Buffer.contents buf

(* {1 Reading} *)

type section = { sname : string; stype : int; saddr : int64; sdata : string }

type file = {
  etype : int;  (** 1 = relocatable, 2 = executable, 3 = shared/PIE *)
  image : Image.t;  (** loadable segments (empty for objects) *)
  sections : section list;
}

let read (s : string) : (file, string) result =
  let ( let* ) = Result.bind in
  let len = String.length s in
  let check c msg = if c then Ok () else Error msg in
  let* () =
    check (len >= 64 && String.sub s 0 4 = "\x7fELF") "not an ELF file"
  in
  let* () =
    check (s.[4] = '\002' && s.[5] = '\001') "not a little-endian ELF64 file"
  in
  let u16 o = String.get_uint16_le s o
  and u32 o = Int32.to_int (String.get_int32_le s o) land 0xffffffff
  and u64 o = String.get_int64_le s o in
  let off64 o = Int64.to_int (u64 o) in
  let* () = check (u16 18 = em_riscv) "not a RISC-V ELF file" in
  let etype = u16 16 in
  let entry = u64 24 and phoff = off64 32 and shoff = off64 40 in
  let phentsize = u16 54
  and phnum = u16 56
  and shentsize = u16 58
  and shnum = u16 60 in
  let shstrndx = u16 62 in
  let in_bounds off size = off >= 0 && size >= 0 && off + size <= len in
  let* raw_sections =
    if shoff = 0 then Ok []
    else
      let* () =
        check (in_bounds shoff (shnum * shentsize)) "truncated section headers"
      in
      Ok
        (List.init shnum (fun k ->
             let b = shoff + (k * shentsize) in
             ( u32 b,
               u32 (b + 4),
               u64 (b + 16),
               off64 (b + 24),
               off64 (b + 32),
               u32 (b + 40) )))
  in
  let name_at tbl_off o =
    match String.index_from_opt s (tbl_off + o) '\000' with
    | Some e -> String.sub s (tbl_off + o) (e - tbl_off - o)
    | None -> ""
  in
  let shstr_off =
    match List.nth_opt raw_sections shstrndx with
    | Some (_, _, _, off, _, _) -> off
    | None -> 0
  in
  let sections =
    List.map
      (fun (name, typ, addr, off, size, _) ->
        let data =
          if typ = 8 || not (in_bounds off size) then ""
          else String.sub s off size
        in
        {
          sname = (if shstr_off > 0 then name_at shstr_off name else "");
          stype = typ;
          saddr = addr;
          sdata = data;
        })
      raw_sections
  in
  (* symbols from .symtab, using its linked string table *)
  let symbols =
    List.concat_map
      (fun (_, typ, _, off, size, link) ->
        if typ <> 2 then []
        else
          match List.nth_opt raw_sections link with
          | None -> []
          | Some (_, _, _, str_off, _, _) ->
              List.init (size / 24) (fun k ->
                  let b = off + (k * 24) in
                  (name_at str_off (u32 b), u64 (b + 8)))
              |> List.filter (fun (n, _) -> n <> ""))
      raw_sections
  in
  let* () =
    check
      (in_bounds phoff (phnum * phentsize) || phnum = 0)
      "truncated program headers"
  in
  let phdr_at k =
    let b = phoff + (k * phentsize) in
    (u32 b, off64 (b + 8), u64 (b + 16))
  in
  let phdrs =
    (* PT_PHDR if present, else the PT_LOAD whose file range covers them *)
    let all = List.init phnum phdr_at in
    let addr =
      match List.find_opt (fun (ty, _, _) -> ty = 6) all with
      | Some (_, _, vaddr) -> Some vaddr
      | None ->
          List.find_map
            (fun (k, (ty, off, vaddr)) ->
              let filesz = off64 (phoff + (k * phentsize) + 32) in
              if ty = 1 && off <= phoff && phoff < off + filesz then
                Some (Int64.add vaddr (Int64.of_int (phoff - off)))
              else None)
            (List.mapi (fun k p -> (k, p)) all)
    in
    Option.map
      (fun addr -> { Image.addr; num = phnum; entsize = phentsize })
      addr
  in
  let segments =
    List.init phnum (fun k -> phoff + (k * phentsize))
    |> List.filter_map (fun b ->
        if String.get_int32_le s b <> pt_load then None
        else
          let flags = u32 (b + 4)
          and off = off64 (b + 8)
          and vaddr = u64 (b + 16) in
          let filesz = off64 (b + 32) and memsz = off64 (b + 40) in
          if not (in_bounds off filesz) then None
          else
            Some
              {
                Image.name = Printf.sprintf "LOAD@0x%Lx" vaddr;
                vaddr;
                data = Bytes.of_string (String.sub s off filesz);
                mem_size = memsz;
                perm =
                  {
                    r = flags land 4 <> 0;
                    w = flags land 2 <> 0;
                    x = flags land 1 <> 0;
                  };
              })
  in
  Ok { etype; image = { entry; segments; symbols; phdrs }; sections }

let is_elf s = String.length s >= 4 && String.sub s 0 4 = "\x7fELF"
