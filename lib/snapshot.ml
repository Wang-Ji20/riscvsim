(** Save and restore a whole {!Machine} (ROADMAP 1.6), so a long boot can be
    resumed from a checkpoint instead of rerun.

    A snapshot holds everything architectural: each hart's registers, pc, FP
    state, counters, LR/SC reservation and CSRs; the virtual clock; every RAM
    page; and each device's own state. It does not hold the decode caches (they
    refill) or the harts' environments: an emulated environment such as {!Os}
    keeps host state (open files) that can't be saved, so snapshots are meant
    for system-mode machines.

    Format, all integers little-endian 64-bit:
    {v
      "RVSNAP" 0x00 version(1 byte)
      now, hart count, then per hart:
        hartid pc instret fflags frm exit_code(-1: none) reservation(-1: none)
        x0..x31 f0..f31   csr count, then (csr, value) pairs, where the
        privilege level is the pseudo-CSR -1
      page count, then per page: page number, 4096 bytes
      device count, then per device: name, state   (strings: length + bytes)
    v} *)

let magic = "RVSNAP\000"
let version = 2

(* {1 Writing} *)

let add_i64 b v = Buffer.add_int64_le b v
let add_int b n = add_i64 b (Int64.of_int n)

let add_string b s =
  add_int b (String.length s);
  Buffer.add_string b s

let add_option b = function Some v -> add_i64 b v | None -> add_i64 b (-1L)

let save_hart b (h : Hart.t) =
  add_int b h.hartid;
  add_i64 b (Hart.pc h);
  List.iter (add_int b) [ h.instret; h.fflags; h.frm ];
  add_int b (Option.value h.exit_code ~default:(-1));
  add_option b (Bus.reservation h.bus ~hartid:h.hartid);
  Buffer.add_bytes b h.x;
  Buffer.add_bytes b h.f;
  let csrs = Csr_file.to_list h.csrs in
  add_int b (List.length csrs);
  List.iter
    (fun (csr, v) ->
      add_int b csr;
      add_i64 b v)
    csrs

let save (m : Machine.t) =
  let b = Buffer.create (1 lsl 16) in
  Buffer.add_string b magic;
  Buffer.add_uint8 b version;
  add_int b m.now;
  add_int b (Array.length m.harts);
  Array.iter (save_hart b) m.harts;
  let pages = Memory.pages (Bus.ram m.bus) in
  add_int b (List.length pages);
  List.iter
    (fun (n, bytes) ->
      add_int b n;
      Buffer.add_bytes b bytes)
    pages;
  let devices = Bus.devices m.bus in
  add_int b (List.length devices);
  List.iter
    (fun (d : Device.t) ->
      add_string b d.name;
      add_string b (d.save ()))
    devices;
  Buffer.contents b

(* {1 Reading} *)

exception Bad of string

type reader = { s : string; mutable pos : int }

let take r n =
  if n < 0 || r.pos + n > String.length r.s then raise (Bad "truncated");
  let v = String.sub r.s r.pos n in
  r.pos <- r.pos + n;
  v

let i64 r = String.get_int64_le (take r 8) 0
let int r = Int64.to_int (i64 r)
let str r = take r (int r)
let option r = match i64 r with -1L -> None | v -> Some v
let expect what ok = if not ok then raise (Bad what)

let restore_hart r (h : Hart.t) =
  expect "hart ids differ" (int r = h.hartid);
  Hart.set_pc h (i64 r);
  h.instret <- int r;
  h.fflags <- int r;
  h.frm <- int r;
  h.exit_code <- (match int r with -1 -> None | c -> Some c);
  Bus.set_reservation h.bus ~hartid:h.hartid (option r);
  Bytes.blit_string (take r 256) 0 h.x 0 256;
  Bytes.blit_string (take r 256) 0 h.f 0 256;
  Csr_file.restore h.csrs
    (List.init (int r) (fun _ ->
         let csr = int r in
         (csr, i64 r)))

let restore_ram r ram =
  Memory.clear ram;
  for _ = 1 to int r do
    let n = int r in
    Memory.set_page ram n (Bytes.of_string (take r Memory.page_size))
  done

let restore_devices r bus =
  let devices = Bus.devices bus in
  expect "device count differs" (int r = List.length devices);
  List.iter
    (fun (d : Device.t) ->
      expect ("expected device " ^ d.name) (str r = d.name);
      d.restore (str r))
    devices

(** Restore [snapshot] into [m], which must have the same harts and devices as
    the machine it was taken from. On error, [m] may be partly restored. *)
let restore (m : Machine.t) snapshot =
  let r = { s = snapshot; pos = 0 } in
  try
    expect "not a riscvsim snapshot" (take r (String.length magic) = magic);
    expect "unsupported snapshot version" (Char.code (take r 1).[0] = version);
    m.now <- int r;
    expect "hart count differs" (int r = Array.length m.harts);
    Array.iter (restore_hart r) m.harts;
    restore_ram r (Bus.ram m.bus);
    restore_devices r m.bus;
    expect "trailing data" (r.pos = String.length snapshot);
    Ok ()
  with Bad e -> Error ("snapshot: " ^ e)
