(** The physical address space: RAM plus memory-mapped devices (ROADMAP 1.2).

    In user mode, RAM covers every address, as a Linux process sees it (pages
    appear on first write). In system mode, RAM is one region, devices occupy
    regions below {!mmio_limit}, and any other address raises an access fault.

    {v
      user mode:    [ RAM .................................................. ]
      system mode:  [ devices ]  ...fault...  [ RAM ram_base, ram_size ]  ...
    v}

    Every access is checked as a whole before anything is written, so a store
    that faults leaves memory unchanged. *)

type region = { base : int64; dev : Device.t }

type t = {
  ram : Memory.t;
  all_ram : bool;  (** user mode: every address is RAM *)
  ram_base : int64;
  ram_size : int64;
  mutable regions : region list;  (** sorted by base *)
  mutable mmio_limit : int64;  (** every device lies below this address *)
  mutable reservations : int64 option array;
      (** LR/SC reservation of each hart, by hart id *)
  mutable holders : int;  (** harts currently holding a reservation *)
}

let make ~all_ram ~ram_base ~ram_size =
  {
    ram = Memory.create ();
    all_ram;
    ram_base;
    ram_size;
    regions = [];
    mmio_limit = 0L;
    reservations = [||];
    holders = 0;
  }

let user () = make ~all_ram:true ~ram_base:0L ~ram_size:(-1L)

(** A system-mode bus with [ram_size] bytes of RAM at [ram_base]. *)
let system ~ram_base ~ram_size = make ~all_ram:false ~ram_base ~ram_size

let ram t = t.ram
let ult a b = Int64.unsigned_compare a b < 0

(** [addr .. addr+size-1] lies in [base .. base+len-1]. *)
let within ~base ~len addr size =
  let off = Int64.sub addr base in
  Int64.unsigned_compare off len < 0
  && Int64.unsigned_compare (Int64.of_int size) (Int64.sub len off) <= 0

let[@inline] in_ram t addr size =
  t.all_ram || within ~base:t.ram_base ~len:t.ram_size addr size

let overlaps a_base a_len b_base b_len =
  ult a_base (Int64.add b_base b_len) && ult b_base (Int64.add a_base a_len)

(** Map [dev] at [base]. Raises [Invalid_argument] if it overlaps RAM or another
    device, or in user mode, where RAM already covers everything. *)
let attach t ~base (dev : Device.t) =
  let clash =
    t.all_ram
    || overlaps base dev.size t.ram_base t.ram_size
    || List.exists (fun r -> overlaps base dev.size r.base r.dev.size) t.regions
  in
  if clash then invalid_arg ("Bus.attach: " ^ dev.name ^ " overlaps");
  t.regions <-
    List.sort
      (fun a b -> Int64.unsigned_compare a.base b.base)
      ({ base; dev } :: t.regions);
  let top = Int64.add base dev.size in
  if ult t.mmio_limit top then t.mmio_limit <- top

let devices t = List.map (fun r -> r.dev) t.regions

let find_region t addr size =
  if ult addr t.mmio_limit then
    List.find_opt
      (fun r -> within ~base:r.base ~len:r.dev.size addr size)
      t.regions
  else None

let mmio_or_fault t addr size cause f =
  match find_region t addr size with
  | Some r -> f r.dev (Int64.sub addr r.base)
  | None -> Trap.raise_trap cause addr

let load_mmio t addr size =
  mmio_or_fault t addr size Load_access_fault (fun d offset ->
      d.read ~offset ~size)

let store_mmio t addr size v =
  mmio_or_fault t addr size Store_access_fault (fun d offset ->
      d.write ~offset ~size v)

(* {1 LR/SC reservations}

   [lr] reserves an address; [sc] succeeds only if the reservation still
   holds. A store by another hart to the reserved 8-byte granule cancels it,
   so harts can build locks from lr/sc. A hart's own stores leave its
   reservation alone (the specification allows either; Spike does the same).
   Reservations live here, not in the hart, because every hart's stores must
   see every other hart's reservation. *)

let reservation t ~hartid =
  if hartid < Array.length t.reservations then t.reservations.(hartid) else None

let set_reservation t ~hartid r =
  if hartid >= Array.length t.reservations then
    t.reservations <- Array.init (hartid + 1) (fun i -> reservation t ~hartid:i);
  let was = t.reservations.(hartid) in
  t.reservations.(hartid) <- r;
  t.holders <- t.holders + Bool.to_int (r <> None) - Bool.to_int (was <> None)

let reserve t ~hartid addr = set_reservation t ~hartid (Some addr)
let cancel_reservation t ~hartid = set_reservation t ~hartid None
let granule addr = Int64.logand addr (-8L)

(** Cancel other harts' reservations overlapping [addr .. addr+size-1]. *)
let break_reservations t ~hartid addr size =
  let first = granule addr
  and last = granule (Int64.add addr (Int64.of_int (size - 1))) in
  Array.iteri
    (fun h r ->
      match r with
      | Some a when h <> hartid && (granule a = first || granule a = last) ->
          cancel_reservation t ~hartid:h
      | _ -> ())
    t.reservations

(* {1 Access} *)

(** [load t addr size] reads [size] (1, 2, 4 or 8) bytes, zero-extended. The RAM
    case is kept small so it can be inlined into the hart. *)
let[@inline] load t addr size =
  if in_ram t addr size then Memory.load t.ram addr size
  else load_mmio t addr size

(** [store t ~hartid addr size v] writes the low [size] bytes of [v], on behalf
    of hart [hartid]. *)
let[@inline] store t ~hartid addr size v =
  if in_ram t addr size then (
    if t.holders > 0 then break_reservations t ~hartid addr size;
    Memory.store t.ram addr size v)
  else store_mmio t addr size v

(** Check an atomic access, which only RAM supports: raises a store/AMO access
    fault (load access fault for [lr]) otherwise. *)
let check_atomic t addr size ~is_load =
  if not (in_ram t addr size) then
    Trap.raise_trap
      (if is_load then Load_access_fault else Store_access_fault)
      addr

(** Code runs only from RAM; anything else is an instruction access fault. An
    instruction of up to 4 bytes must lie in RAM entirely. *)
let[@inline] check_fetch t pc =
  if not (in_ram t pc 2) then Trap.raise_trap Instruction_access_fault pc

let tick t ~now = List.iter (fun r -> r.dev.tick ~now) t.regions
