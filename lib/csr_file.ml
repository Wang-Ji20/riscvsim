(** The privileged state of a hart: its privilege level and the machine and
    supervisor CSRs (ROADMAP 2.1-2.3, 2.6-2.8).

    Registers are WARL ("write any, read legal"): a write keeps only the bits
    that exist, and a field written with a reserved value keeps a legal one.
    The floating-point CSRs and the counters belong to the hart itself and are
    handled there; {!check} still decides whether they may be accessed. *)

(* {1 mstatus fields} *)

let bit n = Int64.shift_left 1L n
let field ~lo ~width = Int64.shift_left (Int64.pred (bit width)) lo
let sie_bit = bit 1
let mie_bit = bit 3
let spie_bit = bit 5
let mpie_bit = bit 7
let spp_bit = bit 8
let mpp_field = field ~lo:11 ~width:2
let fs_field = field ~lo:13 ~width:2
let mprv_bit = bit 17
let sum_bit = bit 18
let mxr_bit = bit 19
let tvm_bit = bit 20
let tw_bit = bit 21
let tsr_bit = bit 22
let sd_bit = bit 63

(** UXL = SXL = 2: user and supervisor modes are 64-bit, read-only. *)
let xl_fields = Int64.shift_left 0xaL 32

let mstatus_writable =
  List.fold_left Int64.logor 0L
    [ sie_bit; mie_bit; spie_bit; mpie_bit; spp_bit; mpp_field; fs_field;
      mprv_bit; sum_bit; mxr_bit; tvm_bit; tw_bit; tsr_bit ]
[@@ocamlformat "disable"]

(** The bits of mstatus that sstatus shows. *)
let sstatus_mask =
  List.fold_left Int64.logor 0L
    [ sie_bit; spie_bit; spp_bit; fs_field; sum_bit; mxr_bit;
      Int64.shift_left 3L 32 (* UXL *); sd_bit ]
[@@ocamlformat "disable"]

(* {1 Interrupt and exception bits} *)

let ssip = bit 1
let msip = bit 3
let stip = bit 5
let mtip = bit 7
let seip = bit 9
let meip = bit 11
let s_interrupts = Int64.logor ssip (Int64.logor stip seip)
let all_interrupts = Int64.logor s_interrupts (Int64.logor msip (Int64.logor mtip meip))

(** Exceptions that may be delegated to S-mode: all except an ecall from M. *)
let delegable_exceptions = 0xb3ffL

(** misa: RV64 with A, C, D, F, I, M, S, U. *)
let misa_value =
  List.fold_left
    (fun acc c -> Int64.logor acc (bit (Char.code c - Char.code 'A')))
    (Int64.shift_left 2L 62)
    [ 'A'; 'C'; 'D'; 'F'; 'I'; 'M'; 'S'; 'U' ]

(* {1 State} *)

type t = {
  hartid : int;
  mutable priv : Priv.t;
  mutable mstatus : int64;
  mutable medeleg : int64;
  mutable mideleg : int64;
  mutable mie : int64;
  mutable mip : int64;
  mutable mtvec : int64;
  mutable mcounteren : int64;
  mutable menvcfg : int64;
  mutable mcountinhibit : int64;
  mutable mscratch : int64;
  mutable mepc : int64;
  mutable mcause : int64;
  mutable mtval : int64;
  mutable stvec : int64;
  mutable scounteren : int64;
  mutable senvcfg : int64;
  mutable sscratch : int64;
  mutable sepc : int64;
  mutable scause : int64;
  mutable stval : int64;
  mutable satp : int64;
  pmpcfg : int64 array;  (** pmpcfg0, 2, ..., 14: eight entries each *)
  pmpaddr : int64 array;
}

(** The state at reset: M-mode, everything zero, the FPU off. *)
let create ~hartid =
  {
    hartid;
    priv = M;
    mstatus = xl_fields;
    medeleg = 0L;
    mideleg = 0L;
    mie = 0L;
    mip = 0L;
    mtvec = 0L;
    mcounteren = 0L;
    menvcfg = 0L;
    mcountinhibit = 0L;
    mscratch = 0L;
    mepc = 0L;
    mcause = 0L;
    mtval = 0L;
    stvec = 0L;
    scounteren = 0L;
    senvcfg = 0L;
    sscratch = 0L;
    sepc = 0L;
    scause = 0L;
    stval = 0L;
    satp = 0L;
    pmpcfg = Array.make 8 0L;
    pmpaddr = Array.make 64 0L;
  }

let has t mask = Int64.logand t.mstatus mask <> 0L
let fs t = Int64.to_int (Int64.shift_right_logical t.mstatus 13) land 3
let fp_enabled t = fs t <> 0

(** SD summarises "some state is dirty": it is set exactly when FS is. *)
let with_sd v =
  if Int64.logand v fs_field = fs_field then Int64.logor v sd_bit
  else Int64.logand v (Int64.lognot sd_bit)

(** Record that FP state changed (FS = Dirty), as every FP write must. *)
let set_fp_dirty t =
  if Int64.logand t.mstatus fs_field <> fs_field then
    t.mstatus <- Int64.logor t.mstatus (Int64.logor fs_field sd_bit)

(** Set mstatus.FS, for an environment that starts with the FPU usable. *)
let set_fs t fs =
  t.mstatus <-
    with_sd
      (Int64.logor
         (Int64.logand t.mstatus (Int64.lognot fs_field))
         (Int64.shift_left (Int64.of_int (fs land 3)) 13))

(* {1 Access checks} *)


(** May the current privilege read the counter CSR [n] (0xc00-0xc1f)? S-mode
    needs mcounteren's bit; U-mode needs it in mcounteren and scounteren. *)
let counter_allowed t n =
  let b = bit (n land 31) in
  let enabled r = Int64.logand r b <> 0L in
  match t.priv with
  | M -> true
  | S -> enabled t.mcounteren
  | U -> enabled t.mcounteren && enabled t.scounteren

(** May CSR [n] be accessed now, writing it if [write]? Covers the
    privilege and read-only rules encoded in the number, plus the FPU being
    off (FS = 0), counter enables, and TVM for satp. Whether the CSR exists
    is decided separately. *)
let check t n ~write =
  Priv.(t.priv >= Csr.min_priv n)
  && ((not write) || not (Csr.read_only n))
  && (match n with
     | 0x001 | 0x002 | 0x003 -> fp_enabled t
     | _ when n >= 0xc00 && n <= 0xc1f -> counter_allowed t n
     | 0x180 -> not (t.priv = S && has t tvm_bit)
     | _ -> true)

(* {1 Reading and writing} *)

let legal_priv v = match Priv.of_int v with Some p -> p | None -> U

(** mstatus after writing [v]: only writable bits change, MPP keeps a legal
    privilege, and SD follows FS. *)
let write_mstatus t v =
  let keep = Int64.logand t.mstatus (Int64.lognot mstatus_writable) in
  let v = Int64.logor keep (Int64.logand v mstatus_writable) in
  let mpp = Int64.to_int (Int64.shift_right_logical v 11) land 3 in
  let v =
    Int64.logor
      (Int64.logand v (Int64.lognot mpp_field))
      (Int64.shift_left (Int64.of_int (Priv.to_int (legal_priv mpp))) 11)
  in
  t.mstatus <- with_sd v

let masked_update old v mask =
  Int64.logor (Int64.logand old (Int64.lognot mask)) (Int64.logand v mask)

(** mtvec/stvec: modes 0 (direct) and 1 (vectored) exist; bit 1 is zero. *)
let legal_tvec v = Int64.logand v (Int64.lognot 2L)

(** mepc/sepc: instructions are 2-byte aligned (C extension). *)
let legal_epc v = Int64.logand v (Int64.lognot 1L)

(** satp: only Bare (mode 0) for now; writes selecting another mode are
    ignored, as the spec requires for unsupported modes (Sv39 is Phase 3). *)
let write_satp t v =
  if Int64.shift_right_logical v 60 = 0L then t.satp <- v

let pmpcfg_index n =
  let i = n - Csr.pmpcfg0 in
  if i land 1 = 0 then Some (i / 2) else None (* odd ones don't exist on RV64 *)

let pmpaddr_mask = Int64.pred (bit 54)

(** The value of CSR [n], or [None] if this file doesn't implement it. *)
let read t n =
  let some v = Some v in
  match n with
  | 0x100 -> some (Int64.logand t.mstatus sstatus_mask)
  | 0x104 -> some (Int64.logand t.mie t.mideleg)
  | 0x105 -> some t.stvec
  | 0x106 -> some t.scounteren
  | 0x10a -> some t.senvcfg
  | 0x140 -> some t.sscratch
  | 0x141 -> some t.sepc
  | 0x142 -> some t.scause
  | 0x143 -> some t.stval
  | 0x144 -> some (Int64.logand t.mip t.mideleg)
  | 0x180 -> some t.satp
  | 0xf11 | 0xf12 | 0xf13 | 0xf15 -> some 0L
  | 0xf14 -> some (Int64.of_int t.hartid)
  | 0x300 -> some t.mstatus
  | 0x301 -> some misa_value
  | 0x302 -> some t.medeleg
  | 0x303 -> some t.mideleg
  | 0x304 -> some t.mie
  | 0x305 -> some t.mtvec
  | 0x306 -> some t.mcounteren
  | 0x30a -> some t.menvcfg
  | 0x320 -> some t.mcountinhibit
  | 0x340 -> some t.mscratch
  | 0x341 -> some t.mepc
  | 0x342 -> some t.mcause
  | 0x343 -> some t.mtval
  | 0x344 -> some t.mip
  | _ when n >= 0x323 && n <= 0x33f -> some 0L (* mhpmevent3-31 *)
  | _ when n >= 0xb03 && n <= 0xb1f -> some 0L (* mhpmcounter3-31 *)
  | _ when n >= 0xc03 && n <= 0xc1f -> some 0L (* hpmcounter3-31 *)
  | _ when n >= Csr.pmpcfg0 && n < Csr.pmpcfg0 + 16 ->
      Option.map (fun i -> t.pmpcfg.(i)) (pmpcfg_index n)
  | _ when n >= Csr.pmpaddr0 && n < Csr.pmpaddr0 + 64 ->
      some t.pmpaddr.(n - Csr.pmpaddr0)
  | _ -> None

(** Write [v] to CSR [n], which {!read} implements. Read-only registers and
    unimplemented fields ignore the write. *)
let write t n v =
  match n with
  | 0x100 ->
      write_mstatus t (masked_update t.mstatus v sstatus_mask)
  | 0x104 -> t.mie <- masked_update t.mie v t.mideleg
  | 0x105 -> t.stvec <- legal_tvec v
  | 0x106 -> t.scounteren <- Int64.logand v 0xffff_ffffL
  | 0x10a -> t.senvcfg <- Int64.logand v 1L
  | 0x140 -> t.sscratch <- v
  | 0x141 -> t.sepc <- legal_epc v
  | 0x142 -> t.scause <- v
  | 0x143 -> t.stval <- v
  | 0x144 -> t.mip <- masked_update t.mip v (Int64.logand t.mideleg ssip)
  | 0x180 -> write_satp t v
  | 0x300 -> write_mstatus t v
  | 0x302 -> t.medeleg <- Int64.logand v delegable_exceptions
  | 0x303 -> t.mideleg <- Int64.logand v s_interrupts
  | 0x304 -> t.mie <- Int64.logand v all_interrupts
  | 0x305 -> t.mtvec <- legal_tvec v
  | 0x306 -> t.mcounteren <- Int64.logand v 0xffff_ffffL
  | 0x30a -> t.menvcfg <- Int64.logand v 1L
  | 0x320 -> t.mcountinhibit <- Int64.logand v 0xffff_fffdL
  | 0x340 -> t.mscratch <- v
  | 0x341 -> t.mepc <- legal_epc v
  | 0x342 -> t.mcause <- v
  | 0x343 -> t.mtval <- v
  | 0x344 -> t.mip <- masked_update t.mip v s_interrupts
  | _ when n >= Csr.pmpcfg0 && n < Csr.pmpcfg0 + 16 ->
      Option.iter (fun i -> t.pmpcfg.(i) <- v) (pmpcfg_index n)
  | _ when n >= Csr.pmpaddr0 && n < Csr.pmpaddr0 + 64 ->
      t.pmpaddr.(n - Csr.pmpaddr0) <- Int64.logand v pmpaddr_mask
  | _ -> () (* misa, ids, hpm counters and events: read-only or hardwired *)

(* {1 Returning from a trap} *)

let set_bit v mask on =
  if on then Int64.logor v mask else Int64.logand v (Int64.lognot mask)

(** [mret]: return to the privilege saved in MPP, restoring MIE from MPIE.
    MPIE becomes 1 and MPP becomes U. Leaving M-mode clears MPRV. Returns
    the new pc, mepc. *)
let mret t =
  let target =
    legal_priv (Int64.to_int (Int64.shift_right_logical t.mstatus 11) land 3)
  in
  let s = t.mstatus in
  let s = set_bit s mie_bit (Int64.logand s mpie_bit <> 0L) in
  let s = set_bit s mpie_bit true in
  let s = Int64.logand s (Int64.lognot mpp_field) in
  let s = if target <> M then Int64.logand s (Int64.lognot mprv_bit) else s in
  t.mstatus <- s;
  t.priv <- target;
  t.mepc

(** [sret]: the same for S-mode, with SPP, SIE and SPIE. The target is never
    M, so MPRV is cleared. Returns sepc. *)
let sret t =
  let target = if has t spp_bit then Priv.S else U in
  let s = t.mstatus in
  let s = set_bit s sie_bit (Int64.logand s spie_bit <> 0L) in
  let s = set_bit s spie_bit true in
  let s = Int64.logand s (Int64.lognot (Int64.logor spp_bit mprv_bit)) in
  t.mstatus <- s;
  t.priv <- target;
  t.sepc

(* {1 Snapshots} *)

(** Every stateful register, as (CSR number, value), plus the privilege level
    under the pseudo-number -1. *)
let to_list t =
  ((-1, Int64.of_int (Priv.to_int t.priv))
   :: List.filter_map
        (fun n -> Option.map (fun v -> (n, v)) (read t n))
        [ 0x300; 0x302; 0x303; 0x304; 0x305; 0x306; 0x30a; 0x320; 0x340;
          0x341; 0x342; 0x343; 0x344; 0x105; 0x106; 0x10a; 0x140; 0x141;
          0x142; 0x143; 0x180 ])
  @ List.init 8 (fun i -> (Csr.pmpcfg0 + (2 * i), t.pmpcfg.(i)))
  @ List.init 64 (fun i -> (Csr.pmpaddr0 + i, t.pmpaddr.(i)))
[@@ocamlformat "disable"]

(** Restore what {!to_list} produced. Values are stored directly, without
    WARL legalisation, since they came from a legal state. *)
let restore t pairs =
  List.iter
    (fun (n, v) ->
      match n with
      | -1 -> t.priv <- legal_priv (Int64.to_int v)
      | 0x300 -> t.mstatus <- v
      | 0x302 -> t.medeleg <- v
      | 0x303 -> t.mideleg <- v
      | 0x304 -> t.mie <- v
      | 0x344 -> t.mip <- v
      | _ -> write t n v)
    pairs
