(** Control and status register numbers and names: the single table shared by
    the assembler, the disassembler and the hart.

    A CSR's number encodes its access rules (privileged spec 2.1): bits 11:10
    are [0b11] for read-only registers, and bits 9:8 give the lowest privilege
    level that may access it. *)

(* Floating point and counters (unprivileged) *)
let fflags = 0x001
let frm = 0x002
let fcsr = 0x003
let cycle = 0xc00
let time = 0xc01
let instret = 0xc02

(* Supervisor *)
let sstatus = 0x100
let sie = 0x104
let stvec = 0x105
let scounteren = 0x106
let senvcfg = 0x10a
let sscratch = 0x140
let sepc = 0x141
let scause = 0x142
let stval = 0x143
let sip = 0x144
let satp = 0x180

(* Machine *)
let mvendorid = 0xf11
let marchid = 0xf12
let mimpid = 0xf13
let mhartid = 0xf14
let mconfigptr = 0xf15
let mstatus = 0x300
let misa = 0x301
let medeleg = 0x302
let mideleg = 0x303
let mie = 0x304
let mtvec = 0x305
let mcounteren = 0x306
let menvcfg = 0x30a
let mcountinhibit = 0x320
let mscratch = 0x340
let mepc = 0x341
let mcause = 0x342
let mtval = 0x343
let mip = 0x344
let pmpcfg0 = 0x3a0
let pmpaddr0 = 0x3b0
let tselect = 0x7a0
let tdata1 = 0x7a1
let tdata2 = 0x7a2
let tdata3 = 0x7a3
let mcycle = 0xb00
let minstret = 0xb02

let range base first last prefix =
  List.init
    (last - first + 1)
    (fun i -> (base + i, prefix ^ string_of_int (first + i)))

let table =
  [ (fflags, "fflags"); (frm, "frm"); (fcsr, "fcsr"); (cycle, "cycle");
    (time, "time"); (instret, "instret");
    (sstatus, "sstatus"); (sie, "sie"); (stvec, "stvec");
    (scounteren, "scounteren"); (senvcfg, "senvcfg"); (sscratch, "sscratch");
    (sepc, "sepc"); (scause, "scause"); (stval, "stval"); (sip, "sip");
    (satp, "satp");
    (mvendorid, "mvendorid"); (marchid, "marchid"); (mimpid, "mimpid");
    (mhartid, "mhartid"); (mconfigptr, "mconfigptr"); (mstatus, "mstatus");
    (misa, "misa"); (medeleg, "medeleg"); (mideleg, "mideleg"); (mie, "mie");
    (mtvec, "mtvec"); (mcounteren, "mcounteren"); (menvcfg, "menvcfg");
    (mcountinhibit, "mcountinhibit"); (mscratch, "mscratch"); (mepc, "mepc");
    (mcause, "mcause"); (mtval, "mtval"); (mip, "mip"); (tselect, "tselect");
    (tdata1, "tdata1"); (tdata2, "tdata2"); (tdata3, "tdata3"); (mcycle, "mcycle");
    (minstret, "minstret") ]
  @ range 0xc03 3 31 "hpmcounter"
  @ range 0xb03 3 31 "mhpmcounter"
  @ range 0x323 3 31 "mhpmevent"
  (* RV64 has only the even pmpcfg registers; the odd ones are RV32-only *)
  @ List.init 8 (fun i -> (pmpcfg0 + (2 * i), "pmpcfg" ^ string_of_int (2 * i)))
  @ range pmpaddr0 0 63 "pmpaddr"
[@@ocamlformat "disable"]

let by_number = Hashtbl.of_seq (List.to_seq table)

let by_name =
  Hashtbl.of_seq (List.to_seq (List.map (fun (n, s) -> (s, n)) table))

let name n = Hashtbl.find_opt by_number n
let of_name s = Hashtbl.find_opt by_name s
let read_only n = (n lsr 10) land 3 = 3

(** The lowest privilege allowed to access CSR [n]. Bits 9:8 = 2 belong to the
    hypervisor extension, which we don't implement: treated as M. *)
let min_priv n =
  match (n lsr 8) land 3 with 0 -> Priv.U | 1 -> Priv.S | _ -> Priv.M
