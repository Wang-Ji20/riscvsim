(** RV64GC (+ Zba/Zbb/Zbs) instructions as a typed AST.

    Immediates and offsets are ordinary, already sign-extended [int]s. The
    exception is [Lui]/[Auipc], which hold the raw 20-bit field exactly as it
    is written in assembly ([lui a0, 0x12345]). Compressed (RVC)
    instructions are not a separate type: they expand to these. *)

type branch_op = Beq | Bne | Blt | Bge | Bltu | Bgeu
type load_op = Lb | Lh | Lw | Ld | Lbu | Lhu | Lwu
type store_op = Sb | Sh | Sw | Sd
type imm_op = Addi | Slti | Sltiu | Xori | Ori | Andi | Addiw

type shift_op =
  | Slli | Srli | Srai | Slliw | Srliw | Sraiw
  | Rori | Roriw  (* Zbb *)
  | Bclri | Bseti | Binvi | Bexti  (* Zbs *)
  | Slli_uw  (* Zba *)
[@@ocamlformat "disable"]

type reg_op =
  | Add | Sub | Sll | Slt | Sltu | Xor | Srl | Sra | Or | And
  | Addw | Subw | Sllw | Srlw | Sraw
  (* M *)
  | Mul | Mulh | Mulhsu | Mulhu | Div | Divu | Rem | Remu
  | Mulw | Divw | Divuw | Remw | Remuw
  (* Zba *)
  | Sh1add | Sh2add | Sh3add | Add_uw | Sh1add_uw | Sh2add_uw | Sh3add_uw
  (* Zbb *)
  | Andn | Orn | Xnor | Max | Maxu | Min | Minu | Rol | Ror | Rolw | Rorw
  (* Zbs *)
  | Bclr | Bset | Binv | Bext
[@@ocamlformat "disable"]

(** Zbb single-source operations. *)
type unary_op =
  | Clz | Ctz | Cpop | Clzw | Ctzw | Cpopw
  | Sext_b | Sext_h | Zext_h | Rev8 | Orc_b
[@@ocamlformat "disable"]

type width = Word | Dword

type amo_op =
  | Lr | Sc | Amoswap | Amoadd | Amoxor | Amoand | Amoor
  | Amomin | Amomax | Amominu | Amomaxu
[@@ocamlformat "disable"]

type csr_op = Csrrw | Csrrs | Csrrc
type csr_src = Rs1 of Reg.t | Uimm of int  (** 5-bit zero-extended *)

(** Floating-point format. *)
type fmt = S | D

(** Rounding mode. [Dyn] means use [frm] from [fcsr]. *)
type rm = Rne | Rtz | Rdn | Rup | Rmm | Dyn

type farith_op = Fadd | Fsub | Fmul | Fdiv
type fsign_op = Fsgnj | Fsgnjn | Fsgnjx | Fmin | Fmax
type fma_op = Fmadd | Fmsub | Fnmsub | Fnmadd
type fcmp_op = Feq | Flt | Fle

(** Integer type for FP conversions: [.w], [.wu], [.l], [.lu]. *)
type int_ty = I32 | U32 | I64 | U64

type t =
  (* RV64I *)
  | Lui of { rd : Reg.t; imm : int }
  | Auipc of { rd : Reg.t; imm : int }
  | Jal of { rd : Reg.t; offset : int }
  | Jalr of { rd : Reg.t; rs1 : Reg.t; offset : int }
  | Branch of { op : branch_op; rs1 : Reg.t; rs2 : Reg.t; offset : int }
  | Load of { op : load_op; rd : Reg.t; rs1 : Reg.t; offset : int }
  | Store of { op : store_op; rs1 : Reg.t; rs2 : Reg.t; offset : int }
      (** [mem[rs1 + offset] <- rs2] *)
  | Op_imm of { op : imm_op; rd : Reg.t; rs1 : Reg.t; imm : int }
  | Shift_imm of { op : shift_op; rd : Reg.t; rs1 : Reg.t; shamt : int }
  | Op of { op : reg_op; rd : Reg.t; rs1 : Reg.t; rs2 : Reg.t }
  | Unary of { op : unary_op; rd : Reg.t; rs1 : Reg.t }
  | Fence of { pred : int; succ : int }  (** 4-bit iorw sets *)
  | Fence_i
  | Ecall
  | Ebreak
  (* Zicsr *)
  | Csr of { op : csr_op; rd : Reg.t; src : csr_src; csr : int }
  (* A *)
  | Amo of {
      op : amo_op;
      width : width;
      aq : bool;
      rl : bool;
      rd : Reg.t;
      rs1 : Reg.t;
      rs2 : Reg.t;  (** always [zero] for [Lr] *)
    }
  (* F / D *)
  | Fload of { fmt : fmt; rd : Freg.t; rs1 : Reg.t; offset : int }
  | Fstore of { fmt : fmt; rs1 : Reg.t; rs2 : Freg.t; offset : int }
  | Farith of {
      op : farith_op;
      fmt : fmt;
      rd : Freg.t;
      rs1 : Freg.t;
      rs2 : Freg.t;
      rm : rm;
    }
  | Fsqrt of { fmt : fmt; rd : Freg.t; rs1 : Freg.t; rm : rm }
  | Fsign of {
      op : fsign_op;
      fmt : fmt;
      rd : Freg.t;
      rs1 : Freg.t;
      rs2 : Freg.t;
    }
  | Fma of {
      op : fma_op;
      fmt : fmt;
      rd : Freg.t;
      rs1 : Freg.t;
      rs2 : Freg.t;
      rs3 : Freg.t;
      rm : rm;
    }
  | Fcmp of { op : fcmp_op; fmt : fmt; rd : Reg.t; rs1 : Freg.t; rs2 : Freg.t }
  | Fclass of { fmt : fmt; rd : Reg.t; rs1 : Freg.t }
  | Fcvt_to_int of {
      fmt : fmt;
      ity : int_ty;
      rd : Reg.t;
      rs1 : Freg.t;
      rm : rm;
    }
  | Fcvt_of_int of {
      fmt : fmt;
      ity : int_ty;
      rd : Freg.t;
      rs1 : Reg.t;
      rm : rm;
    }
  | Fcvt_fmt of { fmt : fmt; rd : Freg.t; rs1 : Freg.t; rm : rm }
      (** convert to [fmt] from the other format *)
  | Fmv_to_int of { fmt : fmt; rd : Reg.t; rs1 : Freg.t }
  | Fmv_of_int of { fmt : fmt; rd : Freg.t; rs1 : Reg.t }

(* {1 Mnemonic tables}

   Shared with the parser, so each table is the one place a spelling is
   defined. *)

let branch_ops = [ (Beq, "beq"); (Bne, "bne"); (Blt, "blt"); (Bge, "bge");
                   (Bltu, "bltu"); (Bgeu, "bgeu") ]
[@@ocamlformat "disable"]

let load_ops = [ (Lb, "lb"); (Lh, "lh"); (Lw, "lw"); (Ld, "ld");
                 (Lbu, "lbu"); (Lhu, "lhu"); (Lwu, "lwu") ]
[@@ocamlformat "disable"]

let store_ops = [ (Sb, "sb"); (Sh, "sh"); (Sw, "sw"); (Sd, "sd") ]

let imm_ops = [ (Addi, "addi"); (Slti, "slti"); (Sltiu, "sltiu");
                (Xori, "xori"); (Ori, "ori"); (Andi, "andi"); (Addiw, "addiw") ]
[@@ocamlformat "disable"]

let shift_ops =
  [ (Slli, "slli"); (Srli, "srli"); (Srai, "srai"); (Slliw, "slliw");
    (Srliw, "srliw"); (Sraiw, "sraiw"); (Rori, "rori"); (Roriw, "roriw");
    (Bclri, "bclri"); (Bseti, "bseti"); (Binvi, "binvi"); (Bexti, "bexti");
    (Slli_uw, "slli.uw") ]
[@@ocamlformat "disable"]

let reg_ops =
  [ (Add, "add"); (Sub, "sub"); (Sll, "sll"); (Slt, "slt"); (Sltu, "sltu");
    (Xor, "xor"); (Srl, "srl"); (Sra, "sra"); (Or, "or"); (And, "and");
    (Addw, "addw"); (Subw, "subw"); (Sllw, "sllw"); (Srlw, "srlw");
    (Sraw, "sraw");
    (Mul, "mul"); (Mulh, "mulh"); (Mulhsu, "mulhsu"); (Mulhu, "mulhu");
    (Div, "div"); (Divu, "divu"); (Rem, "rem"); (Remu, "remu");
    (Mulw, "mulw"); (Divw, "divw"); (Divuw, "divuw"); (Remw, "remw");
    (Remuw, "remuw");
    (Sh1add, "sh1add"); (Sh2add, "sh2add"); (Sh3add, "sh3add");
    (Add_uw, "add.uw"); (Sh1add_uw, "sh1add.uw"); (Sh2add_uw, "sh2add.uw");
    (Sh3add_uw, "sh3add.uw");
    (Andn, "andn"); (Orn, "orn"); (Xnor, "xnor"); (Max, "max");
    (Maxu, "maxu"); (Min, "min"); (Minu, "minu"); (Rol, "rol"); (Ror, "ror");
    (Rolw, "rolw"); (Rorw, "rorw");
    (Bclr, "bclr"); (Bset, "bset"); (Binv, "binv"); (Bext, "bext") ]
[@@ocamlformat "disable"]

let unary_ops =
  [ (Clz, "clz"); (Ctz, "ctz"); (Cpop, "cpop"); (Clzw, "clzw");
    (Ctzw, "ctzw"); (Cpopw, "cpopw"); (Sext_b, "sext.b"); (Sext_h, "sext.h");
    (Zext_h, "zext.h"); (Rev8, "rev8"); (Orc_b, "orc.b") ]
[@@ocamlformat "disable"]

let amo_ops =
  [ (Lr, "lr"); (Sc, "sc"); (Amoswap, "amoswap"); (Amoadd, "amoadd");
    (Amoxor, "amoxor"); (Amoand, "amoand"); (Amoor, "amoor");
    (Amomin, "amomin"); (Amomax, "amomax"); (Amominu, "amominu");
    (Amomaxu, "amomaxu") ]
[@@ocamlformat "disable"]

let csr_ops = [ (Csrrw, "csrrw"); (Csrrs, "csrrs"); (Csrrc, "csrrc") ]
let widths = [ (Word, "w"); (Dword, "d") ]
let fmts = [ (S, "s"); (D, "d") ]

let rms =
  [ (Rne, "rne"); (Rtz, "rtz"); (Rdn, "rdn"); (Rup, "rup"); (Rmm, "rmm");
    (Dyn, "dyn") ]
[@@ocamlformat "disable"]

let farith_ops = [ (Fadd, "fadd"); (Fsub, "fsub"); (Fmul, "fmul"); (Fdiv, "fdiv") ]

let fsign_ops = [ (Fsgnj, "fsgnj"); (Fsgnjn, "fsgnjn"); (Fsgnjx, "fsgnjx");
                  (Fmin, "fmin"); (Fmax, "fmax") ]
[@@ocamlformat "disable"]

let fma_ops = [ (Fmadd, "fmadd"); (Fmsub, "fmsub"); (Fnmsub, "fnmsub");
                (Fnmadd, "fnmadd") ]
[@@ocamlformat "disable"]

let fcmp_ops = [ (Feq, "feq"); (Flt, "flt"); (Fle, "fle") ]
let int_tys = [ (I32, "w"); (U32, "wu"); (I64, "l"); (U64, "lu") ]

let name table op = List.assoc op table

(** Conversions that are always exact. Assemblers encode them with [rne]
    rather than [dyn] by default. *)
let default_rm = function
  | Fcvt_of_int { fmt = D; ity = I32 | U32; _ } | Fcvt_fmt { fmt = D; _ } -> Rne
  | _ -> Dyn

(** Render a fence's iorw set: [0b1111] is ["iorw"]. *)
let fence_set n =
  String.concat ""
    (List.filter_map
       (fun (bit, c) -> if n land bit <> 0 then Some c else None)
       [ (8, "i"); (4, "o"); (2, "r"); (1, "w") ])
let other_fmt = function S -> D | D -> S

(* {1 Pretty printing (disassembly)} *)

let pp ppf =
  let open Format in
  let r = Reg.pp and f = Freg.pp in
  let fm = name fmts in
  let pp_rm i ppf rm = if rm <> default_rm i then fprintf ppf ", %s" (name rms rm) in
  let pp_csr ppf csr =
    match Csr.name csr with
    | Some n -> pp_print_string ppf n
    | None -> fprintf ppf "0x%x" csr
  in
  let ordering aq rl =
    match (aq, rl) with
    | false, false -> ""
    | true, false -> ".aq"
    | false, true -> ".rl"
    | true, true -> ".aqrl"
  in
  fun i ->
  let pp_rm = pp_rm i in
  match i with
  | Lui { rd; imm } -> fprintf ppf "lui %a, 0x%x" r rd imm
  | Auipc { rd; imm } -> fprintf ppf "auipc %a, 0x%x" r rd imm
  | Jal { rd; offset } -> fprintf ppf "jal %a, %d" r rd offset
  | Jalr { rd; rs1; offset } -> fprintf ppf "jalr %a, %d(%a)" r rd offset r rs1
  | Branch { op; rs1; rs2; offset } ->
      fprintf ppf "%s %a, %a, %d" (name branch_ops op) r rs1 r rs2 offset
  | Load { op; rd; rs1; offset } ->
      fprintf ppf "%s %a, %d(%a)" (name load_ops op) r rd offset r rs1
  | Store { op; rs1; rs2; offset } ->
      fprintf ppf "%s %a, %d(%a)" (name store_ops op) r rs2 offset r rs1
  | Op_imm { op; rd; rs1; imm } ->
      fprintf ppf "%s %a, %a, %d" (name imm_ops op) r rd r rs1 imm
  | Shift_imm { op; rd; rs1; shamt } ->
      fprintf ppf "%s %a, %a, %d" (name shift_ops op) r rd r rs1 shamt
  | Op { op; rd; rs1; rs2 } ->
      fprintf ppf "%s %a, %a, %a" (name reg_ops op) r rd r rs1 r rs2
  | Unary { op; rd; rs1 } -> fprintf ppf "%s %a, %a" (name unary_ops op) r rd r rs1
  | Fence { pred = 15; succ = 15 } -> pp_print_string ppf "fence"
  | Fence { pred; succ } -> fprintf ppf "fence %s, %s" (fence_set pred) (fence_set succ)
  | Fence_i -> pp_print_string ppf "fence.i"
  | Ecall -> pp_print_string ppf "ecall"
  | Ebreak -> pp_print_string ppf "ebreak"
  | Csr { op; rd; src = Rs1 rs1; csr } ->
      fprintf ppf "%s %a, %a, %a" (name csr_ops op) r rd pp_csr csr r rs1
  | Csr { op; rd; src = Uimm u; csr } ->
      fprintf ppf "%si %a, %a, %d" (name csr_ops op) r rd pp_csr csr u
  | Amo { op = Lr; width; aq; rl; rd; rs1; _ } ->
      fprintf ppf "lr.%s%s %a, (%a)" (name widths width) (ordering aq rl) r rd
        r rs1
  | Amo { op; width; aq; rl; rd; rs1; rs2 } ->
      fprintf ppf "%s.%s%s %a, %a, (%a)" (name amo_ops op) (name widths width)
        (ordering aq rl) r rd r rs2 r rs1
  | Fload { fmt; rd; rs1; offset } ->
      fprintf ppf "%s %a, %d(%a)" (if fmt = S then "flw" else "fld") f rd offset r rs1
  | Fstore { fmt; rs1; rs2; offset } ->
      fprintf ppf "%s %a, %d(%a)" (if fmt = S then "fsw" else "fsd") f rs2 offset r rs1
  | Farith { op; fmt; rd; rs1; rs2; rm } ->
      fprintf ppf "%s.%s %a, %a, %a%a" (name farith_ops op) (fm fmt) f rd f rs1
        f rs2 pp_rm rm
  | Fsqrt { fmt; rd; rs1; rm } ->
      fprintf ppf "fsqrt.%s %a, %a%a" (fm fmt) f rd f rs1 pp_rm rm
  | Fsign { op; fmt; rd; rs1; rs2 } ->
      fprintf ppf "%s.%s %a, %a, %a" (name fsign_ops op) (fm fmt) f rd f rs1 f rs2
  | Fma { op; fmt; rd; rs1; rs2; rs3; rm } ->
      fprintf ppf "%s.%s %a, %a, %a, %a%a" (name fma_ops op) (fm fmt) f rd f rs1
        f rs2 f rs3 pp_rm rm
  | Fcmp { op; fmt; rd; rs1; rs2 } ->
      fprintf ppf "%s.%s %a, %a, %a" (name fcmp_ops op) (fm fmt) r rd f rs1 f rs2
  | Fclass { fmt; rd; rs1 } -> fprintf ppf "fclass.%s %a, %a" (fm fmt) r rd f rs1
  | Fcvt_to_int { fmt; ity; rd; rs1; rm } ->
      fprintf ppf "fcvt.%s.%s %a, %a%a" (name int_tys ity) (fm fmt) r rd f rs1
        pp_rm rm
  | Fcvt_of_int { fmt; ity; rd; rs1; rm } ->
      fprintf ppf "fcvt.%s.%s %a, %a%a" (fm fmt) (name int_tys ity) f rd r rs1
        pp_rm rm
  | Fcvt_fmt { fmt; rd; rs1; rm } ->
      fprintf ppf "fcvt.%s.%s %a, %a%a" (fm fmt) (fm (other_fmt fmt)) f rd f rs1
        pp_rm rm
  | Fmv_to_int { fmt; rd; rs1 } ->
      fprintf ppf "fmv.x.%s %a, %a" (if fmt = S then "w" else "d") r rd f rs1
  | Fmv_of_int { fmt; rd; rs1 } ->
      fprintf ppf "fmv.%s.x %a, %a" (if fmt = S then "w" else "d") f rd r rs1

let to_string = Format.asprintf "%a" pp
