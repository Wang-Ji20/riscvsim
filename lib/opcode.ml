(** Opcode and funct field assignments from the RISC-V unprivileged spec.
    {!Encode} reads these tables forwards and {!Decode} reads them backwards,
    so the two can't disagree. *)

open Instr

let lui = 0b0110111
let auipc = 0b0010111
let jal = 0b1101111
let jalr = 0b1100111
let branch = 0b1100011
let load = 0b0000011
let store = 0b0100011
let op_imm = 0b0010011
let op_imm_32 = 0b0011011
let op = 0b0110011
let op_32 = 0b0111011
let misc_mem = 0b0001111
let system = 0b1110011
let amo = 0b0101111
let load_fp = 0b0000111
let store_fp = 0b0100111
let op_fp = 0b1010011

let branch_funct3 =
  [ (Beq, 0); (Bne, 1); (Blt, 4); (Bge, 5); (Bltu, 6); (Bgeu, 7) ]

let load_funct3 =
  [ (Lb, 0); (Lh, 1); (Lw, 2); (Ld, 3); (Lbu, 4); (Lhu, 5); (Lwu, 6) ]

let store_funct3 = [ (Sb, 0); (Sh, 1); (Sw, 2); (Sd, 3) ]

(** [(op, (opcode, funct3))] *)
let imm_funct =
  [ (Addi, (op_imm, 0)); (Slti, (op_imm, 2)); (Sltiu, (op_imm, 3));
    (Xori, (op_imm, 4)); (Ori, (op_imm, 6)); (Andi, (op_imm, 7));
    (Addiw, (op_imm_32, 0)) ]
[@@ocamlformat "disable"]

(** [(op, (opcode, funct3, high_bits, shamt_bits))]: the 12-bit immediate is
    [high_bits lsl shamt_bits lor shamt]. *)
let shift_funct =
  [ (Slli, (op_imm, 1, 0x00, 6)); (Srli, (op_imm, 5, 0x00, 6));
    (Srai, (op_imm, 5, 0x10, 6));
    (Slliw, (op_imm_32, 1, 0x00, 5)); (Srliw, (op_imm_32, 5, 0x00, 5));
    (Sraiw, (op_imm_32, 5, 0x20, 5));
    (Rori, (op_imm, 5, 0x18, 6)); (Roriw, (op_imm_32, 5, 0x30, 5));
    (Bclri, (op_imm, 1, 0x12, 6)); (Bseti, (op_imm, 1, 0x0a, 6));
    (Binvi, (op_imm, 1, 0x1a, 6)); (Bexti, (op_imm, 5, 0x12, 6));
    (Slli_uw, (op_imm_32, 1, 0x02, 6)) ]
[@@ocamlformat "disable"]

let shamt_bits op =
  let _, _, _, b = List.assoc op shift_funct in
  b

(** [(op, (opcode, funct3, funct7))] *)
let reg_funct =
  [ (Add, (op, 0, 0x00)); (Sub, (op, 0, 0x20)); (Sll, (op, 1, 0x00));
    (Slt, (op, 2, 0x00)); (Sltu, (op, 3, 0x00)); (Xor, (op, 4, 0x00));
    (Srl, (op, 5, 0x00)); (Sra, (op, 5, 0x20)); (Or, (op, 6, 0x00));
    (And, (op, 7, 0x00));
    (Addw, (op_32, 0, 0x00)); (Subw, (op_32, 0, 0x20));
    (Sllw, (op_32, 1, 0x00)); (Srlw, (op_32, 5, 0x00));
    (Sraw, (op_32, 5, 0x20));
    (Mul, (op, 0, 1)); (Mulh, (op, 1, 1)); (Mulhsu, (op, 2, 1));
    (Mulhu, (op, 3, 1)); (Div, (op, 4, 1)); (Divu, (op, 5, 1));
    (Rem, (op, 6, 1)); (Remu, (op, 7, 1));
    (Mulw, (op_32, 0, 1)); (Divw, (op_32, 4, 1)); (Divuw, (op_32, 5, 1));
    (Remw, (op_32, 6, 1)); (Remuw, (op_32, 7, 1));
    (Sh1add, (op, 2, 0x10)); (Sh2add, (op, 4, 0x10));
    (Sh3add, (op, 6, 0x10)); (Add_uw, (op_32, 0, 0x04));
    (Sh1add_uw, (op_32, 2, 0x10)); (Sh2add_uw, (op_32, 4, 0x10));
    (Sh3add_uw, (op_32, 6, 0x10));
    (Andn, (op, 7, 0x20)); (Orn, (op, 6, 0x20)); (Xnor, (op, 4, 0x20));
    (Max, (op, 6, 0x05)); (Maxu, (op, 7, 0x05)); (Min, (op, 4, 0x05));
    (Minu, (op, 5, 0x05)); (Rol, (op, 1, 0x30)); (Ror, (op, 5, 0x30));
    (Rolw, (op_32, 1, 0x30)); (Rorw, (op_32, 5, 0x30));
    (Bclr, (op, 1, 0x24)); (Bset, (op, 1, 0x14)); (Binv, (op, 1, 0x34));
    (Bext, (op, 5, 0x24)) ]
[@@ocamlformat "disable"]

(** [(op, (opcode, funct3, imm12))]: single-source ops with a fixed
    immediate field ([funct7 lsl 5 lor rs2] for [zext.h]). *)
let unary_funct =
  [ (Clz, (op_imm, 1, 0x600)); (Ctz, (op_imm, 1, 0x601));
    (Cpop, (op_imm, 1, 0x602)); (Sext_b, (op_imm, 1, 0x604));
    (Sext_h, (op_imm, 1, 0x605));
    (Clzw, (op_imm_32, 1, 0x600)); (Ctzw, (op_imm_32, 1, 0x601));
    (Cpopw, (op_imm_32, 1, 0x602));
    (Zext_h, (op_32, 4, 0x080)); (Rev8, (op_imm, 5, 0x6b8));
    (Orc_b, (op_imm, 5, 0x287)) ]
[@@ocamlformat "disable"]

let amo_funct5 =
  [ (Lr, 0x02); (Sc, 0x03); (Amoswap, 0x01); (Amoadd, 0x00);
    (Amoxor, 0x04); (Amoand, 0x0c); (Amoor, 0x08); (Amomin, 0x10);
    (Amomax, 0x14); (Amominu, 0x18); (Amomaxu, 0x1c) ]
[@@ocamlformat "disable"]

let width_funct3 = [ (Word, 2); (Dword, 3) ]
let csr_funct3 = [ (Csrrw, 1); (Csrrs, 2); (Csrrc, 3) ]
let fmt_code = [ (S, 0); (D, 1) ]
let fp_width_funct3 = [ (S, 2); (D, 3) ]

let rm_code =
  [ (Rne, 0); (Rtz, 1); (Rdn, 2); (Rup, 3); (Rmm, 4); (Dyn, 7) ]

let fma_opcode =
  [ (Fmadd, 0b1000011); (Fmsub, 0b1000111); (Fnmsub, 0b1001011);
    (Fnmadd, 0b1001111) ]
[@@ocamlformat "disable"]

(* OP-FP funct5 values. *)
let farith_funct5 = [ (Fadd, 0x00); (Fsub, 0x01); (Fmul, 0x02); (Fdiv, 0x03) ]
let fsqrt_funct5 = 0x0b

(** [(op, (funct5, funct3))] *)
let fsign_funct =
  [ (Fsgnj, (0x04, 0)); (Fsgnjn, (0x04, 1)); (Fsgnjx, (0x04, 2));
    (Fmin, (0x05, 0)); (Fmax, (0x05, 1)) ]
[@@ocamlformat "disable"]

let fcmp_funct5 = 0x14
let fcmp_funct3 = [ (Feq, 2); (Flt, 1); (Fle, 0) ]
let fcvt_fmt_funct5 = 0x08
let fcvt_to_int_funct5 = 0x18
let fcvt_of_int_funct5 = 0x1a
let fclass_funct5 = 0x1c (* funct3 = 1 *)
let fmv_to_int_funct5 = 0x1c (* funct3 = 0 *)
let fmv_of_int_funct5 = 0x1e
let int_ty_code = [ (I32, 0); (U32, 1); (I64, 2); (U64, 3) ]

(** Reverse lookup: the operation whose code is [code], if any. *)
let lookup table code =
  List.find_map (fun (op, c) -> if c = code then Some op else None) table
