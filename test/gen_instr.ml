(** QCheck2 generators for RV64GC instructions, respecting every field's
    encodable range. *)

open Riscv
open Instr
module G = QCheck2.Gen

let ( let+ ) = G.( let+ )
let ( and+ ) = G.( and+ )
let ( let* ) = G.( let* )
let reg = G.map Reg.of_int (G.int_range 0 31)
let freg = G.map Freg.of_int (G.int_range 0 31)
let signed bits = G.int_range (-(1 lsl (bits - 1))) ((1 lsl (bits - 1)) - 1)
let unsigned bits = G.int_range 0 ((1 lsl bits) - 1)
let even bits = G.map (fun v -> v land lnot 1) (signed bits)
let of_table t = G.oneof_list (List.map fst t)
let rm = of_table rms
let fmt = of_table fmts

(** Any encodable instruction. [~fence_nonempty] keeps fence sets non-zero (an
    empty set has no assembly syntax). *)
let instr ?(fence_nonempty = false) () : Instr.t G.t =
  G.oneof
    [
      (let+ rd = reg and+ imm = unsigned 20 in
       Lui { rd; imm });
      (let+ rd = reg and+ imm = unsigned 20 in
       Auipc { rd; imm });
      (let+ rd = reg and+ offset = even 21 in
       Jal { rd; offset });
      (let+ rd = reg and+ rs1 = reg and+ offset = signed 12 in
       Jalr { rd; rs1; offset });
      (let+ op = of_table branch_ops
       and+ rs1 = reg
       and+ rs2 = reg
       and+ offset = even 13 in
       Branch { op; rs1; rs2; offset });
      (let+ op = of_table load_ops
       and+ rd = reg
       and+ rs1 = reg
       and+ offset = signed 12 in
       Load { op; rd; rs1; offset });
      (let+ op = of_table store_ops
       and+ rs1 = reg
       and+ rs2 = reg
       and+ offset = signed 12 in
       Store { op; rs1; rs2; offset });
      (let+ op = of_table imm_ops
       and+ rd = reg
       and+ rs1 = reg
       and+ imm = signed 12 in
       Op_imm { op; rd; rs1; imm });
      (let* op = of_table shift_ops in
       let+ rd = reg
       and+ rs1 = reg
       and+ shamt = unsigned (Opcode.shamt_bits op) in
       Shift_imm { op; rd; rs1; shamt });
      (let+ op = of_table reg_ops and+ rd = reg and+ rs1 = reg and+ rs2 = reg in
       Op { op; rd; rs1; rs2 });
      (let+ op = of_table unary_ops and+ rd = reg and+ rs1 = reg in
       Unary { op; rd; rs1 });
      (let lo = if fence_nonempty then 1 else 0 in
       let+ pred = G.int_range lo 15 and+ succ = G.int_range lo 15 in
       Fence { pred; succ });
      G.oneof_list [ Fence_i; Ecall; Ebreak ];
      (let+ op = of_table csr_ops
       and+ rd = reg
       and+ src =
         G.oneof
           [ G.map (fun r -> Rs1 r) reg; G.map (fun u -> Uimm u) (unsigned 5) ]
       and+ csr = unsigned 12 in
       Csr { op; rd; src; csr });
      (let+ op = of_table amo_ops
       and+ width = of_table widths
       and+ aq = G.bool
       and+ rl = G.bool
       and+ rd = reg
       and+ rs1 = reg
       and+ rs2 = reg in
       Amo
         {
           op;
           width;
           aq;
           rl;
           rd;
           rs1;
           rs2 = (if op = Lr then Reg.zero else rs2);
         });
      (let+ fmt = fmt and+ rd = freg and+ rs1 = reg and+ offset = signed 12 in
       Fload { fmt; rd; rs1; offset });
      (let+ fmt = fmt and+ rs1 = reg and+ rs2 = freg and+ offset = signed 12 in
       Fstore { fmt; rs1; rs2; offset });
      (let+ op = of_table farith_ops
       and+ fmt = fmt
       and+ rd = freg
       and+ rs1 = freg
       and+ rs2 = freg
       and+ rm = rm in
       Farith { op; fmt; rd; rs1; rs2; rm });
      (let+ fmt = fmt and+ rd = freg and+ rs1 = freg and+ rm = rm in
       Fsqrt { fmt; rd; rs1; rm });
      (let+ op = of_table fsign_ops
       and+ fmt = fmt
       and+ rd = freg
       and+ rs1 = freg
       and+ rs2 = freg in
       Fsign { op; fmt; rd; rs1; rs2 });
      (let+ op = of_table fma_ops
       and+ fmt = fmt
       and+ rd = freg
       and+ rs1 = freg
       and+ rs2 = freg
       and+ rs3 = freg
       and+ rm = rm in
       Fma { op; fmt; rd; rs1; rs2; rs3; rm });
      (let+ op = of_table fcmp_ops
       and+ fmt = fmt
       and+ rd = reg
       and+ rs1 = freg
       and+ rs2 = freg in
       Fcmp { op; fmt; rd; rs1; rs2 });
      (let+ fmt = fmt and+ rd = reg and+ rs1 = freg in
       Fclass { fmt; rd; rs1 });
      (let+ fmt = fmt
       and+ ity = of_table int_tys
       and+ rd = reg
       and+ rs1 = freg
       and+ rm = rm in
       Fcvt_to_int { fmt; ity; rd; rs1; rm });
      (let+ fmt = fmt
       and+ ity = of_table int_tys
       and+ rd = freg
       and+ rs1 = reg
       and+ rm = rm in
       Fcvt_of_int { fmt; ity; rd; rs1; rm });
      (let+ fmt = fmt and+ rd = freg and+ rs1 = freg and+ rm = rm in
       Fcvt_fmt { fmt; rd; rs1; rm });
      (let+ fmt = fmt and+ rd = reg and+ rs1 = freg in
       Fmv_to_int { fmt; rd; rs1 });
      (let+ fmt = fmt and+ rd = freg and+ rs1 = reg in
       Fmv_of_int { fmt; rd; rs1 });
    ]

(** Instructions biased toward RVC eligibility: x8..x15, sp, and small, aligned
    immediates. *)
let compressible : Instr.t G.t =
  let creg =
    G.oneof_weighted
      [
        (4, G.map Reg.of_int (G.int_range 8 15)); (1, G.return Reg.sp); (2, reg);
      ]
  in
  let cfreg =
    G.oneof_weighted [ (4, G.map Freg.of_int (G.int_range 8 15)); (1, freg) ]
  in
  let small =
    G.oneof_weighted [ (4, signed 6); (1, signed 10); (1, signed 12) ]
  in
  let scaled k = G.map (fun v -> v * k) (G.int_range 0 70) in
  G.oneof
    [
      (let+ op = G.oneof_list [ Addi; Addiw; Andi ]
       and+ rd = creg
       and+ rs1 = creg
       and+ imm = small in
       let rs1 = if imm land 1 = 0 then rd else rs1 in
       Op_imm { op; rd; rs1; imm });
      (let+ rd = creg and+ imm = small in
       Op_imm { op = Addi; rd; rs1 = Reg.zero; imm });
      (let+ rd = creg and+ imm = G.map (fun v -> v * 16) (signed 6) in
       Op_imm { op = Addi; rd; rs1 = Reg.sp; imm });
      (let+ rd = creg and+ imm = G.map (fun v -> v * 4) (G.int_range 0 260) in
       Op_imm { op = Addi; rd; rs1 = Reg.sp; imm });
      (let+ rd = creg and+ v = signed 6 in
       Lui { rd; imm = v land 0xfffff });
      (let+ op = G.oneof_list [ Slli; Srli; Srai ]
       and+ rd = creg
       and+ shamt = unsigned 6 in
       Shift_imm { op; rd; rs1 = rd; shamt });
      (let+ op = G.oneof_list [ Add; Sub; Xor; Or; And; Subw; Addw ]
       and+ rd = creg
       and+ rs2 = creg
       and+ form = G.int_range 0 2 in
       let rs1, rs2 =
         match form with 0 -> (rd, rs2) | 1 -> (rs2, rd) | _ -> (Reg.zero, rs2)
       in
       Op { op; rd; rs1; rs2 });
      (let+ offset = even 12 in
       Jal { rd = Reg.zero; offset });
      (let+ rd = G.oneof_list [ Reg.zero; Reg.ra ] and+ rs1 = creg in
       Jalr { rd; rs1; offset = 0 });
      (let+ op = G.oneof_list [ Beq; Bne ]
       and+ rs1 = creg
       and+ offset = even 9 in
       Branch { op; rs1; rs2 = Reg.zero; offset });
      (let+ op = G.oneof_list [ Lw; Ld ]
       and+ rd = creg
       and+ rs1 = creg
       and+ off = scaled 4 in
       Load { op; rd; rs1; offset = (if op = Ld then 2 * off else off) });
      (let+ op = G.oneof_list [ Sw; Sd ]
       and+ rs2 = creg
       and+ rs1 = creg
       and+ off = scaled 4 in
       Store { op; rs1; rs2; offset = (if op = Sd then 2 * off else off) });
      (let+ rd = cfreg and+ rs1 = creg and+ off = scaled 8 in
       Fload { fmt = D; rd; rs1; offset = off });
      (let+ rs2 = cfreg and+ rs1 = creg and+ off = scaled 8 in
       Fstore { fmt = D; rs1; rs2; offset = off });
      G.return Ebreak;
    ]

let print = Instr.to_string
