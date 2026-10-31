(** 32-bit instruction encoding. *)

open Instr

let ( let* ) = Result.bind

let check_signed ~bits ~what v =
  let lo = -(1 lsl (bits - 1)) and hi = (1 lsl (bits - 1)) - 1 in
  if v < lo || v > hi then
    Error (Printf.sprintf "%s %d out of range [%d, %d]" what v lo hi)
  else Ok ()

let check_unsigned ~bits ~what v =
  if v < 0 || v >= 1 lsl bits then
    Error
      (Printf.sprintf "%s %d out of range [0, %d]" what v ((1 lsl bits) - 1))
  else Ok ()

let check_even ~what v =
  if v land 1 <> 0 then Error (Printf.sprintf "%s %d is not even" what v)
  else Ok ()

let x = Reg.to_int
let f = Freg.to_int
let bits v ~hi ~lo = (v lsr lo) land ((1 lsl (hi - lo + 1)) - 1)

(* Instruction formats. Register operands are given as plain ints so the
   same builders serve integer and FP registers. Fields must already be
   range-checked. *)

let r_type ~opcode ~funct3 ~funct7 ~rd ~rs1 ~rs2 =
  (funct7 lsl 25) lor (rs2 lsl 20) lor (rs1 lsl 15) lor (funct3 lsl 12)
  lor (rd lsl 7) lor opcode

let r4_type ~opcode ~funct3 ~funct2 ~rd ~rs1 ~rs2 ~rs3 =
  r_type ~opcode ~funct3 ~funct7:((rs3 lsl 2) lor funct2) ~rd ~rs1 ~rs2

let i_type ~opcode ~funct3 ~rd ~rs1 ~imm =
  (bits imm ~hi:11 ~lo:0 lsl 20)
  lor (rs1 lsl 15) lor (funct3 lsl 12) lor (rd lsl 7) lor opcode

let s_type ~opcode ~funct3 ~rs1 ~rs2 ~imm =
  (bits imm ~hi:11 ~lo:5 lsl 25)
  lor (rs2 lsl 20) lor (rs1 lsl 15) lor (funct3 lsl 12)
  lor (bits imm ~hi:4 ~lo:0 lsl 7)
  lor opcode

let b_type ~opcode ~funct3 ~rs1 ~rs2 ~imm =
  (bits imm ~hi:12 ~lo:12 lsl 31)
  lor (bits imm ~hi:10 ~lo:5 lsl 25)
  lor (rs2 lsl 20) lor (rs1 lsl 15) lor (funct3 lsl 12)
  lor (bits imm ~hi:4 ~lo:1 lsl 8)
  lor (bits imm ~hi:11 ~lo:11 lsl 7)
  lor opcode

let u_type ~opcode ~rd ~imm = (imm lsl 12) lor (rd lsl 7) lor opcode

let j_type ~opcode ~rd ~imm =
  (bits imm ~hi:20 ~lo:20 lsl 31)
  lor (bits imm ~hi:10 ~lo:1 lsl 21)
  lor (bits imm ~hi:11 ~lo:11 lsl 20)
  lor (bits imm ~hi:19 ~lo:12 lsl 12)
  lor (rd lsl 7) lor opcode

let i12 = check_signed ~bits:12
let rm_bits rm = List.assoc rm Opcode.rm_code
let fmt_bits fmt = List.assoc fmt Opcode.fmt_code

(** OP-FP instruction with the given funct5. *)
let op_fp ~funct5 ~fmt ~funct3 ~rd ~rs1 ~rs2 =
  r_type ~opcode:Opcode.op_fp ~funct3
    ~funct7:((funct5 lsl 2) lor fmt_bits fmt)
    ~rd ~rs1 ~rs2

let encode_int (i : Instr.t) : (int, string) result =
  match i with
  | Lui { rd; imm } | Auipc { rd; imm } ->
      let* () = check_unsigned ~bits:20 ~what:"upper immediate" imm in
      let opcode = match i with Lui _ -> Opcode.lui | _ -> Opcode.auipc in
      Ok (u_type ~opcode ~rd:(x rd) ~imm)
  | Jal { rd; offset } ->
      let* () = check_signed ~bits:21 ~what:"jump offset" offset in
      let* () = check_even ~what:"jump offset" offset in
      Ok (j_type ~opcode:Opcode.jal ~rd:(x rd) ~imm:offset)
  | Jalr { rd; rs1; offset } ->
      let* () = i12 ~what:"offset" offset in
      Ok
        (i_type ~opcode:Opcode.jalr ~funct3:0 ~rd:(x rd) ~rs1:(x rs1)
           ~imm:offset)
  | Branch { op; rs1; rs2; offset } ->
      let* () = check_signed ~bits:13 ~what:"branch offset" offset in
      let* () = check_even ~what:"branch offset" offset in
      let funct3 = List.assoc op Opcode.branch_funct3 in
      Ok
        (b_type ~opcode:Opcode.branch ~funct3 ~rs1:(x rs1) ~rs2:(x rs2)
           ~imm:offset)
  | Load { op; rd; rs1; offset } ->
      let* () = i12 ~what:"offset" offset in
      let funct3 = List.assoc op Opcode.load_funct3 in
      Ok
        (i_type ~opcode:Opcode.load ~funct3 ~rd:(x rd) ~rs1:(x rs1) ~imm:offset)
  | Store { op; rs1; rs2; offset } ->
      let* () = i12 ~what:"offset" offset in
      let funct3 = List.assoc op Opcode.store_funct3 in
      Ok
        (s_type ~opcode:Opcode.store ~funct3 ~rs1:(x rs1) ~rs2:(x rs2)
           ~imm:offset)
  | Op_imm { op; rd; rs1; imm } ->
      let* () = i12 ~what:"immediate" imm in
      let opcode, funct3 = List.assoc op Opcode.imm_funct in
      Ok (i_type ~opcode ~funct3 ~rd:(x rd) ~rs1:(x rs1) ~imm)
  | Shift_imm { op; rd; rs1; shamt } ->
      let opcode, funct3, high, sbits = List.assoc op Opcode.shift_funct in
      let* () = check_unsigned ~bits:sbits ~what:"shift amount" shamt in
      Ok
        (i_type ~opcode ~funct3 ~rd:(x rd) ~rs1:(x rs1)
           ~imm:((high lsl sbits) lor shamt))
  | Op { op; rd; rs1; rs2 } ->
      let opcode, funct3, funct7 = List.assoc op Opcode.reg_funct in
      Ok (r_type ~opcode ~funct3 ~funct7 ~rd:(x rd) ~rs1:(x rs1) ~rs2:(x rs2))
  | Unary { op; rd; rs1 } ->
      let opcode, funct3, imm = List.assoc op Opcode.unary_funct in
      Ok (i_type ~opcode ~funct3 ~rd:(x rd) ~rs1:(x rs1) ~imm)
  | Fence { pred; succ } ->
      let* () = check_unsigned ~bits:4 ~what:"fence set" pred in
      let* () = check_unsigned ~bits:4 ~what:"fence set" succ in
      Ok ((pred lsl 24) lor (succ lsl 20) lor Opcode.misc_mem)
  | Fence_i -> Ok 0x0000100f
  | Ecall -> Ok 0x00000073
  | Ebreak -> Ok 0x00100073
  | Csr { op; rd; src; csr } ->
      let* () = check_unsigned ~bits:12 ~what:"CSR number" csr in
      let base = List.assoc op Opcode.csr_funct3 in
      let* funct3, rs1 =
        match src with
        | Rs1 r -> Ok (base, x r)
        | Uimm u ->
            let* () = check_unsigned ~bits:5 ~what:"CSR immediate" u in
            Ok (base + 4, u)
      in
      Ok (i_type ~opcode:Opcode.system ~funct3 ~rd:(x rd) ~rs1 ~imm:csr)
  | Amo { op; width; aq; rl; rd; rs1; rs2 } ->
      let funct5 = List.assoc op Opcode.amo_funct5 in
      let funct7 =
        (funct5 lsl 2) lor (Bool.to_int aq lsl 1) lor Bool.to_int rl
      in
      let rs2 = if op = Lr then 0 else x rs2 in
      Ok
        (r_type ~opcode:Opcode.amo
           ~funct3:(List.assoc width Opcode.width_funct3)
           ~funct7 ~rd:(x rd) ~rs1:(x rs1) ~rs2)
  | Fload { fmt; rd; rs1; offset } ->
      let* () = i12 ~what:"offset" offset in
      Ok
        (i_type ~opcode:Opcode.load_fp
           ~funct3:(List.assoc fmt Opcode.fp_width_funct3)
           ~rd:(f rd) ~rs1:(x rs1) ~imm:offset)
  | Fstore { fmt; rs1; rs2; offset } ->
      let* () = i12 ~what:"offset" offset in
      Ok
        (s_type ~opcode:Opcode.store_fp
           ~funct3:(List.assoc fmt Opcode.fp_width_funct3)
           ~rs1:(x rs1) ~rs2:(f rs2) ~imm:offset)
  | Farith { op; fmt; rd; rs1; rs2; rm } ->
      Ok
        (op_fp
           ~funct5:(List.assoc op Opcode.farith_funct5)
           ~fmt ~funct3:(rm_bits rm) ~rd:(f rd) ~rs1:(f rs1) ~rs2:(f rs2))
  | Fsqrt { fmt; rd; rs1; rm } ->
      Ok
        (op_fp ~funct5:Opcode.fsqrt_funct5 ~fmt ~funct3:(rm_bits rm) ~rd:(f rd)
           ~rs1:(f rs1) ~rs2:0)
  | Fsign { op; fmt; rd; rs1; rs2 } ->
      let funct5, funct3 = List.assoc op Opcode.fsign_funct in
      Ok (op_fp ~funct5 ~fmt ~funct3 ~rd:(f rd) ~rs1:(f rs1) ~rs2:(f rs2))
  | Fma { op; fmt; rd; rs1; rs2; rs3; rm } ->
      Ok
        (r4_type
           ~opcode:(List.assoc op Opcode.fma_opcode)
           ~funct3:(rm_bits rm) ~funct2:(fmt_bits fmt) ~rd:(f rd) ~rs1:(f rs1)
           ~rs2:(f rs2) ~rs3:(f rs3))
  | Fcmp { op; fmt; rd; rs1; rs2 } ->
      Ok
        (op_fp ~funct5:Opcode.fcmp_funct5 ~fmt
           ~funct3:(List.assoc op Opcode.fcmp_funct3)
           ~rd:(x rd) ~rs1:(f rs1) ~rs2:(f rs2))
  | Fclass { fmt; rd; rs1 } ->
      Ok
        (op_fp ~funct5:Opcode.fclass_funct5 ~fmt ~funct3:1 ~rd:(x rd)
           ~rs1:(f rs1) ~rs2:0)
  | Fcvt_to_int { fmt; ity; rd; rs1; rm } ->
      Ok
        (op_fp ~funct5:Opcode.fcvt_to_int_funct5 ~fmt ~funct3:(rm_bits rm)
           ~rd:(x rd) ~rs1:(f rs1)
           ~rs2:(List.assoc ity Opcode.int_ty_code))
  | Fcvt_of_int { fmt; ity; rd; rs1; rm } ->
      Ok
        (op_fp ~funct5:Opcode.fcvt_of_int_funct5 ~fmt ~funct3:(rm_bits rm)
           ~rd:(f rd) ~rs1:(x rs1)
           ~rs2:(List.assoc ity Opcode.int_ty_code))
  | Fcvt_fmt { fmt; rd; rs1; rm } ->
      Ok
        (op_fp ~funct5:Opcode.fcvt_fmt_funct5 ~fmt ~funct3:(rm_bits rm)
           ~rd:(f rd) ~rs1:(f rs1)
           ~rs2:(fmt_bits (other_fmt fmt)))
  | Fmv_to_int { fmt; rd; rs1 } ->
      Ok
        (op_fp ~funct5:Opcode.fmv_to_int_funct5 ~fmt ~funct3:0 ~rd:(x rd)
           ~rs1:(f rs1) ~rs2:0)
  | Fmv_of_int { fmt; rd; rs1 } ->
      Ok
        (op_fp ~funct5:Opcode.fmv_of_int_funct5 ~fmt ~funct3:0 ~rd:(f rd)
           ~rs1:(x rs1) ~rs2:0)

(** Encode as a 32-bit word (held in the low 32 bits of an [int]). *)
let encode = encode_int
