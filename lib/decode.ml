(** 32-bit instruction decoding: the inverse of {!Encode}. *)

open Instr

(** [field w ~hi ~lo] extracts bits [hi..lo] of [w], unsigned. *)
let field w ~hi ~lo = (w lsr lo) land ((1 lsl (hi - lo + 1)) - 1)

(** Sign-extend the low [bits] bits of [v]. *)
let sign_extend ~bits v =
  let shift = Sys.int_size - bits in
  (v lsl shift) asr shift

let imm_i w = sign_extend ~bits:12 (field w ~hi:31 ~lo:20)

let imm_s w =
  sign_extend ~bits:12 ((field w ~hi:31 ~lo:25 lsl 5) lor field w ~hi:11 ~lo:7)

let imm_b w =
  sign_extend ~bits:13
    ((field w ~hi:31 ~lo:31 lsl 12)
    lor (field w ~hi:7 ~lo:7 lsl 11)
    lor (field w ~hi:30 ~lo:25 lsl 5)
    lor (field w ~hi:11 ~lo:8 lsl 1))

let imm_j w =
  sign_extend ~bits:21
    ((field w ~hi:31 ~lo:31 lsl 20)
    lor (field w ~hi:19 ~lo:12 lsl 12)
    lor (field w ~hi:20 ~lo:20 lsl 11)
    lor (field w ~hi:30 ~lo:21 lsl 1))

let ( let* ) = Option.bind
let ( let+ ) o f = Option.map f o
let guard c = if c then Some () else None

(** Decode a 32-bit instruction word (low 32 bits of [w]). [None] means an
    illegal or unsupported encoding. *)
let decode (w : int) : Instr.t option =
  let w = w land 0xffff_ffff in
  let opcode = field w ~hi:6 ~lo:0 in
  let rd_n = field w ~hi:11 ~lo:7
  and rs1_n = field w ~hi:19 ~lo:15
  and rs2_n = field w ~hi:24 ~lo:20 in
  let rd = Reg.of_int rd_n
  and rs1 = Reg.of_int rs1_n
  and rs2 = Reg.of_int rs2_n in
  let frd = Freg.of_int rd_n
  and frs1 = Freg.of_int rs1_n
  and frs2 = Freg.of_int rs2_n in
  let funct3 = field w ~hi:14 ~lo:12 in
  let funct7 = field w ~hi:31 ~lo:25 in
  let imm12 = field w ~hi:31 ~lo:20 in
  let lookup = Opcode.lookup in
  let fmt_of code = lookup Opcode.fmt_code code in
  let rm_of code = lookup Opcode.rm_code code in
  (* Integer instructions on OP-IMM / OP-IMM-32 / OP / OP-32. *)
  let unary () =
    let+ op = lookup Opcode.unary_funct (opcode, funct3, imm12) in
    Unary { op; rd; rs1 }
  in
  let shift () =
    List.find_map
      (fun (op, (opc, f3, high, sbits)) ->
        if opc = opcode && f3 = funct3 && imm12 lsr sbits = high then
          Some
            (Shift_imm { op; rd; rs1; shamt = imm12 land ((1 lsl sbits) - 1) })
        else None)
      Opcode.shift_funct
  in
  let int_alu () =
    match unary () with
    | Some _ as u -> u
    | None when opcode = Opcode.op || opcode = Opcode.op_32 ->
        let+ op = lookup Opcode.reg_funct (opcode, funct3, funct7) in
        Op { op; rd; rs1; rs2 }
    | None when funct3 = 1 || funct3 = 5 -> shift ()
    | None ->
        let+ op = lookup Opcode.imm_funct (opcode, funct3) in
        Op_imm { op; rd; rs1; imm = imm_i w }
  in
  let op_fp () =
    let funct5 = funct7 lsr 2 in
    let* fmt = fmt_of (funct7 land 3) in
    let with_rm k =
      let* rm = rm_of funct3 in
      k rm
    in
    match funct5 with
    | 0x00 | 0x01 | 0x02 | 0x03 ->
        let* op = lookup Opcode.farith_funct5 funct5 in
        with_rm (fun rm ->
            Some (Farith { op; fmt; rd = frd; rs1 = frs1; rs2 = frs2; rm }))
    | 0x0b ->
        let* () = guard (rs2_n = 0) in
        with_rm (fun rm -> Some (Fsqrt { fmt; rd = frd; rs1 = frs1; rm }))
    | 0x04 | 0x05 ->
        let+ op = lookup Opcode.fsign_funct (funct5, funct3) in
        Fsign { op; fmt; rd = frd; rs1 = frs1; rs2 = frs2 }
    | 0x14 ->
        let+ op = lookup Opcode.fcmp_funct3 funct3 in
        Fcmp { op; fmt; rd; rs1 = frs1; rs2 = frs2 }
    | 0x1c when rs2_n = 0 && funct3 = 1 -> Some (Fclass { fmt; rd; rs1 = frs1 })
    | 0x1c when rs2_n = 0 && funct3 = 0 ->
        Some (Fmv_to_int { fmt; rd; rs1 = frs1 })
    | 0x1e when rs2_n = 0 && funct3 = 0 ->
        Some (Fmv_of_int { fmt; rd = frd; rs1 })
    | 0x18 ->
        let* ity = lookup Opcode.int_ty_code rs2_n in
        with_rm (fun rm -> Some (Fcvt_to_int { fmt; ity; rd; rs1 = frs1; rm }))
    | 0x1a ->
        let* ity = lookup Opcode.int_ty_code rs2_n in
        with_rm (fun rm -> Some (Fcvt_of_int { fmt; ity; rd = frd; rs1; rm }))
    | 0x08 ->
        let* src = fmt_of rs2_n in
        let* () = guard (src <> fmt) in
        with_rm (fun rm -> Some (Fcvt_fmt { fmt; rd = frd; rs1 = frs1; rm }))
    | _ -> None
  in
  if opcode = Opcode.lui then Some (Lui { rd; imm = field w ~hi:31 ~lo:12 })
  else if opcode = Opcode.auipc then
    Some (Auipc { rd; imm = field w ~hi:31 ~lo:12 })
  else if opcode = Opcode.jal then Some (Jal { rd; offset = imm_j w })
  else if opcode = Opcode.jalr then
    let+ () = guard (funct3 = 0) in
    Jalr { rd; rs1; offset = imm_i w }
  else if opcode = Opcode.branch then
    let+ op = lookup Opcode.branch_funct3 funct3 in
    Branch { op; rs1; rs2; offset = imm_b w }
  else if opcode = Opcode.load then
    let+ op = lookup Opcode.load_funct3 funct3 in
    Load { op; rd; rs1; offset = imm_i w }
  else if opcode = Opcode.store then
    let+ op = lookup Opcode.store_funct3 funct3 in
    Store { op; rs1; rs2; offset = imm_s w }
  else if
    opcode = Opcode.op_imm || opcode = Opcode.op_imm_32 || opcode = Opcode.op
    || opcode = Opcode.op_32
  then int_alu ()
  else if opcode = Opcode.misc_mem then
    match funct3 with
    | 0 ->
        Some
          (Fence { pred = field w ~hi:27 ~lo:24; succ = field w ~hi:23 ~lo:20 })
    | 1 -> Some Fence_i
    | _ -> None
  else if opcode = Opcode.system then
    match funct3 with
    | 0 when w = 0x00000073 -> Some Ecall
    | 0 when w = 0x00100073 -> Some Ebreak
    | 1 | 2 | 3 | 5 | 6 | 7 ->
        let+ op = lookup Opcode.csr_funct3 (funct3 land 3) in
        let src = if funct3 >= 4 then Uimm rs1_n else Rs1 rs1 in
        Csr { op; rd; src; csr = imm12 }
    | _ -> None
  else if opcode = Opcode.amo then
    let* width = lookup Opcode.width_funct3 funct3 in
    let* op = lookup Opcode.amo_funct5 (funct7 lsr 2) in
    let+ () = guard (op <> Lr || rs2_n = 0) in
    let aq = funct7 land 2 <> 0 and rl = funct7 land 1 <> 0 in
    Amo { op; width; aq; rl; rd; rs1; rs2 }
  else if opcode = Opcode.load_fp then
    let+ fmt = lookup Opcode.fp_width_funct3 funct3 in
    Fload { fmt; rd = frd; rs1; offset = imm_i w }
  else if opcode = Opcode.store_fp then
    let+ fmt = lookup Opcode.fp_width_funct3 funct3 in
    Fstore { fmt; rs1; rs2 = frs2; offset = imm_s w }
  else if opcode = Opcode.op_fp then op_fp ()
  else
    match lookup Opcode.fma_opcode opcode with
    | Some op ->
        let* fmt = fmt_of (funct7 land 3) in
        let+ rm = rm_of funct3 in
        let rs3 = Freg.of_int (funct7 lsr 2) in
        Fma { op; fmt; rd = frd; rs1 = frs1; rs2 = frs2; rs3; rm }
    | None -> None
