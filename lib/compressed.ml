(** The RV64C compressed instruction set.

    [expand] decodes a 16-bit parcel into the base instruction it stands for.
    [compress] goes the other way. It returns a 16-bit encoding whose expansion
    is [i] itself, or is equivalent to it: [addi rd, rs, 0] compresses to
    [c.mv rd, rs], which expands to [add rd, zero, rs], and commutative
    operations with [rd = rs2] are commuted ([or rd, rs, rd] becomes
    [c.or rd, rs]). Both choices match clang. *)

open Instr

let field = Decode.field
let sext = Decode.sign_extend
let bit h n = (h lsr n) land 1

(** [true] if a 16-bit parcel's low two bits mark it as compressed. *)
let is_compressed parcel = parcel land 3 <> 3

let ( let* ) = Option.bind
let guard c = if c then Some () else None

(* {1 Expansion} *)

let expand (h : int) : Instr.t option =
  let h = h land 0xffff in
  let funct3 = field h ~hi:15 ~lo:13 in
  let r n = Reg.of_int n and fr n = Freg.of_int n in
  (* 3-bit "prime" register fields name x8..x15 *)
  let r' hi lo = r (8 + field h ~hi ~lo) in
  let fr' hi lo = fr (8 + field h ~hi ~lo) in
  let rd_full = field h ~hi:11 ~lo:7 and rs2_full = field h ~hi:6 ~lo:2 in
  let imm6 = sext ~bits:6 ((bit h 12 lsl 5) lor field h ~hi:6 ~lo:2) in
  let uimm6 = (bit h 12 lsl 5) lor field h ~hi:6 ~lo:2 in
  (* scaled offsets for the register-based loads/stores *)
  let off_w =
    (field h ~hi:12 ~lo:10 lsl 3) lor (bit h 6 lsl 2) lor (bit h 5 lsl 6)
  in
  let off_d = (field h ~hi:12 ~lo:10 lsl 3) lor (field h ~hi:6 ~lo:5 lsl 6) in
  match (h land 3, funct3) with
  (* Quadrant 0 *)
  | 0, 0b000 ->
      let nzuimm =
        (field h ~hi:12 ~lo:11 lsl 4)
        lor (field h ~hi:10 ~lo:7 lsl 6)
        lor (bit h 6 lsl 2)
        lor (bit h 5 lsl 3)
      in
      let* () = guard (nzuimm <> 0) in
      Some (Op_imm { op = Addi; rd = r' 4 2; rs1 = Reg.sp; imm = nzuimm })
  | 0, 0b001 ->
      Some (Fload { fmt = D; rd = fr' 4 2; rs1 = r' 9 7; offset = off_d })
  | 0, 0b010 ->
      Some (Load { op = Lw; rd = r' 4 2; rs1 = r' 9 7; offset = off_w })
  | 0, 0b011 ->
      Some (Load { op = Ld; rd = r' 4 2; rs1 = r' 9 7; offset = off_d })
  | 0, 0b101 ->
      Some (Fstore { fmt = D; rs1 = r' 9 7; rs2 = fr' 4 2; offset = off_d })
  | 0, 0b110 ->
      Some (Store { op = Sw; rs1 = r' 9 7; rs2 = r' 4 2; offset = off_w })
  | 0, 0b111 ->
      Some (Store { op = Sd; rs1 = r' 9 7; rs2 = r' 4 2; offset = off_d })
  (* Quadrant 1 *)
  | 1, 0b000 ->
      Some (Op_imm { op = Addi; rd = r rd_full; rs1 = r rd_full; imm = imm6 })
  | 1, 0b001 ->
      let* () = guard (rd_full <> 0) in
      Some (Op_imm { op = Addiw; rd = r rd_full; rs1 = r rd_full; imm = imm6 })
  | 1, 0b010 ->
      Some (Op_imm { op = Addi; rd = r rd_full; rs1 = Reg.zero; imm = imm6 })
  | 1, 0b011 when rd_full = 2 ->
      let nzimm =
        sext ~bits:10
          ((bit h 12 lsl 9)
          lor (bit h 6 lsl 4)
          lor (bit h 5 lsl 6)
          lor (field h ~hi:4 ~lo:3 lsl 7)
          lor (bit h 2 lsl 5))
      in
      let* () = guard (nzimm <> 0) in
      Some (Op_imm { op = Addi; rd = Reg.sp; rs1 = Reg.sp; imm = nzimm })
  | 1, 0b011 ->
      let* () = guard (imm6 <> 0) in
      Some (Lui { rd = r rd_full; imm = imm6 land 0xfffff })
  | 1, 0b100 -> (
      let rd = r' 9 7 in
      match field h ~hi:11 ~lo:10 with
      | 0b00 -> Some (Shift_imm { op = Srli; rd; rs1 = rd; shamt = uimm6 })
      | 0b01 -> Some (Shift_imm { op = Srai; rd; rs1 = rd; shamt = uimm6 })
      | 0b10 -> Some (Op_imm { op = Andi; rd; rs1 = rd; imm = imm6 })
      | _ ->
          let* op =
            match (bit h 12, field h ~hi:6 ~lo:5) with
            | 0, 0b00 -> Some Sub
            | 0, 0b01 -> Some Xor
            | 0, 0b10 -> Some Or
            | 0, 0b11 -> Some And
            | 1, 0b00 -> Some Subw
            | 1, 0b01 -> Some Addw
            | _ -> None
          in
          Some (Op { op; rd; rs1 = rd; rs2 = r' 4 2 }))
  | 1, 0b101 ->
      let offset =
        sext ~bits:12
          ((bit h 12 lsl 11)
          lor (bit h 11 lsl 4)
          lor (field h ~hi:10 ~lo:9 lsl 8)
          lor (bit h 8 lsl 10)
          lor (bit h 7 lsl 6)
          lor (bit h 6 lsl 7)
          lor (field h ~hi:5 ~lo:3 lsl 1)
          lor (bit h 2 lsl 5))
      in
      Some (Jal { rd = Reg.zero; offset })
  | 1, (0b110 | 0b111) ->
      let offset =
        sext ~bits:9
          ((bit h 12 lsl 8)
          lor (field h ~hi:11 ~lo:10 lsl 3)
          lor (field h ~hi:6 ~lo:5 lsl 6)
          lor (field h ~hi:4 ~lo:3 lsl 1)
          lor (bit h 2 lsl 5))
      in
      let op = if funct3 = 0b110 then Beq else Bne in
      Some (Branch { op; rs1 = r' 9 7; rs2 = Reg.zero; offset })
  (* Quadrant 2 *)
  | 2, 0b000 ->
      Some
        (Shift_imm { op = Slli; rd = r rd_full; rs1 = r rd_full; shamt = uimm6 })
  | 2, (0b001 | 0b011) ->
      let offset =
        (bit h 12 lsl 5)
        lor (field h ~hi:6 ~lo:5 lsl 3)
        lor (field h ~hi:4 ~lo:2 lsl 6)
      in
      if funct3 = 0b001 then
        Some (Fload { fmt = D; rd = fr rd_full; rs1 = Reg.sp; offset })
      else
        let* () = guard (rd_full <> 0) in
        Some (Load { op = Ld; rd = r rd_full; rs1 = Reg.sp; offset })
  | 2, 0b010 ->
      let* () = guard (rd_full <> 0) in
      let offset =
        (bit h 12 lsl 5)
        lor (field h ~hi:6 ~lo:4 lsl 2)
        lor (field h ~hi:3 ~lo:2 lsl 6)
      in
      Some (Load { op = Lw; rd = r rd_full; rs1 = Reg.sp; offset })
  | 2, 0b100 -> (
      match (bit h 12, rd_full, rs2_full) with
      | 0, 0, 0 -> None
      | 0, rs1, 0 -> Some (Jalr { rd = Reg.zero; rs1 = r rs1; offset = 0 })
      | 0, rd, rs2 ->
          Some (Op { op = Add; rd = r rd; rs1 = Reg.zero; rs2 = r rs2 })
      | _, 0, 0 -> Some Ebreak
      | _, rs1, 0 -> Some (Jalr { rd = Reg.ra; rs1 = r rs1; offset = 0 })
      | _, rd, rs2 -> Some (Op { op = Add; rd = r rd; rs1 = r rd; rs2 = r rs2 })
      )
  | 2, (0b101 | 0b111) ->
      let offset =
        (field h ~hi:12 ~lo:10 lsl 3) lor (field h ~hi:9 ~lo:7 lsl 6)
      in
      if funct3 = 0b101 then
        Some (Fstore { fmt = D; rs1 = Reg.sp; rs2 = fr rs2_full; offset })
      else Some (Store { op = Sd; rs1 = Reg.sp; rs2 = r rs2_full; offset })
  | 2, 0b110 ->
      let offset =
        (field h ~hi:12 ~lo:9 lsl 2) lor (field h ~hi:8 ~lo:7 lsl 6)
      in
      Some (Store { op = Sw; rs1 = Reg.sp; rs2 = r rs2_full; offset })
  | _ -> None

(* {1 Compression} *)

let bits v ~hi ~lo = (v lsr lo) land ((1 lsl (hi - lo + 1)) - 1)
let fits ~bits v = v >= -(1 lsl (bits - 1)) && v < 1 lsl (bits - 1)
let aligned n v = v land (n - 1) = 0
let in_range lo hi v = lo <= v && v < hi

(** Index into x8..x15, if [r] is one of them. *)
let prime n = if n >= 8 && n <= 15 then Some (n - 8) else None

let q ~op ~funct3 = (funct3 lsl 13) lor op

(* CI-format: imm[5] at bit 12, imm[4:0] at 6:2 *)
let ci ~op ~funct3 ~rd imm =
  q ~op ~funct3
  lor (bits imm ~hi:5 ~lo:5 lsl 12)
  lor (rd lsl 7)
  lor (bits imm ~hi:4 ~lo:0 lsl 2)

let rec compress (i : Instr.t) : int option =
  let x = Reg.to_int and f = Freg.to_int in
  match i with
  | Op_imm { op = Addi; rd; rs1; imm } -> (
      let rd = x rd and rs1 = x rs1 in
      match prime rd with
      | _ when rd = 0 && rs1 = 0 && imm = 0 -> Some 0x0001 (* c.nop *)
      | Some rd' when rs1 = 2 && in_range 1 1024 imm && aligned 4 imm ->
          Some
            (q ~op:0 ~funct3:0
            lor (bits imm ~hi:5 ~lo:4 lsl 11)
            lor (bits imm ~hi:9 ~lo:6 lsl 7)
            lor (bits imm ~hi:2 ~lo:2 lsl 6)
            lor (bits imm ~hi:3 ~lo:3 lsl 5)
            lor (rd' lsl 2))
      | _ when rd = rs1 && rd <> 0 && imm <> 0 && fits ~bits:6 imm ->
          Some (ci ~op:1 ~funct3:0b000 ~rd imm)
      | _
        when rd = 2 && rs1 = 2 && imm <> 0 && fits ~bits:10 imm
             && aligned 16 imm ->
          Some
            (q ~op:1 ~funct3:0b011
            lor (bits imm ~hi:9 ~lo:9 lsl 12)
            lor (2 lsl 7)
            lor (bits imm ~hi:4 ~lo:4 lsl 6)
            lor (bits imm ~hi:6 ~lo:6 lsl 5)
            lor (bits imm ~hi:8 ~lo:7 lsl 3)
            lor (bits imm ~hi:5 ~lo:5 lsl 2))
      | _ when rs1 = 0 && rd <> 0 && fits ~bits:6 imm ->
          Some (ci ~op:1 ~funct3:0b010 ~rd imm)
      | _ when imm = 0 && rd <> 0 && rs1 <> 0 ->
          Some (q ~op:2 ~funct3:0b100 lor (rd lsl 7) lor (rs1 lsl 2)) (* c.mv *)
      | _ -> None)
  | Op_imm { op = Addiw; rd; rs1; imm }
    when x rd = x rs1 && x rd <> 0 && fits ~bits:6 imm ->
      Some (ci ~op:1 ~funct3:0b001 ~rd:(x rd) imm)
  | Op_imm { op = Andi; rd; rs1; imm } when x rd = x rs1 && fits ~bits:6 imm ->
      let* rd' = prime (x rd) in
      Some (ci ~op:1 ~funct3:0b100 ~rd:((0b10 lsl 3) lor rd') imm)
      (* funct2 = 10 *)
  | Lui { rd; imm } ->
      let v = sext ~bits:20 imm in
      let* () = guard (x rd <> 0 && x rd <> 2 && v <> 0 && fits ~bits:6 v) in
      Some (ci ~op:1 ~funct3:0b011 ~rd:(x rd) v)
  | Shift_imm { op = Slli; rd; rs1; shamt }
    when x rd = x rs1 && x rd <> 0 && shamt <> 0 ->
      Some (ci ~op:2 ~funct3:0b000 ~rd:(x rd) shamt)
  | Shift_imm { op = (Srli | Srai) as op; rd; rs1; shamt }
    when x rd = x rs1 && shamt <> 0 ->
      let* rd' = prime (x rd) in
      let funct2 = if op = Srli then 0b00 else 0b01 in
      Some (ci ~op:1 ~funct3:0b100 ~rd:((funct2 lsl 3) lor rd') shamt)
  | Op { op = Add; rd; rs1; rs2 } when x rd <> 0 && x rs2 <> 0 ->
      if x rs1 = x rd then
        Some
          (q ~op:2 ~funct3:0b100 lor (1 lsl 12)
          lor (x rd lsl 7)
          lor (x rs2 lsl 2))
      else if x rs2 = x rd && x rs1 <> 0 then (* commuted: add rd, rs1, rd *)
        Some
          (q ~op:2 ~funct3:0b100 lor (1 lsl 12)
          lor (x rd lsl 7)
          lor (x rs1 lsl 2))
      else if x rs1 = 0 then
        Some (q ~op:2 ~funct3:0b100 lor (x rd lsl 7) lor (x rs2 lsl 2))
      else None
  | Op { op = (Xor | Or | And | Addw) as op; rd; rs1; rs2 }
    when x rd = x rs2 && x rd <> x rs1 ->
      (* commutative: [op rd, rs1, rd] = [op rd, rd, rs1] *)
      compress (Op { op; rd; rs1 = rs2; rs2 = rs1 })
  | Op { op = (Sub | Xor | Or | And | Subw | Addw) as op; rd; rs1; rs2 }
    when x rd = x rs1 ->
      let* rd' = prime (x rd) in
      let* rs2' = prime (x rs2) in
      let b12, funct2 =
        match op with
        | Sub -> (0, 0) | Xor -> (0, 1) | Or -> (0, 2) | And -> (0, 3)
        | Subw -> (1, 0) | _ (* Addw *) -> (1, 1)
        [@@ocamlformat "disable"]
      in
      Some
        (q ~op:1 ~funct3:0b100 lor (b12 lsl 12) lor (0b11 lsl 10)
       lor (rd' lsl 7) lor (funct2 lsl 5) lor (rs2' lsl 2))
  | Jal { rd; offset } when x rd = 0 && fits ~bits:12 offset && aligned 2 offset
    ->
      let o = offset in
      Some
        (q ~op:1 ~funct3:0b101
        lor (bits o ~hi:11 ~lo:11 lsl 12)
        lor (bits o ~hi:4 ~lo:4 lsl 11)
        lor (bits o ~hi:9 ~lo:8 lsl 9)
        lor (bits o ~hi:10 ~lo:10 lsl 8)
        lor (bits o ~hi:6 ~lo:6 lsl 7)
        lor (bits o ~hi:7 ~lo:7 lsl 6)
        lor (bits o ~hi:3 ~lo:1 lsl 3)
        lor (bits o ~hi:5 ~lo:5 lsl 2))
  | Jalr { rd; rs1; offset = 0 } when x rs1 <> 0 && (x rd = 0 || x rd = 1) ->
      Some (q ~op:2 ~funct3:0b100 lor (x rd lsl 12) lor (x rs1 lsl 7))
  | Ebreak -> Some 0x9002
  | Branch { op = (Beq | Bne) as op; rs1; rs2; offset }
    when x rs2 = 0 && fits ~bits:9 offset && aligned 2 offset ->
      let* rs1' = prime (x rs1) in
      let o = offset in
      Some
        (q ~op:1 ~funct3:(if op = Beq then 0b110 else 0b111)
        lor (bits o ~hi:8 ~lo:8 lsl 12)
        lor (bits o ~hi:4 ~lo:3 lsl 10)
        lor (rs1' lsl 7)
        lor (bits o ~hi:7 ~lo:6 lsl 5)
        lor (bits o ~hi:2 ~lo:1 lsl 3)
        lor (bits o ~hi:5 ~lo:5 lsl 2))
  | Load { op = (Lw | Ld) as op; rd; rs1; offset } -> (
      let scale = if op = Lw then 4 else 8 in
      match (prime (x rd), prime (x rs1)) with
      | Some rd', Some rs1'
        when aligned scale offset && in_range 0 (32 * scale) offset ->
          Some
            (cl_cs
               ~op:(if op = Lw then 0b010 else 0b011)
               ~scale ~rs1' ~r2':rd' offset)
      | _
        when x rs1 = 2
             && x rd <> 0
             && aligned scale offset
             && in_range 0 (64 * scale) offset ->
          Some
            (ci_sp
               ~funct3:(if op = Lw then 0b010 else 0b011)
               ~scale ~rd:(x rd) offset)
      | _ -> None)
  | Fload { fmt = D; rd; rs1; offset } -> (
      match (prime (f rd), prime (x rs1)) with
      | Some rd', Some rs1' when aligned 8 offset && in_range 0 256 offset ->
          Some (cl_cs ~op:0b001 ~scale:8 ~rs1' ~r2':rd' offset)
      | _ when x rs1 = 2 && aligned 8 offset && in_range 0 512 offset ->
          Some (ci_sp ~funct3:0b001 ~scale:8 ~rd:(f rd) offset)
      | _ -> None)
  | Store { op = (Sw | Sd) as op; rs1; rs2; offset } -> (
      let scale = if op = Sw then 4 else 8 in
      match (prime (x rs2), prime (x rs1)) with
      | Some rs2', Some rs1'
        when aligned scale offset && in_range 0 (32 * scale) offset ->
          Some
            (cl_cs
               ~op:(if op = Sw then 0b110 else 0b111)
               ~scale ~rs1' ~r2':rs2' offset)
      | _
        when x rs1 = 2 && aligned scale offset && in_range 0 (64 * scale) offset
        ->
          Some
            (css
               ~funct3:(if op = Sw then 0b110 else 0b111)
               ~scale ~rs2:(x rs2) offset)
      | _ -> None)
  | Fstore { fmt = D; rs1; rs2; offset } -> (
      match (prime (f rs2), prime (x rs1)) with
      | Some rs2', Some rs1' when aligned 8 offset && in_range 0 256 offset ->
          Some (cl_cs ~op:0b101 ~scale:8 ~rs1' ~r2':rs2' offset)
      | _ when x rs1 = 2 && aligned 8 offset && in_range 0 512 offset ->
          Some (css ~funct3:0b101 ~scale:8 ~rs2:(f rs2) offset)
      | _ -> None)
  | _ -> None

(* Quadrant-0 loads/stores (CL/CS): offset[5:3] at 12:10; for words
   offset[2] at 6 and offset[6] at 5, for doublewords offset[7:6] at 6:5. *)
and cl_cs ~op ~scale ~rs1' ~r2' o =
  let low =
    if scale = 4 then (bits o ~hi:2 ~lo:2 lsl 6) lor (bits o ~hi:6 ~lo:6 lsl 5)
    else bits o ~hi:7 ~lo:6 lsl 5
  in
  q ~op:0 ~funct3:op
  lor (bits o ~hi:5 ~lo:3 lsl 10)
  lor (rs1' lsl 7) lor low lor (r2' lsl 2)

(* sp-relative loads (CI): offset[5] at 12, then [4:2|7:6] (words) or
   [4:3|8:6] (doublewords) at 6:2. *)
and ci_sp ~funct3 ~scale ~rd o =
  let low =
    if scale = 4 then (bits o ~hi:4 ~lo:2 lsl 4) lor (bits o ~hi:7 ~lo:6 lsl 2)
    else (bits o ~hi:4 ~lo:3 lsl 5) lor (bits o ~hi:8 ~lo:6 lsl 2)
  in
  q ~op:2 ~funct3 lor (bits o ~hi:5 ~lo:5 lsl 12) lor (rd lsl 7) lor low

(* sp-relative stores (CSS): [5:2|7:6] (words) or [5:3|8:6] (doublewords)
   at 12:7. *)
and css ~funct3 ~scale ~rs2 o =
  let high =
    if scale = 4 then (bits o ~hi:5 ~lo:2 lsl 9) lor (bits o ~hi:7 ~lo:6 lsl 7)
    else (bits o ~hi:5 ~lo:3 lsl 10) lor (bits o ~hi:8 ~lo:6 lsl 7)
  in
  q ~op:2 ~funct3 lor high lor (rs2 lsl 2)
