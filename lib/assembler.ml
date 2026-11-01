(** Two-pass assembler: source text to {!Image.t}.

    Pass 1 walks the statements, assigns every item (instruction or data) an
    offset in its section, and records labels. Each instruction becomes a
    closure [ctx -> Instr.t]. If the closure can run without knowing any
    addresses, it is evaluated on the spot, which is what lets us compress it to
    16 bits when RVC is on. Pass 2 places the sections, resolves symbols, and
    emits bytes. *)

open Instr
module P = Parser

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

exception Unresolved
(** Raised in pass 1 by expressions that need addresses. *)

type error = { line : int; msg : string }
type section = Text | Rodata | Data | Bss

let section_of_name name =
  let starts p =
    String.length name >= String.length p
    && String.sub name 0 (String.length p) = p
  in
  if starts ".text" then Some Text
  else if starts ".rodata" || starts ".srodata" then Some Rodata
  else if starts ".data" || starts ".sdata" then Some Data
  else if starts ".bss" || starts ".sbss" then Some Bss
  else None

let all_sections = [ Text; Rodata; Data; Bss ]
let text_base = 0x10000L

type ctx = {
  pc : unit -> int64;  (** address of the current item *)
  eval : P.expr -> int64;
  is_const : P.expr -> bool;  (** evaluable without any addresses *)
}

type gen = ctx -> Instr.t

(* {1 Operand helpers} *)

let reg = function
  | P.Expr (P.Sym s) -> (
      match Reg.of_string s with
      | Some r -> r
      | None -> error "expected an integer register, got %s" s)
  | _ -> error "expected an integer register"

let freg = function
  | P.Expr (P.Sym s) -> (
      match Freg.of_string s with
      | Some r -> r
      | None -> error "expected an FP register, got %s" s)
  | _ -> error "expected an FP register"

let expr = function
  | P.Expr e -> e
  | P.Mem _ -> error "unexpected memory operand"
  | P.Str _ -> error "unexpected string"

let mem = function
  | P.Mem (e, base) -> (
      match Reg.of_string base with
      | Some r -> (e, r)
      | None -> error "expected a base register, got %s" base)
  | _ -> error "expected a memory operand off(reg)"

let lookup_name table what = function
  | P.Expr (P.Sym s) -> (
      match
        List.find_opt (fun (_, n) -> n = String.lowercase_ascii s) table
      with
      | Some (v, _) -> v
      | None -> error "unknown %s %s" what s)
  | _ -> error "expected a %s" what

let rm_operand = lookup_name rms "rounding mode"
let i ctx e = Int64.to_int (ctx.eval e)

(** A branch/jump target: a symbolic expression is an absolute address, a plain
    number is a pc-relative offset (LLVM convention, which also lets disassembly
    round-trip). *)
let offset ctx e =
  if ctx.is_const e then i ctx e
  else Int64.to_int (Int64.sub (ctx.eval e) (ctx.pc ()))

let sext12 v = Int64.to_int (Int64.shift_right (Int64.shift_left v 52) 52)

(** Split a pc-relative distance into [auipc]/low-12 parts. *)
let pcrel_hi d =
  Int64.to_int (Int64.shift_right (Int64.add d 0x800L) 12) land 0xfffff

let pcrel_lo d = sext12 d

(* {1 Constant materialisation (li)}

   The LLVM algorithm: a 32-bit value is lui + addiw. Larger values
   materialise their upper bits recursively, then shift left and add the low
   12 bits. *)

let rec li_sequence rd (v : int64) : Instr.t list =
  if Int64.of_int32 (Int64.to_int32 v) = v then
    let hi20 =
      Int64.to_int (Int64.shift_right (Int64.add v 0x800L) 12) land 0xfffff
    in
    let lo12 = sext12 v in
    if hi20 = 0 then [ Op_imm { op = Addi; rd; rs1 = Reg.zero; imm = lo12 } ]
    else
      Lui { rd; imm = hi20 }
      ::
      (if lo12 = 0 then []
       else [ Op_imm { op = Addiw; rd; rs1 = rd; imm = lo12 } ])
  else
    let lo12 = sext12 v in
    let hi52 = Int64.shift_right_logical (Int64.add v 0x800L) 12 in
    let tz =
      let rec go n =
        if Int64.logand (Int64.shift_right_logical hi52 n) 1L = 1L then n
        else go (n + 1)
      in
      go 0
    in
    let shift = 12 + tz in
    let upper =
      let u = Int64.shift_right_logical hi52 tz in
      let bits = 64 - shift in
      Int64.shift_right (Int64.shift_left u (64 - bits)) (64 - bits)
    in
    li_sequence rd upper
    @ [ Shift_imm { op = Slli; rd; rs1 = rd; shamt = shift } ]
    @
    if lo12 = 0 then [] else [ Op_imm { op = Addi; rd; rs1 = rd; imm = lo12 } ]

(* {1 Mnemonic table} *)

let handlers : (string, P.operand list -> gen list) Hashtbl.t =
  Hashtbl.create 512

let def name h = Hashtbl.replace handlers name h

let arity name n ops =
  if List.length ops <> n then error "%s expects %d operands" name n

(** [pcrel_pair ~hi ~lo target]: auipc + an instruction using the low part, both
    relative to the auipc's address. *)
let pcrel_pair ~auipc_rd ~(lo : int -> Instr.t) target : gen list =
  [
    (fun ctx ->
      Auipc
        {
          rd = auipc_rd;
          imm = pcrel_hi (Int64.sub (ctx.eval target) (ctx.pc ()));
        });
    (fun ctx ->
      let auipc_pc = Int64.sub (ctx.pc ()) 4L in
      lo (pcrel_lo (Int64.sub (ctx.eval target) auipc_pc)));
  ]

let () =
  let simple name f = def name (fun ops -> [ f ops ]) in
  let rrr name f =
    simple name (fun ops ->
        arity name 3 ops;
        match ops with
        | [ a; b; c ] ->
            let rd = reg a and rs1 = reg b and rs2 = reg c in
            fun _ -> f rd rs1 rs2
        | _ -> assert false)
  in
  let rri name f =
    simple name (fun ops ->
        arity name 3 ops;
        match ops with
        | [ a; b; c ] ->
            let rd = reg a and rs1 = reg b and e = expr c in
            fun ctx -> f rd rs1 (i ctx e)
        | _ -> assert false)
  in
  let rr name f =
    simple name (fun ops ->
        arity name 2 ops;
        match ops with
        | [ a; b ] ->
            let rd = reg a and rs = reg b in
            fun _ -> f rd rs
        | _ -> assert false)
  in
  (* RV64I / M / Zb* register-register and immediate forms *)
  List.iter
    (fun (op, n) -> rrr n (fun rd rs1 rs2 -> Op { op; rd; rs1; rs2 }))
    reg_ops;
  List.iter
    (fun (op, n) -> rri n (fun rd rs1 imm -> Op_imm { op; rd; rs1; imm }))
    imm_ops;
  List.iter
    (fun (op, n) ->
      rri n (fun rd rs1 shamt -> Shift_imm { op; rd; rs1; shamt }))
    shift_ops;
  List.iter
    (fun (op, n) -> rr n (fun rd rs1 -> Unary { op; rd; rs1 }))
    unary_ops;
  (* branches *)
  let branch name op ~swap =
    simple name (fun ops ->
        arity name 3 ops;
        match ops with
        | [ a; b; t ] ->
            let r1 = reg a and r2 = reg b and t = expr t in
            let rs1, rs2 = if swap then (r2, r1) else (r1, r2) in
            fun ctx -> Branch { op; rs1; rs2; offset = offset ctx t }
        | _ -> assert false)
  in
  List.iter (fun (op, n) -> branch n op ~swap:false) branch_ops;
  List.iter
    (fun (n, op) -> branch n op ~swap:true)
    [ ("bgt", Blt); ("ble", Bge); ("bgtu", Bltu); ("bleu", Bgeu) ];
  let branch_zero name op ~zero_first =
    simple name (fun ops ->
        arity name 2 ops;
        match ops with
        | [ a; t ] ->
            let r = reg a and t = expr t in
            let rs1, rs2 =
              if zero_first then (Reg.zero, r) else (r, Reg.zero)
            in
            fun ctx -> Branch { op; rs1; rs2; offset = offset ctx t }
        | _ -> assert false)
  in
  branch_zero "beqz" Beq ~zero_first:false;
  branch_zero "bnez" Bne ~zero_first:false;
  branch_zero "bltz" Blt ~zero_first:false;
  branch_zero "bgez" Bge ~zero_first:false;
  branch_zero "blez" Bge ~zero_first:true;
  branch_zero "bgtz" Blt ~zero_first:true;
  (* loads and stores; a symbol operand expands to auipc + access *)
  List.iter
    (fun (op, n) ->
      def n (fun ops ->
          arity n 2 ops;
          match ops with
          | [ a; (P.Mem _ as m) ] ->
              let rd = reg a and e, rs1 = mem m in
              [ (fun ctx -> Load { op; rd; rs1; offset = i ctx e }) ]
          | [ a; P.Expr target ] ->
              let rd = reg a in
              pcrel_pair ~auipc_rd:rd
                ~lo:(fun offset -> Load { op; rd; rs1 = rd; offset })
                target
          | _ -> error "%s expects rd, off(rs1)" n))
    load_ops;
  List.iter
    (fun (op, n) ->
      def n (fun ops ->
          match ops with
          | [ a; (P.Mem _ as m) ] ->
              let rs2 = reg a and e, rs1 = mem m in
              [ (fun ctx -> Store { op; rs1; rs2; offset = i ctx e }) ]
          | [ a; P.Expr target; tmp ] ->
              let rs2 = reg a and tmp = reg tmp in
              pcrel_pair ~auipc_rd:tmp
                ~lo:(fun offset -> Store { op; rs1 = tmp; rs2; offset })
                target
          | _ -> error "%s expects rs2, off(rs1)" n))
    store_ops;
  (* upper immediates *)
  let upper name f =
    simple name (fun ops ->
        arity name 2 ops;
        match ops with
        | [ a; b ] ->
            let rd = reg a and e = expr b in
            fun ctx -> f rd (i ctx e land 0xfffff)
        | _ -> assert false)
  in
  upper "lui" (fun rd imm -> Lui { rd; imm });
  upper "auipc" (fun rd imm -> Auipc { rd; imm });
  (* jumps *)
  simple "jal" (function
    | [ t ] ->
        let t = expr t in
        fun ctx -> Jal { rd = Reg.ra; offset = offset ctx t }
    | [ a; t ] ->
        let rd = reg a and t = expr t in
        fun ctx -> Jal { rd; offset = offset ctx t }
    | _ -> error "jal expects [rd,] target");
  simple "j" (function
    | [ t ] ->
        let t = expr t in
        fun ctx -> Jal { rd = Reg.zero; offset = offset ctx t }
    | _ -> error "j expects a target");
  simple "jalr" (function
    | [ a ] ->
        let rs1 = reg a in
        fun _ -> Jalr { rd = Reg.ra; rs1; offset = 0 }
    | [ a; (P.Mem _ as m) ] ->
        let rd = reg a and e, rs1 = mem m in
        fun ctx -> Jalr { rd; rs1; offset = i ctx e }
    | [ a; b ] ->
        let rd = reg a and rs1 = reg b in
        fun _ -> Jalr { rd; rs1; offset = 0 }
    | [ a; b; c ] ->
        let rd = reg a and rs1 = reg b and e = expr c in
        fun ctx -> Jalr { rd; rs1; offset = i ctx e }
    | _ -> error "jalr expects rd, off(rs1)");
  simple "jr" (function
    | [ a ] ->
        let rs1 = reg a in
        fun _ -> Jalr { rd = Reg.zero; rs1; offset = 0 }
    | _ -> error "jr expects a register");
  simple "ret" (fun ops ->
      arity "ret" 0 ops;
      fun _ -> Jalr { rd = Reg.zero; rs1 = Reg.ra; offset = 0 });
  let far_call name ~link ~tmp =
    def name (function
      | [ t ] ->
          let t = expr t in
          pcrel_pair ~auipc_rd:tmp
            ~lo:(fun offset -> Jalr { rd = link; rs1 = tmp; offset })
            t
      | _ -> error "%s expects a target" name)
  in
  far_call "call" ~link:Reg.ra ~tmp:Reg.ra;
  far_call "tail" ~link:Reg.zero ~tmp:(Reg.of_int 6);
  (* address and constant loading *)
  let la name =
    def name (function
      | [ a; P.Expr target ] ->
          let rd = reg a in
          pcrel_pair ~auipc_rd:rd
            ~lo:(fun imm -> Op_imm { op = Addi; rd; rs1 = rd; imm })
            target
      | _ -> error "%s expects rd, symbol" name)
  in
  la "la";
  la "lla";
  (* "li" is special-cased in pass 1: its length depends on the value. *)
  (* simple pseudo-instructions *)
  simple "nop" (fun ops ->
      arity "nop" 0 ops;
      fun _ -> Op_imm { op = Addi; rd = Reg.zero; rs1 = Reg.zero; imm = 0 });
  rr "mv" (fun rd rs1 -> Op_imm { op = Addi; rd; rs1; imm = 0 });
  rr "not" (fun rd rs1 -> Op_imm { op = Xori; rd; rs1; imm = -1 });
  rr "neg" (fun rd rs2 -> Op { op = Sub; rd; rs1 = Reg.zero; rs2 });
  rr "negw" (fun rd rs2 -> Op { op = Subw; rd; rs1 = Reg.zero; rs2 });
  rr "sext.w" (fun rd rs1 -> Op_imm { op = Addiw; rd; rs1; imm = 0 });
  rr "zext.b" (fun rd rs1 -> Op_imm { op = Andi; rd; rs1; imm = 255 });
  rr "zext.w" (fun rd rs1 -> Op { op = Add_uw; rd; rs1; rs2 = Reg.zero });
  rr "seqz" (fun rd rs1 -> Op_imm { op = Sltiu; rd; rs1; imm = 1 });
  rr "snez" (fun rd rs2 -> Op { op = Sltu; rd; rs1 = Reg.zero; rs2 });
  rr "sltz" (fun rd rs1 -> Op { op = Slt; rd; rs1; rs2 = Reg.zero });
  rr "sgtz" (fun rd rs2 -> Op { op = Slt; rd; rs1 = Reg.zero; rs2 });
  (* system *)
  simple "ecall" (fun _ _ -> Ecall);
  simple "ebreak" (fun _ _ -> Ebreak);
  simple "fence.i" (fun _ _ -> Fence_i);
  let fence_set = function
    | P.Expr (P.Sym s) ->
        String.fold_left
          (fun acc c ->
            acc
            lor
            match c with
            | 'i' -> 8
            | 'o' -> 4
            | 'r' -> 2
            | 'w' -> 1
            | _ -> error "bad fence set %s" s [@ocamlformat "disable"])
          0 (String.lowercase_ascii s)
    | _ -> error "expected a fence set like rw"
  in
  simple "fence" (function
    | [] -> fun _ -> Fence { pred = 15; succ = 15 }
    | [ p; s ] ->
        let pred = fence_set p and succ = fence_set s in
        fun _ -> Fence { pred; succ }
    | _ -> error "fence expects no operands or pred, succ");
  (* Zicsr *)
  let csr_num = function
    | P.Expr (P.Sym s) when Csr.of_name s <> None ->
        `Known (Option.get (Csr.of_name s))
    | P.Expr e -> `Expr e
    | _ -> error "expected a CSR"
  in
  let csr_value ctx = function `Known n -> n | `Expr e -> i ctx e in
  List.iter
    (fun (op, n) ->
      simple n (fun ops ->
          arity n 3 ops;
          match ops with
          | [ a; c; b ] ->
              let rd = reg a and c = csr_num c and rs1 = reg b in
              fun ctx -> Csr { op; rd; src = Rs1 rs1; csr = csr_value ctx c }
          | _ -> assert false);
      simple (n ^ "i") (fun ops ->
          arity n 3 ops;
          match ops with
          | [ a; c; b ] ->
              let rd = reg a and c = csr_num c and u = expr b in
              fun ctx ->
                Csr { op; rd; src = Uimm (i ctx u); csr = csr_value ctx c }
          | _ -> assert false))
    csr_ops;
  simple "csrr" (function
    | [ a; c ] ->
        let rd = reg a and c = csr_num c in
        fun ctx ->
          Csr { op = Csrrs; rd; src = Rs1 Reg.zero; csr = csr_value ctx c }
    | _ -> error "csrr expects rd, csr");
  List.iter
    (fun (n, op) ->
      simple n (function
        | [ c; b ] ->
            let c = csr_num c and rs1 = reg b in
            fun ctx ->
              Csr { op; rd = Reg.zero; src = Rs1 rs1; csr = csr_value ctx c }
        | _ -> error "%s expects csr, rs" n);
      simple (n ^ "i") (function
        | [ c; b ] ->
            let c = csr_num c and u = expr b in
            fun ctx ->
              Csr
                {
                  op;
                  rd = Reg.zero;
                  src = Uimm (i ctx u);
                  csr = csr_value ctx c;
                }
        | _ -> error "%si expects csr, imm" n))
    [ ("csrw", Csrrw); ("csrs", Csrrs); ("csrc", Csrrc) ];
  List.iter
    (fun (n, csr) ->
      simple n (function
        | [ a ] ->
            let rd = reg a in
            fun _ -> Csr { op = Csrrs; rd; src = Rs1 Reg.zero; csr }
        | _ -> error "%s expects rd" n))
    [
      ("rdcycle", 0xc00);
      ("rdtime", 0xc01);
      ("rdinstret", 0xc02);
      ("frcsr", 0x003);
      ("frrm", 0x002);
      ("frflags", 0x001);
    ];
  List.iter
    (fun (n, csr) ->
      simple n (function
        | [ b ] ->
            let rs1 = reg b in
            fun _ -> Csr { op = Csrrw; rd = Reg.zero; src = Rs1 rs1; csr }
        | [ a; b ] ->
            let rd = reg a and rs1 = reg b in
            fun _ -> Csr { op = Csrrw; rd; src = Rs1 rs1; csr }
        | _ -> error "%s expects [rd,] rs" n))
    [ ("fscsr", 0x003); ("fsrm", 0x002); ("fsflags", 0x001) ];
  (* A *)
  List.iter
    (fun (op, n) ->
      List.iter
        (fun (width, w) ->
          List.iter
            (fun (suffix, aq, rl) ->
              let name = Printf.sprintf "%s.%s%s" n w suffix in
              simple name (fun ops ->
                  match (op, ops) with
                  | Lr, [ a; m ] ->
                      let rd = reg a and _, rs1 = mem m in
                      fun _ ->
                        Amo { op; width; aq; rl; rd; rs1; rs2 = Reg.zero }
                  | Lr, _ -> error "%s expects rd, (rs1)" name
                  | _, [ a; b; m ] ->
                      let rd = reg a and rs2 = reg b and _, rs1 = mem m in
                      fun _ -> Amo { op; width; aq; rl; rd; rs1; rs2 }
                  | _ -> error "%s expects rd, rs2, (rs1)" name))
            [
              ("", false, false);
              (".aq", true, false);
              (".rl", false, true);
              (".aqrl", true, true);
            ])
        widths)
    amo_ops;
  (* F / D *)
  let with_rm i ops k =
    match ops with
    | [] -> k (default_rm (i Dyn))
    | [ r ] -> k (rm_operand r)
    | _ -> error "too many operands"
  in
  List.iter
    (fun (fmt, fs) ->
      let nm base = base ^ "." ^ fs in
      List.iter
        (fun (op, n) ->
          simple (nm n) (function
            | a :: b :: c :: rest ->
                let rd = freg a and rs1 = freg b and rs2 = freg c in
                let mk rm = Farith { op; fmt; rd; rs1; rs2; rm } in
                with_rm mk rest (fun rm _ -> mk rm)
            | _ -> error "%s expects fd, fs1, fs2" (nm n)))
        farith_ops;
      simple (nm "fsqrt") (function
        | a :: b :: rest ->
            let rd = freg a and rs1 = freg b in
            let mk rm = Fsqrt { fmt; rd; rs1; rm } in
            with_rm mk rest (fun rm _ -> mk rm)
        | _ -> error "fsqrt expects fd, fs");
      List.iter
        (fun (op, n) ->
          simple (nm n) (function
            | [ a; b; c ] ->
                let rd = freg a and rs1 = freg b and rs2 = freg c in
                fun _ -> Fsign { op; fmt; rd; rs1; rs2 }
            | _ -> error "%s expects fd, fs1, fs2" (nm n)))
        fsign_ops;
      List.iter
        (fun (n, op) ->
          simple (nm n) (function
            | [ a; b ] ->
                let rd = freg a and rs = freg b in
                fun _ -> Fsign { op; fmt; rd; rs1 = rs; rs2 = rs }
            | _ -> error "%s expects fd, fs" (nm n)))
        [ ("fmv", Fsgnj); ("fneg", Fsgnjn); ("fabs", Fsgnjx) ];
      List.iter
        (fun (op, n) ->
          simple (nm n) (function
            | a :: b :: c :: d :: rest ->
                let rd = freg a
                and rs1 = freg b
                and rs2 = freg c
                and rs3 = freg d in
                let mk rm = Fma { op; fmt; rd; rs1; rs2; rs3; rm } in
                with_rm mk rest (fun rm _ -> mk rm)
            | _ -> error "%s expects fd, fs1, fs2, fs3" (nm n)))
        fma_ops;
      List.iter
        (fun (op, n) ->
          simple (nm n) (function
            | [ a; b; c ] ->
                let rd = reg a and rs1 = freg b and rs2 = freg c in
                fun _ -> Fcmp { op; fmt; rd; rs1; rs2 }
            | _ -> error "%s expects rd, fs1, fs2" (nm n)))
        fcmp_ops;
      simple (nm "fclass") (function
        | [ a; b ] ->
            let rd = reg a and rs1 = freg b in
            fun _ -> Fclass { fmt; rd; rs1 }
        | _ -> error "fclass expects rd, fs");
      List.iter
        (fun (ity, it) ->
          simple (Printf.sprintf "fcvt.%s.%s" it fs) (function
            | a :: b :: rest ->
                let rd = reg a and rs1 = freg b in
                let mk rm = Fcvt_to_int { fmt; ity; rd; rs1; rm } in
                with_rm mk rest (fun rm _ -> mk rm)
            | _ -> error "fcvt expects rd, fs");
          simple (Printf.sprintf "fcvt.%s.%s" fs it) (function
            | a :: b :: rest ->
                let rd = freg a and rs1 = reg b in
                let mk rm = Fcvt_of_int { fmt; ity; rd; rs1; rm } in
                with_rm mk rest (fun rm _ -> mk rm)
            | _ -> error "fcvt expects fd, rs"))
        int_tys;
      simple
        (Printf.sprintf "fcvt.%s.%s" fs (name fmts (other_fmt fmt)))
        (function
          | a :: b :: rest ->
              let rd = freg a and rs1 = freg b in
              let mk rm = Fcvt_fmt { fmt; rd; rs1; rm } in
              with_rm mk rest (fun rm _ -> mk rm)
          | _ -> error "fcvt expects fd, fs");
      let xw = if fmt = S then "w" else "d" in
      simple ("fmv.x." ^ xw) (function
        | [ a; b ] ->
            let rd = reg a and rs1 = freg b in
            fun _ -> Fmv_to_int { fmt; rd; rs1 }
        | _ -> error "fmv.x expects rd, fs");
      simple
        ("fmv." ^ xw ^ ".x")
        (function
          | [ a; b ] ->
              let rd = freg a and rs1 = reg b in
              fun _ -> Fmv_of_int { fmt; rd; rs1 }
          | _ -> error "fmv expects fd, rs");
      let ld = if fmt = S then "flw" else "fld"
      and st = if fmt = S then "fsw" else "fsd" in
      def ld (function
        | [ a; (P.Mem _ as m) ] ->
            let rd = freg a and e, rs1 = mem m in
            [ (fun ctx -> Fload { fmt; rd; rs1; offset = i ctx e }) ]
        | [ a; P.Expr target; tmp ] ->
            let rd = freg a and tmp = reg tmp in
            pcrel_pair ~auipc_rd:tmp
              ~lo:(fun offset -> Fload { fmt; rd; rs1 = tmp; offset })
              target
        | _ -> error "%s expects fd, off(rs1)" ld);
      def st (function
        | [ a; (P.Mem _ as m) ] ->
            let rs2 = freg a and e, rs1 = mem m in
            [ (fun ctx -> Fstore { fmt; rs1; rs2; offset = i ctx e }) ]
        | [ a; P.Expr target; tmp ] ->
            let rs2 = freg a and tmp = reg tmp in
            pcrel_pair ~auipc_rd:tmp
              ~lo:(fun offset -> Fstore { fmt; rs1 = tmp; rs2; offset })
              target
        | _ -> error "%s expects fs, off(rs1)" st))
    fmts

(* {1 Passes} *)

type item = {
  item_line : int;
  section : section;
  offset : int;
  size : int;
  emit : ctx -> Bytes.t;
}

let le_bytes size (v : int64) =
  let b = Bytes.create size in
  for k = 0 to size - 1 do
    Bytes.set_uint8 b k
      (Int64.to_int (Int64.shift_right_logical v (8 * k)) land 0xff)
  done;
  b

let encode_exn instr =
  match Encode.encode instr with
  | Ok w -> le_bytes 4 (Int64.of_int w)
  | Error msg -> error "%s: %s" (Instr.to_string instr) msg

type state = {
  mutable section : section;
  offsets : (section, int) Hashtbl.t;
  mutable items : item list;
  labels : (string, section * int) Hashtbl.t;
  consts : (string, int64) Hashtbl.t;
  pcrel_targets : (string, P.expr) Hashtbl.t;
      (** label of an auipc -> its target *)
  mutable rvc : bool;
  mutable rvc_stack : bool list;
  mutable errors : error list;
}

let cur_offset st =
  Option.value ~default:0 (Hashtbl.find_opt st.offsets st.section)

let add_item st ~line ~size emit =
  let it : item =
    {
      item_line = line;
      section = st.section;
      offset = cur_offset st;
      size;
      emit;
    }
  in
  st.items <- it :: st.items;
  Hashtbl.replace st.offsets st.section (cur_offset st + size)

(** Evaluate an expression. [addr] resolves labels; in pass 1 it raises
    [Unresolved]. *)
let rec eval st ~addr ~pc (e : P.expr) : int64 =
  let ev = eval st ~addr ~pc in
  match e with
  | P.Num n -> n
  | P.Fnum _ -> error "floating-point value in an integer context"
  | P.Sym s -> (
      match Hashtbl.find_opt st.consts s with
      | Some v -> v
      | None ->
          if Reg.of_string s <> None || Freg.of_string s <> None then
            error "unexpected register %s" s;
          addr s)
  | P.Neg e -> Int64.neg (ev e)
  | P.Add (a, b) -> Int64.add (ev a) (ev b)
  | P.Sub (a, b) -> Int64.sub (ev a) (ev b)
  | P.Reloc (P.Hi, e) -> Int64.of_int (pcrel_hi (ev e))
  | P.Reloc (P.Lo, e) -> Int64.of_int (sext12 (ev e))
  | P.Reloc (P.Pcrel_hi, e) ->
      Int64.of_int (pcrel_hi (Int64.sub (ev e) (pc ())))
  | P.Reloc (P.Pcrel_lo, P.Sym label) -> (
      match Hashtbl.find_opt st.pcrel_targets label with
      | None ->
          error "%%pcrel_lo(%s): %s does not label an auipc with %%pcrel_hi"
            label label
      | Some target ->
          Int64.of_int (pcrel_lo (Int64.sub (ev target) (addr label))))
  | P.Reloc (P.Pcrel_lo, _) -> error "%%pcrel_lo expects the label of an auipc"

let pass1_ctx st =
  let eval =
    eval st ~addr:(fun _ -> raise Unresolved) ~pc:(fun () -> raise Unresolved)
  in
  let is_const e =
    match eval e with _ -> true | exception Unresolved -> false
  in
  { pc = (fun () -> raise Unresolved); eval; is_const }

let add_instruction st ~line (g : gen) =
  if st.section <> Text then error "instructions must be in .text";
  match g (pass1_ctx st) with
  | instr -> (
      (* fully known now: may be compressed *)
      match if st.rvc then Compressed.compress instr else None with
      | Some c ->
          add_item st ~line ~size:2 (fun _ -> le_bytes 2 (Int64.of_int c))
      | None ->
          let bytes = encode_exn instr in
          add_item st ~line ~size:4 (fun _ -> bytes))
  | exception Unresolved ->
      add_item st ~line ~size:4 (fun ctx -> encode_exn (g ctx))

let align_to st ~line n =
  if n <= 0 || n land (n - 1) <> 0 then
    error "alignment %d is not a power of two" n;
  let pad = (n - (cur_offset st land (n - 1))) land (n - 1) in
  if pad > 0 then
    let fill =
      if st.section = Text then (
        (* pad code with nops so falling through an alignment is harmless *)
        let b = Buffer.create pad in
        if pad land 2 <> 0 then Buffer.add_bytes b (le_bytes 2 0x0001L);
        for _ = 1 to pad / 4 do
          Buffer.add_bytes b (le_bytes 4 0x00000013L)
        done;
        if pad land 1 <> 0 then Buffer.add_char b '\000';
        Buffer.to_bytes b)
      else Bytes.make pad '\000'
    in
    add_item st ~line ~size:pad (fun _ -> fill)

let const_int st e =
  match (pass1_ctx st).eval e with
  | v -> v
  | exception Unresolved -> error "expected a constant expression"

let directive st ~line name (ops : P.operand list) =
  let data_values size =
    List.iter
      (fun op ->
        let e = expr op in
        add_item st ~line ~size (fun ctx -> le_bytes size (ctx.eval e)))
      ops
  in
  let float_values size =
    List.iter
      (fun op ->
        let f =
          match expr op with
          | P.Fnum f -> f
          | P.Num n -> Int64.to_float n
          | _ -> error "expected a number"
        in
        let bits =
          if size = 4 then Int64.of_int32 (Int32.bits_of_float f)
          else Int64.bits_of_float f
        in
        add_item st ~line ~size (fun _ -> le_bytes size bits))
      ops
  in
  let strings ~nul =
    List.iter
      (function
        | P.Str s ->
            let s = if nul then s ^ "\000" else s in
            add_item st ~line ~size:(String.length s) (fun _ ->
                Bytes.of_string s)
        | _ -> error "%s expects strings" name)
      ops
  in
  let in_bss_ok =
    [ ".space"; ".skip"; ".zero"; ".align"; ".p2align"; ".balign" ]
  in
  if
    st.section = Bss
    && List.mem name
         [ ".byte"; ".half"; ".word"; ".dword"; ".ascii"; ".asciz"; ".string" ]
    && not (List.mem name in_bss_ok)
  then error "%s is not allowed in .bss" name;
  match name with
  | ".text" -> st.section <- Text
  | ".data" -> st.section <- Data
  | ".rodata" -> st.section <- Rodata
  | ".bss" -> st.section <- Bss
  | ".section" -> (
      match ops with
      | P.Expr (P.Sym s) :: _ -> (
          match section_of_name s with
          | Some sec -> st.section <- sec
          | None -> error "unknown section %s" s)
      | _ -> error ".section expects a name")
  | ".globl" | ".global" | ".local" | ".type" | ".size" | ".file" | ".ident"
  | ".attribute" | ".weak" | ".hidden" | ".cfi_startproc" | ".cfi_endproc" ->
      ()
  | ".equ" | ".set" -> (
      match ops with
      | [ P.Expr (P.Sym n); P.Expr e ] ->
          Hashtbl.replace st.consts n (const_int st e)
      | _ -> error "%s expects name, value" name)
  | ".align" | ".p2align" -> (
      match ops with
      | P.Expr e :: _ -> align_to st ~line (1 lsl Int64.to_int (const_int st e))
      | _ -> error "%s expects a power" name)
  | ".balign" -> (
      match ops with
      | P.Expr e :: _ -> align_to st ~line (Int64.to_int (const_int st e))
      | _ -> error ".balign expects a size")
  | ".byte" -> data_values 1
  | ".half" | ".2byte" | ".short" -> data_values 2
  | ".word" | ".4byte" | ".long" -> data_values 4
  | ".dword" | ".8byte" | ".quad" -> data_values 8
  | ".float" -> float_values 4
  | ".double" -> float_values 8
  | ".ascii" -> strings ~nul:false
  | ".asciz" | ".string" -> strings ~nul:true
  | ".space" | ".skip" | ".zero" ->
      let n, fill =
        match ops with
        | [ P.Expr n ] -> (const_int st n, 0L)
        | [ P.Expr n; P.Expr f ] -> (const_int st n, const_int st f)
        | _ -> error "%s expects size[, fill]" name
      in
      let n = Int64.to_int n in
      if n < 0 then error "negative size";
      if st.section = Bss && fill <> 0L then error "non-zero fill in .bss";
      let b = Bytes.make n (Char.chr (Int64.to_int fill land 0xff)) in
      add_item st ~line ~size:n (fun _ -> b)
  | ".option" -> (
      match ops with
      | [ P.Expr (P.Sym "rvc") ] -> st.rvc <- true
      | [ P.Expr (P.Sym "norvc") ] -> st.rvc <- false
      | [ P.Expr (P.Sym "push") ] -> st.rvc_stack <- st.rvc :: st.rvc_stack
      | [ P.Expr (P.Sym "pop") ] -> (
          match st.rvc_stack with
          | r :: rest ->
              st.rvc <- r;
              st.rvc_stack <- rest
          | [] -> error ".option pop without push")
      | [ P.Expr (P.Sym ("relax" | "norelax" | "pic" | "nopic")) ] -> ()
      | _ -> error "unsupported .option")
  | _ -> error "unknown directive %s" name

let statement st (s : P.stmt) =
  List.iter
    (fun l ->
      if Hashtbl.mem st.labels l || Hashtbl.mem st.consts l then
        error "duplicate label %s" l;
      Hashtbl.replace st.labels l (st.section, cur_offset st))
    s.labels;
  match s.op with
  | None -> ()
  | Some (name, ops) when String.length name > 0 && name.[0] = '.' ->
      directive st ~line:s.line name ops
  | Some ("li", ops) -> (
      match ops with
      | [ a; P.Expr e ] ->
          let rd = reg a in
          let v =
            match (pass1_ctx st).eval e with
            | v -> v
            | exception Unresolved ->
                error "li needs a constant; use la for addresses"
          in
          List.iter
            (fun ins -> add_instruction st ~line:s.line (fun _ -> ins))
            (li_sequence rd v)
      | _ -> error "li expects rd, imm")
  | Some (name, ops) -> (
      (* remember auipc targets for %pcrel_lo(label) *)
      (match (name, ops) with
      | "auipc", [ _; P.Expr (P.Reloc (P.Pcrel_hi, target)) ] ->
          List.iter
            (fun l -> Hashtbl.replace st.pcrel_targets l target)
            s.labels
      | _ -> ());
      match Hashtbl.find_opt handlers name with
      | Some h -> List.iter (add_instruction st ~line:s.line) (h ops)
      | None -> error "unknown instruction %s" name)

let page_align a = Int64.(logand (add a 4095L) (lognot 4095L))

let assemble ?(rvc = false) (source : string) : (Image.t, error list) result =
  match Parser.parse source with
  | Error errs -> Error (List.map (fun (line, msg) -> { line; msg }) errs)
  | Ok stmts -> (
      let st =
        {
          section = Text;
          offsets = Hashtbl.create 4;
          items = [];
          labels = Hashtbl.create 64;
          consts = Hashtbl.create 16;
          pcrel_targets = Hashtbl.create 16;
          rvc;
          rvc_stack = [];
          errors = [];
        }
      in
      List.iter
        (fun (s : P.stmt) ->
          try statement st s
          with Error msg -> st.errors <- { line = s.line; msg } :: st.errors)
        stmts;
      (* layout: sections in order, each page-aligned *)
      let size sec =
        Option.value ~default:0 (Hashtbl.find_opt st.offsets sec)
      in
      let bases =
        List.fold_left
          (fun (acc, next) sec ->
            ( (sec, next) :: acc,
              page_align (Int64.add next (Int64.of_int (size sec))) ))
          ([], text_base) all_sections
        |> fst
      in
      let base sec = List.assoc sec bases in
      let addr name =
        match Hashtbl.find_opt st.labels name with
        | Some (sec, off) -> Int64.add (base sec) (Int64.of_int off)
        | None -> error "undefined symbol %s" name
      in
      let buffers =
        List.map (fun sec -> (sec, Bytes.make (size sec) '\000')) all_sections
      in
      List.iter
        (fun (it : item) ->
          let pc = Int64.add (base it.section) (Int64.of_int it.offset) in
          let eval = eval st ~addr ~pc:(fun () -> pc) in
          let is_const e = (pass1_ctx st).is_const e in
          match it.emit { pc = (fun () -> pc); eval; is_const } with
          | b ->
              Bytes.blit b 0 (List.assoc it.section buffers) it.offset it.size
          | exception Error msg ->
              st.errors <- { line = it.item_line; msg } :: st.errors)
        (List.rev st.items);
      match List.sort (fun a b -> compare a.line b.line) st.errors with
      | _ :: _ as errs -> Error errs
      | [] ->
          let perm = function
            | Text -> { Image.r = true; w = false; x = true }
            | Rodata -> { r = true; w = false; x = false }
            | Data | Bss -> { r = true; w = true; x = false }
          in
          let name = function
            | Text -> ".text"
            | Rodata -> ".rodata"
            | Data -> ".data"
            | Bss -> ".bss"
          in
          let segments =
            List.filter_map
              (fun sec ->
                if size sec = 0 then None
                else
                  Some
                    {
                      Image.name = name sec;
                      vaddr = base sec;
                      data =
                        (if sec = Bss then Bytes.empty
                         else List.assoc sec buffers);
                      mem_size = size sec;
                      perm = perm sec;
                    })
              all_sections
          in
          let symbols =
            Hashtbl.fold
              (fun name _ acc -> (name, addr name) :: acc)
              st.labels []
            |> List.sort (fun (_, a) (_, b) -> Int64.compare a b)
          in
          let symbols =
            ("__global_pointer$", Int64.add (base Data) 0x800L) :: symbols
          in
          let entry =
            match
              List.find_map
                (fun n -> List.assoc_opt n symbols)
                [ "_start"; "main" ]
            with
            | Some e -> e
            | None -> text_base
          in
          (* marks images that use the RARS-style debug syscalls; see Os *)
          let symbols = ("__riscvsim_debug_calls", 0L) :: symbols in
          Ok { Image.entry; segments; symbols; phdrs = None })
