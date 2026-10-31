(** Parse assembly source into statements. Operands are kept generic
    (expressions, memory references, strings). The assembler interprets them per
    mnemonic, where it knows whether a name is a register, a symbol, a CSR or a
    rounding mode. *)

type reloc = Hi | Lo | Pcrel_hi | Pcrel_lo

type expr =
  | Num of int64
  | Fnum of float
  | Sym of string
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Reloc of reloc * expr

type operand =
  | Expr of expr
  | Mem of expr * string  (** [off(base)] *)
  | Str of string

type stmt = {
  line : int;
  labels : string list;
  op : (string * operand list) option;  (** mnemonic or directive *)
}

open Lexer

let relocs =
  [ ("hi", Hi); ("lo", Lo); ("pcrel_hi", Pcrel_hi); ("pcrel_lo", Pcrel_lo) ]

(* Expression grammar:
     sum   := unary (('+' | '-') unary)*
     unary := '-' unary | '+' unary | atom
     atom  := INT | FLOAT | IDENT | LOCALREF | '(' sum ')' | '%' IDENT '(' sum ')' *)

let rec sum toks =
  let e, rest = unary toks in
  let rec more e = function
    | Plus :: rest ->
        let e', rest = unary rest in
        more (Add (e, e')) rest
    | Minus :: rest ->
        let e', rest = unary rest in
        more (Sub (e, e')) rest
    | rest -> (e, rest)
  in
  more e rest

and unary = function
  | Minus :: rest ->
      let e, rest = unary rest in
      ( (match e with
        | Num n -> Num (Int64.neg n)
        | Fnum f -> Fnum (-.f)
        | e -> Neg e),
        rest )
  | Plus :: rest -> unary rest
  | toks -> atom toks

and atom = function
  | Int n :: rest -> (Num n, rest)
  | Float f :: rest -> (Fnum f, rest)
  | Ident s :: rest -> (Sym s, rest)
  | Local_ref (n, d) :: rest ->
      (* resolved to a real symbol by [resolve_local_labels] *)
      (Sym (Printf.sprintf "%d%s" n (if d = `Fwd then "f" else "b")), rest)
  | Lparen :: rest -> (
      match sum rest with
      | e, Rparen :: rest -> (e, rest)
      | _ -> Lexer.error "expected ')'")
  | Percent :: Ident r :: Lparen :: rest -> (
      match List.assoc_opt r relocs with
      | None -> Lexer.error "unknown relocation %%%s" r
      | Some reloc -> (
          match sum rest with
          | e, Rparen :: rest -> (Reloc (reloc, e), rest)
          | _ -> Lexer.error "expected ')' after %%%s(" r))
  | t :: _ -> Lexer.error "unexpected %s" (Format.asprintf "%a" pp_token t)
  | [] -> Lexer.error "expected an expression"

let operand = function
  | [ String s ] -> Str s
  | [ Lparen; Ident base; Rparen ] -> Mem (Num 0L, base)
  | toks -> (
      match sum toks with
      | e, [] -> Expr e
      | e, [ Lparen; Ident base; Rparen ] -> Mem (e, base)
      | _, t :: _ ->
          Lexer.error "unexpected %s in operand"
            (Format.asprintf "%a" pp_token t))

(** Split a token list on top-level commas (commas inside parentheses don't
    separate operands). *)
let split_commas toks =
  let rec go depth cur acc = function
    | [] -> List.rev (List.rev cur :: acc)
    | Comma :: rest when depth = 0 -> go depth [] (List.rev cur :: acc) rest
    | (Lparen as t) :: rest -> go (depth + 1) (t :: cur) acc rest
    | (Rparen as t) :: rest -> go (depth - 1) (t :: cur) acc rest
    | t :: rest -> go depth (t :: cur) acc rest
  in
  match toks with [] -> [] | _ -> go 0 [] [] toks

let parse_line line_no text =
  let toks = Lexer.tokenize text in
  let rec labels acc = function
    | Ident l :: Colon :: rest -> labels (l :: acc) rest
    | Int n :: Colon :: rest -> labels (Printf.sprintf "%Ld:" n :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let labels, rest = labels [] toks in
  let op =
    match rest with
    | [] -> None
    | Ident m :: args ->
        Some (String.lowercase_ascii m, List.map operand (split_commas args))
    | t :: _ ->
        Lexer.error "expected a mnemonic, got %s"
          (Format.asprintf "%a" pp_token t)
  in
  { line = line_no; labels; op }

(* Numeric local labels: "1:" may be defined many times. "1b" refers to
   the closest previous definition, "1f" to the next one. We rename every
   definition to a unique symbol and rewrite the references to match. *)

let local_name n k = Printf.sprintf ".L%d$%d" n k

let resolve_local_labels stmts =
  let counts = Hashtbl.create 8 in
  let count n = Option.value ~default:0 (Hashtbl.find_opt counts n) in
  let is_local_def l = String.length l > 1 && l.[String.length l - 1] = ':' in
  let rec rw_expr = function
    | Sym s
      when String.length s > 1
           && (s.[String.length s - 1] = 'f' || s.[String.length s - 1] = 'b')
           && String.for_all
                (fun c -> c >= '0' && c <= '9')
                (String.sub s 0 (String.length s - 1)) ->
        let n = int_of_string (String.sub s 0 (String.length s - 1)) in
        let k =
          if s.[String.length s - 1] = 'b' then count n else count n + 1
        in
        Sym (local_name n k)
    | Neg e -> Neg (rw_expr e)
    | Add (a, b) -> Add (rw_expr a, rw_expr b)
    | Sub (a, b) -> Sub (rw_expr a, rw_expr b)
    | Reloc (r, e) -> Reloc (r, rw_expr e)
    | e -> e
  in
  let rw_operand = function
    | Expr e -> Expr (rw_expr e)
    | Mem (e, b) -> Mem (rw_expr e, b)
    | o -> o
  in
  List.map
    (fun st ->
      let labels =
        List.map
          (fun l ->
            if is_local_def l then (
              let n = int_of_string (String.sub l 0 (String.length l - 1)) in
              let k = count n + 1 in
              Hashtbl.replace counts n k;
              local_name n k)
            else l)
          st.labels
      in
      (* references on this line see this line's definitions as "previous" *)
      let op =
        Option.map (fun (m, ops) -> (m, List.map rw_operand ops)) st.op
      in
      { st with labels; op })
    stmts

let parse (source : string) : (stmt list, (int * string) list) result =
  let lines = String.split_on_char '\n' source in
  let stmts, errors =
    List.fold_left
      (fun (stmts, errs) (i, text) ->
        match parse_line i text with
        | st -> (st :: stmts, errs)
        | exception Lexer.Error msg -> (stmts, (i, msg) :: errs))
      ([], [])
      (List.mapi (fun i l -> (i + 1, l)) lines)
  in
  match errors with
  | [] -> Ok (resolve_local_labels (List.rev stmts))
  | errs -> Error (List.rev errs)
