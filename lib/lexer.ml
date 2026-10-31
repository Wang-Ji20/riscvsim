(** Line-oriented tokenizer for GNU-style RISC-V assembly. *)

type token =
  | Ident of string  (** mnemonics, registers, symbols, directives *)
  | Int of int64
  | Float of float
  | String of string
  | Local_ref of int * [ `Fwd | `Bwd ]  (** [1f], [2b] *)
  | Comma
  | Lparen
  | Rparen
  | Colon
  | Plus
  | Minus
  | Percent

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let pp_token ppf = function
  | Ident s -> Format.fprintf ppf "%s" s
  | Int n -> Format.fprintf ppf "%Ld" n
  | Float f -> Format.fprintf ppf "%g" f
  | String s -> Format.fprintf ppf "%S" s
  | Local_ref (n, d) ->
      Format.fprintf ppf "%d%c" n (if d = `Fwd then 'f' else 'b')
  | Comma -> Format.pp_print_string ppf ","
  | Lparen -> Format.pp_print_string ppf "("
  | Rparen -> Format.pp_print_string ppf ")"
  | Colon -> Format.pp_print_string ppf ":"
  | Plus -> Format.pp_print_string ppf "+"
  | Minus -> Format.pp_print_string ppf "-"
  | Percent -> Format.pp_print_string ppf "%"

let is_digit c = c >= '0' && c <= '9'

let is_ident_start c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c = '_' || c = '.' || c = '$'

let is_ident_char c = is_ident_start c || is_digit c

let escape = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '0' -> '\000'
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | c -> error "unknown escape \\%c" c

(** Parse an integer literal: decimal, [0x] hex or [0b] binary, up to 64 bits
    (values above [Int64.max_int] wrap, as assemblers allow). *)
let parse_int s =
  let s' = String.lowercase_ascii s in
  match Int64.of_string_opt s' with
  | Some n -> n
  | None -> (
      match Int64.of_string_opt ("0u" ^ s') with
      | Some n -> n
      | None -> error "invalid integer %s" s)

let tokenize (line : string) : token list =
  let n = String.length line in
  let toks = ref [] in
  let push t = toks := t :: !toks in
  let rec go i =
    if i >= n then ()
    else
      match line.[i] with
      | ' ' | '\t' | '\r' -> go (i + 1)
      | '#' -> () (* comment *)
      | '/' when i + 1 < n && line.[i + 1] = '/' -> ()
      | ',' ->
          push Comma;
          go (i + 1)
      | '(' ->
          push Lparen;
          go (i + 1)
      | ')' ->
          push Rparen;
          go (i + 1)
      | ':' ->
          push Colon;
          go (i + 1)
      | '+' ->
          push Plus;
          go (i + 1)
      | '-' ->
          push Minus;
          go (i + 1)
      | '%' ->
          push Percent;
          go (i + 1)
      | '"' -> string_lit (i + 1) (Buffer.create 16)
      | '\'' -> char_lit (i + 1)
      | c when is_digit c -> number i
      | c when is_ident_start c ->
          let j = ref i in
          while !j < n && is_ident_char line.[!j] do
            incr j
          done;
          push (Ident (String.sub line i (!j - i)));
          go !j
      | c -> error "unexpected character %C" c
  and string_lit i buf =
    if i >= n then error "unterminated string"
    else
      match line.[i] with
      | '"' ->
          push (String (Buffer.contents buf));
          go (i + 1)
      | '\\' when i + 1 < n ->
          Buffer.add_char buf (escape line.[i + 1]);
          string_lit (i + 2) buf
      | c ->
          Buffer.add_char buf c;
          string_lit (i + 1) buf
  and char_lit i =
    let c, j =
      if i < n && line.[i] = '\\' && i + 1 < n then (escape line.[i + 1], i + 2)
      else if i < n then (line.[i], i + 1)
      else error "unterminated character literal"
    in
    if j < n && line.[j] = '\'' then (
      push (Int (Int64.of_int (Char.code c)));
      go (j + 1))
    else error "unterminated character literal"
  and number i =
    let j = ref i in
    while !j < n && is_ident_char line.[!j] do
      incr j
    done;
    (* a float: digits '.' digits, with optional exponent *)
    let is_float_char c = is_digit c || c = '.' || c = 'e' || c = 'E' in
    let k = ref i in
    while
      !k < n
      && (is_float_char line.[!k]
         || (line.[!k] = '-' || line.[!k] = '+')
            && (line.[!k - 1] = 'e' || line.[!k - 1] = 'E'))
    do
      incr k
    done;
    let word = String.sub line i (!j - i) in
    let fword = String.sub line i (!k - i) in
    let is_hex_or_bin =
      String.length word > 1
      && word.[0] = '0'
      &&
      match word.[1] with
      | 'x' | 'X' -> true
      | 'b' | 'B' -> String.length word > 2
      | _ -> false
    in
    if
      (not is_hex_or_bin)
      && (String.contains fword '.' || String.contains fword 'e'
        || String.contains fword 'E')
      && !k >= !j
    then
      match float_of_string_opt fword with
      | Some f ->
          push (Float f);
          go !k
      | None -> error "invalid number %s" fword
    else
      let last = word.[String.length word - 1] in
      let body = String.sub word 0 (String.length word - 1) in
      if
        (last = 'f' || last = 'b')
        && (not is_hex_or_bin)
        && String.for_all is_digit body
        && body <> ""
      then (
        push (Local_ref (int_of_string body, if last = 'f' then `Fwd else `Bwd));
        go !j)
      else (
        push (Int (parse_int word));
        go !j)
  in
  go 0;
  List.rev !toks
