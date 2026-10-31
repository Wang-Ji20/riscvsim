(** IEEE-754 support for the F and D extensions.

    OCaml floats are binary64 and the host only rounds to nearest-even. We get
    the other RISC-V rounding modes by recovering the exact rounding error of
    each operation with error-free transforms (TwoSum, FMA residuals). That
    error lets us move to the correct neighbouring float. Single-precision
    results are computed as round-to-odd binary64 values, then rounded to
    binary32 in the requested mode, which is provably correct because 53 >= 24 +
    2.

    Fused multiply-add uses Boldo and Muller's ErrFma for its exact error. *)

open Instr

(* {1 Exception flags (fflags bits)} *)

let nv = 0x10 (* invalid *)
let dz = 0x08 (* divide by zero *)
let ofl = 0x04 (* overflow *)
let ufl = 0x02 (* underflow *)
let nx = 0x01 (* inexact *)

(* {1 Bit-level representation} *)

let canonical_nan_s = 0x7fc00000l
let canonical_nan_d = 0x7ff8000000000000L
let box_mask = 0xffffffff00000000L

(** NaN-box a single: upper 32 bits all ones. *)
let box_s (bits : int32) =
  Int64.logor box_mask (Int64.logand (Int64.of_int32 bits) 0xffffffffL)

(** The single held in an FP register. Improperly boxed values read as the
    canonical NaN, as the spec requires. *)
let unbox_s (reg : int64) : int32 =
  if Int64.logand reg box_mask = box_mask then Int64.to_int32 reg
  else canonical_nan_s

let is_nan_s b =
  Int32.logand b 0x7f800000l = 0x7f800000l && Int32.logand b 0x007fffffl <> 0l

let is_snan_s b = is_nan_s b && Int32.logand b 0x00400000l = 0l

let is_nan_d b =
  Int64.logand b 0x7ff0000000000000L = 0x7ff0000000000000L
  && Int64.logand b 0x000fffffffffffffL <> 0L

let is_snan_d b = is_nan_d b && Int64.logand b 0x0008000000000000L = 0L

(* {1 Inexact results}

   An operation's exact (infinitely precise) result is [x * 2^scale], where
   [x] is described by its round-to-nearest-even value [r], the sign of
   [x - r], and whether [x] lies exactly halfway between [r] and its
   neighbour. [scale] is non-zero only for results that overflow: they are
   computed in a scaled-down domain, rounded there (that is, with an
   unbounded exponent, as IEEE 754 defines overflow), and scaled back. *)

type approx = { r : float; err : int; tie : bool; scale : int }

let exact r = { r; err = 0; tie = false; scale = 0 }
let sign x = if x > 0. then 1 else if x < 0. then -1 else 0

(** The float adjacent to [r] in the direction [dir] (+1 or -1). *)
let step r dir = if dir > 0 then Float.succ r else Float.pred r

let make_approx r err_val =
  let err = sign err_val in
  if err = 0 then exact r
  else
    let n = step r err in
    { r; err; tie = 2. *. Float.abs err_val = Float.abs (n -. r); scale = 0 }

(** Knuth's TwoSum: [s + e = a + b] exactly, with [s = RN(a + b)]. *)
let two_sum a b =
  let s = a +. b in
  let bb = s -. a in
  (s, a -. (s -. bb) +. (b -. bb))

(** Approximations for the basic operations, given finite operands. *)
let add a b =
  let r, e = two_sum a b in
  if Float.is_finite r then make_approx r e
  else if Float.is_finite a && Float.is_finite b then
    (* overflow: halving is exact for numbers this large *)
    let r', e' = two_sum (a /. 2.) (b /. 2.) in
    { (make_approx r' e') with scale = 1 }
  else exact r

(* {2 Exact comparisons by floating-point expansions}

   An expansion is a list of floats, in increasing magnitude, whose exact
   sum represents a number and whose components don't overlap (Shewchuk,
   "Adaptive precision floating-point arithmetic", 1997). The largest
   nonzero component determines the sign of the sum. That lets us decide
   [sign (exact - candidate)] exactly for products, quotients and roots,
   as long as every intermediate stays in the normal range. *)

let two_prod a b =
  let p = a *. b in
  (p, Float.fma a b (-.p))

(** Shewchuk's Grow-Expansion: add [b] to the expansion [e]. *)
let grow_expansion e b =
  let q, hs =
    List.fold_left
      (fun (q, hs) x ->
        let s, h = two_sum q x in
        (s, h :: hs))
      (b, []) e
  in
  List.rev (q :: hs)

(** The exact sign of a sum of floats. *)
let sign_of_sum terms =
  List.fold_left grow_expansion [] terms
  |> List.fold_left (fun acc x -> if x <> 0. then sign x else acc) 0

(** [v = m * 2^-k] with [m] in [0.5, 1): returns [(m, k)]. *)
let normalise v =
  let _, e = Float.frexp v in
  (Float.ldexp v (-e), -e)

(** Describe the exact result of an operation, given its RNE value [r] and a
    comparator [exact_minus ts = sign (exact * 2^domain - sum ts)]. *)
let approx_by_comparison r ~domain ~exact_minus =
  let up v = Float.ldexp v domain in
  let err = exact_minus [ up r ] in
  if err = 0 then exact r
  else
    let half = (up (step r err) -. up r) /. 2. in
    { r; err; tie = exact_minus [ up r; half ] = 0; scale = 0 }

let negate_all = List.map Float.neg

(* {2 Products, quotients, roots, fused multiply-add} *)

let all_finite = List.for_all Float.is_finite

let mul a b =
  let r = a *. b in
  if (not (all_finite [ a; b ])) || a = 0. || b = 0. then exact r
  else
    let a', ka = normalise a and b', kb = normalise b in
    let p, e = two_prod a' b' in
    if Float.is_finite r then
      approx_by_comparison r ~domain:(ka + kb) ~exact_minus:(fun ts ->
          sign_of_sum (p :: e :: negate_all ts))
    else { (make_approx p e) with scale = -(ka + kb) }

let div a b =
  let r = a /. b in
  if (not (all_finite [ a; b ])) || a = 0. || b = 0. then exact r
  else
    (* a'/b' - t has the sign of (a' - b' t) * sign b' *)
    let a', ka = normalise a and b', kb = normalise b in
    let exact_minus ts =
      let products =
        List.concat_map
          (fun t ->
            let p, e = two_prod b' t in
            [ -.p; -.e ])
          ts
      in
      sign_of_sum (a' :: products) * sign b'
    in
    if Float.is_finite r then
      approx_by_comparison r ~domain:(ka - kb) ~exact_minus
    else
      {
        (approx_by_comparison (a' /. b') ~domain:0 ~exact_minus) with
        scale = kb - ka;
      }

let sqrt a =
  let r = Float.sqrt a in
  if (not (Float.is_finite r)) || r <= 0. then exact r
  else
    (* scale by an even power of two: sqrt (a 2^2k) = sqrt a * 2^k *)
    let _, e = Float.frexp a in
    let k = -(e / 2) in
    let a' = Float.ldexp a (2 * k) in
    approx_by_comparison r ~domain:k ~exact_minus:(fun ts ->
        (* sqrt a' - t has the sign of a' - t^2 (t >= 0); t^2 = sum of t_i t_j *)
        let squares =
          List.concat_map
            (fun (x, y) ->
              let p, e = two_prod x y in
              [ -.p; -.e ])
            (List.concat_map (fun x -> List.map (fun y -> (x, y)) ts) ts)
        in
        sign_of_sum (a' :: squares))

(** [fma a b c] with its exact error, by Boldo and Muller's ErrFma ("Exact and
    approximated error of the FMA", IEEE TC 2011): [a*b + c = r1 + r2 + r3]
    exactly, with [r2 = RN(r2 + r3)]. So [r2] carries the error's sign, and
    there is a tie only if [r3 = 0] and [r2] is exactly half an ulp. Exact
    unless intermediates underflow; tiny results are therefore decided by exact
    comparison in a scaled domain. *)
let err_fma a b c r1 =
  let u1, u2 = two_prod a b in
  let alpha1, alpha2 = two_sum c u2 in
  let beta1, beta2 = two_sum u1 alpha1 in
  let gamma = beta1 -. r1 +. beta2 in
  let r2 = gamma +. alpha2 in
  let r3 = alpha2 -. (r2 -. gamma) in
  let ap = make_approx r1 r2 in
  { ap with tie = ap.tie && r3 = 0. }

let tiny = Float.ldexp 1. (-960)

(** An FMA whose product or result overflows: scale the larger factor and [c]
    down so nothing does. Gives up only if [c] would lose bits. *)
let scaled_fma a b c r1 =
  let exponent v = snd (Float.frexp v) in
  let k = 1000 - (exponent a + exponent b) in
  let a, b =
    if exponent a >= exponent b then (Float.ldexp a k, b)
    else (a, Float.ldexp b k)
  in
  let c' = Float.ldexp c k in
  if Float.ldexp c' (-k) = c then
    { (err_fma a b c' (Float.fma a b c')) with scale = -k }
  else { r = r1; err = -sign r1; tie = false; scale = 0 }
(* approximate *)

let fma_approx a b c =
  let r1 = Float.fma a b c in
  if not (all_finite [ a; b; c ]) then exact r1
  else if a = 0. || b = 0. then exact r1
  else if not (Float.is_finite r1 && Float.is_finite (a *. b)) then
    scaled_fma a b c r1
  else
    let a', ka = normalise a and b', kb = normalise b in
    let c' = Float.ldexp c (ka + kb) in
    let c_scales_exactly =
      Float.is_finite c' && Float.ldexp c' (-(ka + kb)) = c
    in
    if
      Float.abs r1 >= tiny || Float.abs (a *. b) >= tiny || not c_scales_exactly
    then err_fma a b c r1
    else
      let p, e = two_prod a' b' in
      approx_by_comparison r1 ~domain:(ka + kb) ~exact_minus:(fun ts ->
          sign_of_sum (p :: e :: c' :: negate_all ts))

(** Round an approximation to binary64 in mode [rm]. *)
let round_d rm { r; err; tie; _ } =
  if err = 0 then r
  else
    let toward dir = if err = dir then step r err else r in
    match rm with
    | Rne | Dyn -> r
    | Rdn -> toward (-1)
    | Rup -> toward 1
    | Rtz ->
        (* toward zero: only move if the exact value is closer to zero *)
        if (r > 0. && err < 0) || (r < 0. && err > 0) then step r err else r
    | Rmm ->
        (* ties away from zero; RNE already chose correctly otherwise *)
        if tie && Float.abs (step r err) > Float.abs r then step r err else r

(** The IEEE 754 overflow result for each rounding mode. *)
let overflow_result rm ~negative =
  let inf = if negative then Float.neg_infinity else Float.infinity in
  let max = if negative then -.Float.max_float else Float.max_float in
  match rm with
  | Rne | Rmm | Dyn -> inf
  | Rtz -> max
  | Rdn -> if negative then inf else max
  | Rup -> if negative then max else inf

(** Round to binary64, returning the result and whether it overflowed. *)
let round_double rm a =
  let r' = round_d rm a in
  let r = Float.ldexp r' a.scale in
  if Float.is_finite r || (a.err = 0 && a.scale = 0) then (r, false)
  else (overflow_result rm ~negative:(Float.sign_bit r'), true)

(** Round to odd: truncate, then set the lsb if inexact. Only used for binary32
    results, which never need scaling in binary64. *)
let round_odd ({ r; err; _ } as a) =
  if err = 0 then r
  else
    let t = round_d Rtz a in
    if Float.is_finite t then
      Int64.float_of_bits (Int64.logor (Int64.bits_of_float t) 1L)
    else t

(* {1 Single precision} *)

let s_of_d (d : float) : int32 = Int32.bits_of_float d
let d_of_s (b : int32) : float = Int32.float_of_bits b

(** Next binary32 value up/down from [b] (by value). *)
let next_s b dir =
  let f = d_of_s b in
  if Float.is_nan f then b
  else if f = 0. then if dir > 0 then 1l else 0x80000001l
  else
    let positive = Int32.compare b 0l >= 0 in
    if positive = (dir > 0) then Int32.add b 1l else Int32.sub b 1l

(** Round binary64 [d] (exact, or round-to-odd) to binary32 in mode [rm]. *)
let round_s rm (d : float) : int32 =
  let s = s_of_d d in
  let sf = d_of_s s in
  if Float.is_nan d || sf = d then s
  else
    let lo, hi = if sf < d then (s, next_s s 1) else (next_s s (-1), s) in
    let lo_f = d_of_s lo and hi_f = d_of_s hi in
    match rm with
    | Rne | Dyn -> s
    | Rdn -> lo
    | Rup -> hi
    | Rtz -> if d > 0. then lo else hi
    | Rmm -> if d -. lo_f = hi_f -. d then if d > 0. then hi else lo else s

(* {1 Flags for a rounded result} *)

let min_normal_d = Float.min_float
let min_normal_s = 1.17549435082228751e-38

let rounding_flags ~min_normal ~inexact ~overflow result =
  let nx_ = inexact || overflow in
  let uf_ = nx_ && (not overflow) && Float.abs result < min_normal in
  (if overflow then ofl else 0)
  lor (if nx_ then nx else 0)
  lor if uf_ then ufl else 0

(* {1 Integer conversion} *)

let round_to_integral rm x =
  match rm with
  | Rtz -> Float.trunc x
  | Rdn -> Float.floor x
  | Rup -> Float.ceil x
  | Rmm -> Float.round x
  | Rne | Dyn ->
      let r = Float.round x in
      if Float.abs (x -. Float.trunc x) = 0.5 then 2. *. Float.round (x /. 2.)
      else r

let two63 = 9223372036854775808.
let two64 = 18446744073709551616.

(** Convert a float to an integer type. Returns the 64-bit register value (W
    results sign-extended) and the flags raised. *)
let to_int rm ity (x : float) : int64 * int =
  let lo, hi, nan_result, lo_result, hi_result =
    match ity with
    | I32 -> (-2147483648., 2147483647., 0x7fffffffL, -0x80000000L, 0x7fffffffL)
    | U32 -> (0., 4294967295., -1L, 0L, -1L) (* 0xffffffff sign-extended *)
    | I64 -> (-.two63, two63, Int64.max_int, Int64.min_int, Int64.max_int)
    | U64 -> (0., two64, -1L, 0L, -1L)
  in
  if Float.is_nan x then (nan_result, nv)
  else
    let r = round_to_integral rm x in
    let too_big = match ity with I64 | U64 -> r >= hi | _ -> r > hi in
    if r < lo then (lo_result, nv)
    else if too_big then (hi_result, nv)
    else
      let flags = if r <> x then nx else 0 in
      let v =
        match ity with
        | I32 | I64 -> Int64.of_float r
        | U32 -> Int64.of_int32 (Int64.to_int32 (Int64.of_float r))
        | U64 ->
            if r >= two63 then
              Int64.add (Int64.of_float (r -. two63)) Int64.min_int
            else Int64.of_float r
      in
      (v, flags)

(** Approximation of an integer register value as a float. *)
let approx_of_int ity (v : int64) : approx =
  let unsigned_to_float v =
    if Int64.compare v 0L >= 0 then Int64.to_float v
    else
      (* halve with sticky bit, convert, double: correctly rounded *)
      let h = Int64.logor (Int64.shift_right_logical v 1) (Int64.logand v 1L) in
      2. *. Int64.to_float h
  in
  let v, to_float, unsigned =
    match ity with
    | I32 -> (Int64.of_int32 (Int64.to_int32 v), Int64.to_float, false)
    | U32 -> (Int64.logand v 0xffffffffL, Int64.to_float, false)
    | I64 -> (v, Int64.to_float, false)
    | U64 -> (v, unsigned_to_float, true)
  in
  let r = to_float v in
  (* exact - r, computed in integers; r is within 2^11 of v *)
  let back =
    if unsigned then
      if r >= two63 then Int64.add (Int64.of_float (r -. two63)) Int64.min_int
      else Int64.of_float r
    else if r >= two63 then Int64.max_int (* r = 2^63, v < r *)
    else Int64.of_float r
  in
  let diff =
    if (not unsigned) && r >= two63 then -1.
    else Int64.to_float (Int64.sub v back)
  in
  make_approx r diff

(* {1 Classification} *)

(** fclass bit index: 0 -inf, 1 -normal, 2 -subnormal, 3 -0, 4 +0, 5 +subnormal,
    6 +normal, 7 +inf, 8 sNaN, 9 qNaN. *)
let class_bit ~neg = function
  | `Inf -> if neg then 0 else 7
  | `Normal -> if neg then 1 else 6
  | `Subnormal -> if neg then 2 else 5
  | `Zero -> if neg then 3 else 4

let fclass_d bits =
  if is_snan_d bits then 1 lsl 8
  else if is_nan_d bits then 1 lsl 9
  else
    let x = Int64.float_of_bits bits in
    let kind =
      match Float.classify_float x with
      | FP_infinite -> `Inf
      | FP_normal -> `Normal
      | FP_subnormal -> `Subnormal
      | FP_zero | FP_nan -> `Zero
    in
    1 lsl class_bit ~neg:(Float.sign_bit x) kind

let fclass_s bits =
  if is_snan_s bits then 1 lsl 8
  else if is_nan_s bits then 1 lsl 9
  else
    (* subnormal singles are normal doubles, so classify by single range *)
    let ax = Float.abs (d_of_s bits) in
    let kind =
      if ax = Float.infinity then `Inf
      else if ax = 0. then `Zero
      else if ax < min_normal_s then `Subnormal
      else `Normal
    in
    1 lsl class_bit ~neg:(Int32.compare bits 0l < 0) kind

(* {1 Display} *)

(** Shortest decimal that round-trips. *)
let shortest_string ~round_trips x =
  if Float.is_nan x then "NaN"
  else if Float.is_integer x && Float.abs x < 1e16 then Printf.sprintf "%.1f" x
  else
    let rec go p =
      let s = Printf.sprintf "%.*g" p x in
      if p >= 17 || round_trips (float_of_string s) then s else go (p + 1)
    in
    go 1

let string_of_double x = shortest_string ~round_trips:(fun y -> y = x) x

let string_of_single b =
  let x = d_of_s b in
  shortest_string ~round_trips:(fun y -> s_of_d y = b) x

(* {1 Format-generic operations on bit patterns}

   ['b] is [int64] for binary64 and [int32] for binary32. Every operation
   returns the result bits and the fflags it raises. *)

type 'b format = {
  to_float : 'b -> float;  (** exact; only called on non-NaN values *)
  is_nan : 'b -> bool;
  is_snan : 'b -> bool;
  canonical : 'b;
  round : rm -> approx -> 'b * bool * bool;  (** bits, inexact, overflow *)
  min_normal : float;
  narrow : bool;  (** binary32: products of two operands are exact in binary64 *)
}

let double =
  {
    to_float = Int64.float_of_bits;
    is_nan = is_nan_d;
    is_snan = is_snan_d;
    canonical = canonical_nan_d;
    round =
      (fun rm a ->
        let r, overflow = round_double rm a in
        (Int64.bits_of_float r, a.err <> 0, overflow));
    min_normal = min_normal_d;
    narrow = false;
  }

let single =
  {
    to_float = d_of_s;
    is_nan = is_nan_s;
    is_snan = is_snan_s;
    canonical = canonical_nan_s;
    round =
      (fun rm a ->
        let d = round_odd a in
        let s = round_s rm d in
        let inexact = a.err <> 0 || d_of_s s <> d in
        (* overflow iff rounding with an unbounded exponent exceeds max *)
        let overflow =
          inexact && (Float.abs d >= 0x1p128 || not (Float.is_finite (d_of_s s)))
        in
        (s, inexact, overflow));
    min_normal = min_normal_s;
    narrow = true;
  }

let finish fm rm ?(extra = 0) ap =
  if Float.is_nan ap.r then (fm.canonical, nv lor extra)
  else
    let bits, inexact, overflow = fm.round rm ap in
    ( bits,
      extra
      lor rounding_flags ~min_normal:fm.min_normal ~inexact ~overflow
            (fm.to_float bits) )

(** IEEE 754 6.3: when a sum is exactly zero and the addends aren't both +0, the
    result is +0 except under round-toward-negative, where it is -0. The host
    already computed the RNE answer, so only RDN needs fixing. *)
let signed_zero_sum rm (p, q) ap =
  let is_pos_zero v = v = 0. && not (Float.sign_bit v) in
  if rm = Rdn && ap.err = 0 && ap.r = 0. && not (is_pos_zero p && is_pos_zero q)
  then { ap with r = -0. }
  else ap

let snan_flag fm xs = if List.exists fm.is_snan xs then nv else 0

let arith fm rm op a b =
  if fm.is_nan a || fm.is_nan b then (fm.canonical, snan_flag fm [ a; b ])
  else
    let x = fm.to_float a and y = fm.to_float b in
    let ap =
      match op with
      | Fadd -> signed_zero_sum rm (x, y) (add x y)
      | Fsub -> signed_zero_sum rm (x, -.y) (add x (-.y))
      | Fmul -> mul x y
      | Fdiv -> div x y
    in
    let extra =
      if op = Fdiv && y = 0. && Float.is_finite x && x <> 0. then dz else 0
    in
    finish fm rm ~extra ap

let fsqrt fm rm a =
  if fm.is_nan a then (fm.canonical, snan_flag fm [ a ])
  else
    let x = fm.to_float a in
    if x < 0. then (fm.canonical, nv) else finish fm rm (sqrt x)

let fma fm rm op a b c =
  if List.exists fm.is_nan [ a; b; c ] then
    let inf_times_zero =
      (not (fm.is_nan a || fm.is_nan b))
      &&
      let x = fm.to_float a and y = fm.to_float b in
      let is_inf v = Float.abs v = Float.infinity in
      (x = 0. && is_inf y) || (y = 0. && is_inf x)
    in
    (fm.canonical, snan_flag fm [ a; b; c ] lor if inf_times_zero then nv else 0)
  else
    let x = fm.to_float a and y = fm.to_float b and z = fm.to_float c in
    let x = match op with Fnmsub | Fnmadd -> -.x | _ -> x in
    let z = match op with Fmsub | Fnmadd -> -.z | _ -> z in
    (* binary32: the product is exact in binary64, so TwoSum is exact *)
    let ap = if fm.narrow then add (x *. y) z else fma_approx x y z in
    finish fm rm (signed_zero_sum rm (x *. y, z) ap)

let min_max fm ~is_max a b =
  let flags = snan_flag fm [ a; b ] in
  match (fm.is_nan a, fm.is_nan b) with
  | true, true -> (fm.canonical, flags)
  | true, false -> (b, flags)
  | false, true -> (a, flags)
  | false, false ->
      let x = fm.to_float a and y = fm.to_float b in
      let a_first =
        if x = y then Float.sign_bit x <> is_max (* -0 < +0 *)
        else x < y <> is_max
      in
      ((if a_first then a else b), flags)

let compare fm op a b =
  if fm.is_nan a || fm.is_nan b then
    (false, if op = Feq then snan_flag fm [ a; b ] else nv)
  else
    let x = fm.to_float a and y = fm.to_float b in
    ((match op with Feq -> x = y | Flt -> x < y | Fle -> x <= y), 0)

let to_int_bits fm rm ity a =
  if fm.is_nan a then to_int rm ity Float.nan else to_int rm ity (fm.to_float a)

let of_int fm rm ity v = finish fm rm (approx_of_int ity v)

let d_to_s rm (a : int64) =
  if is_nan_d a then (canonical_nan_s, snan_flag double [ a ])
  else finish single rm (exact (Int64.float_of_bits a))

let s_to_d (a : int32) =
  if is_nan_s a then (canonical_nan_d, snan_flag single [ a ])
  else (Int64.bits_of_float (d_of_s a), 0)

(** Sign injection on raw bits. *)
let sgnj_d op a b =
  let sign = Int64.min_int in
  let sb =
    match op with
    | Fsgnj -> Int64.logand b sign
    | Fsgnjn -> Int64.logand (Int64.lognot b) sign
    | _ (* Fsgnjx *) -> Int64.logand (Int64.logxor a b) sign
  in
  Int64.logor (Int64.logand a Int64.max_int) sb

let sgnj_s op a b =
  let sign = Int32.min_int in
  let sb =
    match op with
    | Fsgnj -> Int32.logand b sign
    | Fsgnjn -> Int32.logand (Int32.lognot b) sign
    | _ -> Int32.logand (Int32.logxor a b) sign
  in
  Int32.logor (Int32.logand a Int32.max_int) sb
