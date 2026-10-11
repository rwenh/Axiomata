(** Render an {!Ast.expr} back to a string that re-parses to (at least)
    the same value, adding parentheses only where precedence genuinely
    requires them.

    The precedence numbers below are exactly the levels implied by
    {!Parser}'s grammar (unary minus sits between additive/multiplicative
    on one side and [^] on the other), which is what makes the round trip
    -- parse, print, reparse -- land on an equivalent tree.

    One deliberate simplification: for left-associative [+]/[-] and
    [*]/[/], this always parenthesises a same-precedence right operand,
    even in cases like [a + (b - c)] where the parens aren't strictly
    required. Fully minimal parenthesisation needs to distinguish "same
    precedence, same operator" from "same precedence, different operator",
    which is more machinery than a mini pretty-printer needs. *)

open Ast

let prec_add = 1 (* + - *)
let prec_mul = 2 (* * / *)
let prec_neg = 3 (* unary - *)
let prec_pow = 4 (* ^ *)

let format_num (n : float) : string =
  if Float.is_nan n then "nan"
  else if Float.is_integer n && Float.abs n < 1e15 then
    Printf.sprintf "%.0f" n
  else Printf.sprintf "%.12g" n

let rec go (min_prec : int) (e : expr) : string =
  let wrap prec s = if prec < min_prec then "(" ^ s ^ ")" else s in
  match e with
  | Num n -> format_num n
  | Var v -> v
  | Neg a ->
      let inner = go prec_neg a in
      let inner =
        if String.length inner > 0 && Char.equal inner.[0] '-' then
          " " ^ inner
        else inner
      in
      wrap prec_neg ("-" ^ inner)
  | Add (a, b) ->
      wrap prec_add (go prec_add a ^ " + " ^ go (prec_add + 1) b)
  | Sub (a, b) ->
      wrap prec_add (go prec_add a ^ " - " ^ go (prec_add + 1) b)
  | Mul (a, b) ->
      wrap prec_mul (go prec_mul a ^ " * " ^ go (prec_mul + 1) b)
  | Div (a, b) ->
      wrap prec_mul (go prec_mul a ^ " / " ^ go (prec_mul + 1) b)
  | Pow (a, b) ->
      wrap prec_pow (go (prec_pow + 1) a ^ "^" ^ go prec_pow b)
  | Fun (name, a) -> name ^ "(" ^ go 0 a ^ ")"

let to_string (e : expr) : string = go 0 e
