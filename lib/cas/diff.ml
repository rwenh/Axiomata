(** Symbolic differentiation via the standard rules (linearity, product,
    quotient, chain) plus the power rule for a constant exponent and the
    fully general [f^g] rule (via logarithmic differentiation). *)

open Ast

type diff_error = Unknown_function of string

let error_to_string : diff_error -> string = function
  | Unknown_function name ->
      Printf.sprintf "don't know how to differentiate '%s'" name

let ( let* ) x f = match x with Ok v -> f v | Error _ as e -> e
let ( let+ ) x f = Result.map f x

(** [fun_derivative name a] is d/d[a] of [name(a)] -- the derivative with
    respect to the function's argument, not yet multiplied by [da/dx]
    (that happens once in {!diff}'s [Fun] case; the chain rule is applied
    uniformly). *)
let fun_derivative (name : string) (a : expr) : (expr, diff_error) result =
  match name with
  | "sin" -> Ok (Fun ("cos", a))
  | "cos" -> Ok (Neg (Fun ("sin", a)))
  | "tan" -> Ok (Div (Num 1.0, Pow (Fun ("cos", a), Num 2.0)))
  | "exp" -> Ok (Fun ("exp", a))
  | "ln" -> Ok (Div (Num 1.0, a))
  | "log" -> Ok (Div (Num 1.0, Mul (a, Num (log 10.0))))
  | "sqrt" -> Ok (Div (Num 1.0, Mul (Num 2.0, Fun ("sqrt", a))))
  | "abs" -> Ok (Div (a, Fun ("abs", a)))
  | other -> Error (Unknown_function other)

let rec diff (var : string) (e : expr) : (expr, diff_error) result =
  match e with
  | Num _ -> Ok (Num 0.0)
  | Var v -> Ok (Num (if String.equal v var then 1.0 else 0.0))
  | Neg a ->
      let+ da = diff var a in
      Neg da
  | Add (a, b) ->
      let* da = diff var a in
      let+ db = diff var b in
      Add (da, db)
  | Sub (a, b) ->
      let* da = diff var a in
      let+ db = diff var b in
      Sub (da, db)
  | Mul (a, b) ->
      let* da = diff var a in
      let+ db = diff var b in
      Add (Mul (da, b), Mul (a, db))
  | Div (a, b) ->
      let* da = diff var a in
      let+ db = diff var b in
      Div (Sub (Mul (da, b), Mul (a, db)), Pow (b, Num 2.0))
  | Pow (base, Num n) ->
      let+ dbase = diff var base in
      Mul (Mul (Num n, Pow (base, Num (n -. 1.0))), dbase)
  | Pow (base, exponent) ->
      (* General case by logarithmic differentiation:
           d/dx(f^g) = f^g * (g' * ln(f) + g * f'/f).
         The constant-exponent case is handled separately above so the
         common case does not produce [ln]. *)
      let* dbase = diff var base in
      let+ dexp = diff var exponent in
      Mul
        ( Pow (base, exponent),
          Add
            ( Mul (dexp, Fun ("ln", base)),
              Mul (exponent, Div (dbase, base)) ) )
  | Fun (name, a) ->
      let* da = diff var a in
      let+ d = fun_derivative name a in
      Mul (d, da)
