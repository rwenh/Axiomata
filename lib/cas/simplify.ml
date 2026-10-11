(** Symbolic simplification: constant folding ([2 + 3] becomes [5]) plus a
    handful of algebraic identities ([x + 0], [x * 1], [x * 0], [x - x],
    [x / x], [x ^ 0], [x ^ 1], double negation). Each is applied bottom-up
    in a single pass ({!simplify_once}); {!simplify} repeats that pass to a
    fixed point, because simplifying children can expose a new pattern at
    their parent (e.g. [(x + 0) * 1] needs two passes). *)

open Ast

let is_num = function Num _ -> true | _ -> false

let rec simplify_once (e : expr) : expr =
  match e with
  | Num _ | Var _ -> e
  | Neg a -> (
      match simplify_once a with
      | Num n -> Num (-.n)
      | Neg a' -> a' (* double negation *)
      | a' -> Neg a')
  | Add (a, b) -> (
      match (simplify_once a, simplify_once b) with
      | Num x, Num y -> Num (x +. y)
      | Num n, b' when n = 0.0 -> b'
      | a', Num n when n = 0.0 -> a'
      | a', b' -> Add (a', b'))
  | Sub (a, b) -> (
      match (simplify_once a, simplify_once b) with
      | Num x, Num y -> Num (x -. y)
      | a', Num n when n = 0.0 -> a'
      | a', b' when equal a' b' -> Num 0.0
      | a', b' -> Sub (a', b'))
  | Mul (a, b) -> (
      match (simplify_once a, simplify_once b) with
      | Num x, Num y -> Num (x *. y)
      | Num n, _ when n = 0.0 -> Num 0.0
      | _, Num n when n = 0.0 -> Num 0.0
      | Num n, b' when n = 1.0 -> b'
      | a', Num n when n = 1.0 -> a'
      | a', b' -> Mul (a', b'))
  | Div (a, b) -> (
      match (simplify_once a, simplify_once b) with
      | Num x, Num y when y <> 0.0 -> Num (x /. y)
      | a', Num n when n = 1.0 -> a'
      | a', b' when equal a' b' && not (is_num a') -> Num 1.0
      | a', b' -> Div (a', b'))
  | Pow (a, b) -> (
      match (simplify_once a, simplify_once b) with
      | Num x, Num y -> Num (x ** y)
      | _, Num n when n = 0.0 -> Num 1.0
      | a', Num n when n = 1.0 -> a'
      | a', b' -> Pow (a', b'))
  | Fun (name, a) -> (
      match simplify_once a with
      | Num x -> (
          match Eval.apply_fun name x with
          | Ok y -> Num y
          | Error _ -> Fun (name, Num x))
      | a' -> Fun (name, a'))

(** Repeat {!simplify_once} until it stops changing anything (or a
    generous iteration cap is hit -- real expressions converge in a
    handful of passes). *)
let simplify (e : expr) : expr =
  let rec loop e fuel =
    if fuel <= 0 then e
    else
      let e' = simplify_once e in
      if equal e e' then e else loop e' (fuel - 1)
  in
  loop e 100
