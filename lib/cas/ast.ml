(** The expression language: arithmetic over floats, variables, and a small
    set of named unary functions (see {!Eval.apply_fun} and
    {!Diff.fun_derivative}).

    This is the one data type the parser produces and every other part of
    the CAS -- evaluator, simplifier, differentiator, pretty-printer --
    pattern matches over. *)

type expr =
  | Num of float
  | Var of string
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Fun of string * expr

(** Structural equality on expr. [Num] compares its floats with
    [Float.equal] (so [nan] compares equal to itself, unlike [( = )]);
    used by {!Simplify} to detect a fixed point, and to spot patterns like
    [x - x] or [x / x]. *)
let rec equal (a : expr) (b : expr) : bool =
  match (a, b) with
  | Num x, Num y -> Float.equal x y
  | Var x, Var y -> String.equal x y
  | Neg x, Neg y -> equal x y
  | Add (a1, b1), Add (a2, b2)
  | Sub (a1, b1), Sub (a2, b2)
  | Mul (a1, b1), Mul (a2, b2)
  | Div (a1, b1), Div (a2, b2)
  | Pow (a1, b1), Pow (a2, b2) -> equal a1 a2 && equal b1 b2
  | Fun (f1, a1), Fun (f2, a2) -> String.equal f1 f2 && equal a1 a2
  | _, _ -> false
