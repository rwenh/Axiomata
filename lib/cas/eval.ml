(** Numeric evaluation. No exceptions: every way an expression can fail to
    have a value -- an unbound variable, an unknown function, division by
    zero, a function applied outside its domain -- is a constructor of
    {!eval_error} threaded through [result]'s [Ok]/[Error] via a tiny
    local [let*]. *)

type eval_error =
  | Unbound_variable of string
  | Unknown_function of string
  | Division_by_zero
  | Domain_error of string

let error_to_string : eval_error -> string = function
  | Unbound_variable v -> Printf.sprintf "unbound variable '%s'" v
  | Unknown_function f -> Printf.sprintf "unknown function '%s'" f
  | Division_by_zero -> "division by zero"
  | Domain_error msg -> Printf.sprintf "domain error: %s" msg

(** The functions the language understands, and how to evaluate each on a
    float. Shared with {!Simplify}, which uses it to constant-fold
    expressions like [sin(0)] once the argument reduces to a number. *)
let apply_fun (name : string) (x : float) : (float, eval_error) result =
  match name with
  | "sin" -> Ok (sin x)
  | "cos" -> Ok (cos x)
  | "tan" -> Ok (tan x)
  | "exp" -> Ok (exp x)
  | "abs" -> Ok (abs_float x)
  | "sqrt" ->
      if x < 0.0 then
        Error (Domain_error (Printf.sprintf "sqrt of negative number %g" x))
      else Ok (sqrt x)
  | "ln" ->
      if x <= 0.0 then
        Error (Domain_error (Printf.sprintf "ln of non-positive number %g" x))
      else Ok (log x)
  | "log" ->
      if x <= 0.0 then
        Error (Domain_error (Printf.sprintf "log of non-positive number %g" x))
      else Ok (log10 x)
  | other -> Error (Unknown_function other)

let ( let* ) x f = match x with Ok v -> f v | Error _ as e -> e

let rec eval (env : Env.t) (e : Ast.expr) : (float, eval_error) result =
  match e with
  | Ast.Num n -> Ok n
  | Ast.Var v -> (
      match Env.find v env with
      | Some x -> Ok x
      | None -> Error (Unbound_variable v))
  | Ast.Neg a ->
      let* x = eval env a in
      Ok (-.x)
  | Ast.Add (a, b) ->
      let* x = eval env a in
      let* y = eval env b in
      Ok (x +. y)
  | Ast.Sub (a, b) ->
      let* x = eval env a in
      let* y = eval env b in
      Ok (x -. y)
  | Ast.Mul (a, b) ->
      let* x = eval env a in
      let* y = eval env b in
      Ok (x *. y)
  | Ast.Div (a, b) ->
      let* x = eval env a in
      let* y = eval env b in
      if y = 0.0 then Error Division_by_zero else Ok (x /. y)
  | Ast.Pow (a, b) ->
      let* x = eval env a in
      let* y = eval env b in
      Ok (x ** y)
  | Ast.Fun (name, a) ->
      let* x = eval env a in
      apply_fun name x
