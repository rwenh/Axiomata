open Cas

let parse_exn s =
  match Parser.parse_expr s with
  | Ok e -> e
  | Error { Combinators.pos; msg } ->
      failwith (Printf.sprintf "parse error for %S at %d: %s" s pos msg)

let eval_exn ?(env = Env.default) s =
  match Eval.eval env (parse_exn s) with
  | Ok v -> v
  | Error e ->
      failwith
        (Printf.sprintf "eval error for %S: %s" s (Eval.error_to_string e))

let check_float = Alcotest.(check (float 1e-9))
let check_close = Alcotest.(check (float 1e-6))

(* ---------- parser: structure and precedence ---------- *)

let test_parses_arithmetic () =
  Alcotest.(check bool) "simple sum" true
    (Ast.equal (parse_exn "1 + 2") (Ast.Add (Ast.Num 1., Ast.Num 2.)))

let test_precedence_mul_over_add () =
  Alcotest.(check bool) "* binds tighter than +" true
    (Ast.equal (parse_exn "1 + 2 * 3")
       (Ast.Add (Ast.Num 1., Ast.Mul (Ast.Num 2., Ast.Num 3.))))

let test_unary_minus_than_pow () =
  (* -2^2 = -(2^2) = -4, matching ordinary mathematical convention *)
  Alcotest.(check bool) "-2^2 parses as -(2^2)" true
    (Ast.equal (parse_exn "-2^2") (Ast.Neg (Ast.Pow (Ast.Num 2., Ast.Num 2.))));
  check_float "and therefore evaluates to -4" (-4.0) (eval_exn "-2^2")

let test_pow_is_right_associative () =
  Alcotest.(check bool) "2^3^2 parses as 2^(3^2)" true
    (Ast.equal (parse_exn "2^3^2")
       (Ast.Pow (Ast.Num 2., Ast.Pow (Ast.Num 3., Ast.Num 2.))));
  check_float "and therefore is 512, not 64" 512.0 (eval_exn "2^3^2")

let test_function_call () =
  Alcotest.(check bool) "sin(x) parses as a call" true
    (Ast.equal (parse_exn "sin(x)") (Ast.Fun ("sin", Ast.Var "x")))

let test_parenthesised_grouping () =
  check_float "parens override precedence" 9.0 (eval_exn "(1 + 2) * 3")

let test_negative_exponent () =
  check_close "2^-2 = 0.25" 0.25 (eval_exn "2^-2")

let test_parse_error_on_incomplete_input () =
  match Parser.parse_expr "1 + " with
  | Ok _ -> Alcotest.fail "trailing operator with no operand should be a syntax error"
  | Error _ -> ()

let test_no_implicit_multiplication () =
  match Parser.parse_expr "2x" with
  | Ok _ -> Alcotest.fail "implicit multiplication is not part of this grammar"
  | Error _ -> ()

(* ---------- evaluator ---------- *)

let test_eval_arithmetic () =
  check_float "arithmetic with precedence" 14.0 (eval_exn "2 + 3 * 4")

let test_eval_variables () =
  let env = Env.add "x" 2.0 Env.default in
  check_float "x^2 + 1 at x=2" 5.0 (eval_exn ~env "x^2 + 1")

let test_eval_unbound_variable () =
  match Eval.eval Env.default (parse_exn "y") with
  | Error (Eval.Unbound_variable "y") -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Eval.error_to_string e)
  | Ok _ -> Alcotest.fail "y should be unbound"

let test_eval_division_by_zero () =
  match Eval.eval Env.default (parse_exn "1 / 0") with
  | Error Eval.Division_by_zero -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Eval.error_to_string e)
  | Ok _ -> Alcotest.fail "1/0 should fail"

let test_eval_domain_error () =
  match Eval.eval Env.default (parse_exn "sqrt(-1)") with
  | Error (Eval.Domain_error _) -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Eval.error_to_string e)
  | Ok _ -> Alcotest.fail "sqrt(-1) should fail"

let test_eval_constants () =
  check_close "pi is close to 3.14159" 3.14159265 (eval_exn "pi");
  check_close "e^0 = 1" 1.0 (eval_exn "exp(0)")

(* ---------- simplifier ---------- *)

let test_simplify_constant_folding () =
  Alcotest.(check bool) "2+3 folds to 5" true
    (Ast.equal (Simplify.simplify (parse_exn "2 + 3")) (Ast.Num 5.))

let test_simplify_identities () =
  let check name input expected =
    Alcotest.(check bool) name true
      (Ast.equal (Simplify.simplify (parse_exn input)) expected)
  in
  check "x + 0 = x" "x + 0" (Ast.Var "x");
  check "0 + x = x" "0 + x" (Ast.Var "x");
  check "x * 1 = x" "x * 1" (Ast.Var "x");
  check "x * 0 = 0" "x * 0" (Ast.Num 0.);
  check "x - x = 0" "x - x" (Ast.Num 0.);
  check "x / x = 1" "x / x" (Ast.Num 1.);
  check "x^1 = x" "x^1" (Ast.Var "x");
  check "x^0 = 1" "x^0" (Ast.Num 1.);
  check "double negation cancels" "- -x" (Ast.Var "x")

let test_simplify_preserves_zero_over_zero () =
  (* the x/x=1 identity must not fire when x is literally 0: 0/0 is not 1 *)
  Alcotest.(check bool) "0/0 is left alone, not folded to 1" true
    (Ast.equal
       (Simplify.simplify (Ast.Div (Ast.Num 0., Ast.Num 0.)))
       (Ast.Div (Ast.Num 0., Ast.Num 0.)))

let tests =
  [
    ("parses arithmetic", `Quick, test_parses_arithmetic);
    ("precedence * over +", `Quick, test_precedence_mul_over_add);
    ("unary minus vs power", `Quick, test_unary_minus_than_pow);
    ("power right-associative", `Quick, test_pow_is_right_associative);
    ("function call", `Quick, test_function_call);
    ("parenthesised grouping", `Quick, test_parenthesised_grouping);
    ("negative exponent", `Quick, test_negative_exponent);
    ("parse error incomplete", `Quick, test_parse_error_on_incomplete_input);
    ("no implicit mult", `Quick, test_no_implicit_multiplication);
    ("eval arithmetic", `Quick, test_eval_arithmetic);
    ("eval variables", `Quick, test_eval_variables);
    ("eval unbound", `Quick, test_eval_unbound_variable);
    ("eval div by zero", `Quick, test_eval_division_by_zero);
    ("eval domain error", `Quick, test_eval_domain_error);
    ("eval constants", `Quick, test_eval_constants);
    ("simplify constant fold", `Quick, test_simplify_constant_folding);
    ("simplify identities", `Quick, test_simplify_identities);
    ("simplify 0/0", `Quick, test_simplify_preserves_zero_over_zero);
  ]
