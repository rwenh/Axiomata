(** The expression grammar, as a set of parser-combinator productions:

    {v
    expr    := term (('+' | '-') term)*
    term    := unary (('*' | '/') unary)*
    unary   := '-'* power           (* zero or more leading minuses *)
    power   := atom ('^' unary)?    (* right-associative *)
    atom    := number
             | ident ('(' expr ')')?  (* a bare name, or a fn call *)
             | '(' expr ')'
    v}

    Unary minus binds looser than [^], so [-2^2] parses as [-(2^2)], and
    [^] is right-associative, so [2^3^2] parses as [2^(3^2)] -- both match
    ordinary mathematical convention. Multiplication is never implicit:
    "2x" is a syntax error; write "2*x". A function call is just "an
    identifier immediately followed by a parenthesised expression": the
    parser doesn't check the name against the known list of functions
    (that happens later in {!Eval} and {!Diff}) -- syntax and semantics
    are deliberately separated. *)

open Combinators
open Combinators.Let_syntax
open Combinators.Infix

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_ident_start c = is_alpha c || Char.equal c '_'
let is_ident_char c = is_ident_start c || is_digit c

(* --- lexical productions --- *)

let number : Ast.expr t =
  let digits = take_while1 is_digit in
  let frac =
    option ""
      (let* _ = char '.' in
       let+ ds = digits in
       "." ^ ds)
  in
  let exponent =
    option ""
      (let* _ = one_of "eE" in
       let* sign = option "" (map (String.make 1) (one_of "+-")) in
       let+ ds = digits in
       "e" ^ sign ^ ds)
  in
  lexeme
    (let* whole = digits in
     let* frac_part = frac in
     let+ exp_part = exponent in
     Ast.Num (float_of_string (whole ^ frac_part ^ exp_part)))

let ident : string t =
  lexeme
    (let* first = satisfy is_ident_start in
     let+ rest = take_while is_ident_char in
     String.make 1 first ^ rest)

let lparen = token "("
let rparen = token ")"

(* --- the recursive core ---
   [expr], [term], [power], [unary] and [atom] are mutually recursive.
   Combinators.t is abstract, so we use ref cells + delay. *)

let expr_ref : Ast.expr t ref = ref (fail "expr: grammar not yet initialized")
let term_ref : Ast.expr t ref = ref (fail "term: grammar not yet initialized")
let power_ref : Ast.expr t ref = ref (fail "power: grammar not yet initialized")
let unary_ref : Ast.expr t ref = ref (fail "unary: grammar not yet initialized")
let atom_ref : Ast.expr t ref = ref (fail "atom: grammar not yet initialized")

let expr = delay (fun () -> !expr_ref)
let term = delay (fun () -> !term_ref)
let power = delay (fun () -> !power_ref)
let unary = delay (fun () -> !unary_ref)
let atom = delay (fun () -> !atom_ref)

let () =
  atom_ref :=
    choice
      [
        number;
        (let* name = ident in
         choice
           [
             (let* _ = lparen in
              let* arg = expr in
              let+ _ = rparen in
              Ast.Fun (name, arg));
             return (Ast.Var name);
           ]);
        (let* _ = lparen in
         let* e = expr in
         let+ _ = rparen in
         e);
      ]

let () =
  power_ref :=
    (let* base = atom in
     let* rhs = optional (token "^" *> unary) in
     return (match rhs with None -> base | Some e -> Ast.Pow (base, e)))

let () =
  unary_ref :=
    (let* signs = many (token "-") in
     let+ p = power in
     List.fold_left (fun acc _ -> Ast.Neg acc) p signs)

let () =
  term_ref :=
    chainl1 unary
      (choice
         [
           (token "*" >>| fun _ -> fun a b -> Ast.Mul (a, b));
           (token "/" >>| fun _ -> fun a b -> Ast.Div (a, b));
         ])

let () =
  expr_ref :=
    chainl1 term
      (choice
         [
           (token "+" >>| fun _ -> fun a b -> Ast.Add (a, b));
           (token "-" >>| fun _ -> fun a b -> Ast.Sub (a, b));
         ])

(** Parse a full expression, allowing (and skipping) leading whitespace.
    Fails unless the whole string is consumed. *)
let parse_expr (s : string) : (Ast.expr, error) result =
  parse (spaces *> expr) s
