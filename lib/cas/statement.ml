(** One line of REPL/TUI input is a {!t}, not just an {!Ast.expr}: it might
    be a variable assignment or a meta-command like [:diff], neither of
    which is part of the mathematical expression grammar in {!Parser}. *)

open Combinators
open Combinators.Let_syntax
open Combinators.Infix

type t =
  | Assign of string * Ast.expr
  | Eval of Ast.expr
  | Simplify of Ast.expr
  | Diff of Ast.expr * string
  | Vars
  | Clear
  | Reset
  | Help
  | Quit

let assign : t Combinators.t =
  let* name = Parser.ident in
  let* _ = token "=" in
  let+ e = Parser.expr in
  Assign (name, e)

let diff_stmt : t Combinators.t =
  let* _ = token ":diff" in
  let* e = Parser.expr in
  let* _ = token "wrt" in
  let+ v = Parser.ident in
  Diff (e, v)

let simplify_stmt : t Combinators.t =
  let* _ = token ":simplify" in
  let+ e = Parser.expr in
  Simplify e

let keyword (kw : string) (result : t) : t Combinators.t =
  token kw >>| fun _ -> result

let command : t Combinators.t =
  choice
    [
      diff_stmt;
      simplify_stmt;
      keyword ":vars" Vars;
      keyword ":clear" Clear;
      keyword ":reset" Reset;
      keyword ":help" Help;
      (* [:quit] must be tried before its own prefix [:q], or ":quit" would
         match only the first 2 chars and leave "uit" dangling. *)
      keyword ":quit" Quit;
      keyword ":q" Quit;
    ]

let statement : t Combinators.t =
  spaces *> choice [ command; assign; (Parser.expr >>| fun e -> Eval e) ]

let parse (s : string) : (t, error) result = Combinators.parse statement s

let help_text =
  String.concat "\n"
    [
      "Expressions:  2 + 3 * 4         sin(pi / 2)     2^10";
      "Assignment:   x = 5             y = x^2 + 1";
      "              (the last bare expression's value is also remembered as 'ans')";
      "";
      "Commands:";
      "  :simplify EXPR         symbolic simplification, no numeric evaluation";
      "  :diff EXPR wrt VAR     symbolic derivative with respect to VAR";
      "  :vars                  list currently defined variables";
      "  :clear                 clear the history display";
      "  :reset                 drop all variables back to defaults (pi, e)";
      "  :help                  this message";
      "  :quit or :q            exit";
      "";
      "Functions: sin cos tan sqrt ln log exp abs  (all take one argument)";
      "Constants: pi    e";
    ]
