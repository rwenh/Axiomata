(** Entry point. With no arguments, launches the full-screen TUI. With
    arguments, defers to {!Cmdliner} for a small one-shot CLI --
    [mcas eval/simplify/diff] -- handy for scripting or for sanity-checking
    the library from a shell without dealing with a live terminal. *)

let parse_or_die (s : string) : Cas.Ast.expr =
  match Cas.Parser.parse_expr s with
  | Ok e -> e
  | Error { Combinators.pos; msg } ->
      let line, col = Combinators.line_col s pos in
      Printf.eprintf "syntax error at line %d, column %d: %s\n" line col msg;
      exit 1

(* --------- One-shot CLI --------- *)

open Cmdliner

let expr_arg ~position =
  Arg.(required & pos position (some string) None & info [] ~docv:"EXPR")

let eval_cmd =
  let run expr =
    match Cas.Eval.eval Cas.Env.default (parse_or_die expr) with
    | Ok v -> print_endline (Cas.Pretty.format_num v)
    | Error err ->
        Printf.eprintf "%s\n" (Cas.Eval.error_to_string err);
        exit 1
  in
  let term = Term.(const run $ expr_arg ~position:0) in
  Cmd.v (Cmd.info "eval" ~doc:"Evaluate an expression and print its numeric value") term

let simplify_cmd =
  let run expr =
    print_endline
      (Cas.Pretty.to_string (Cas.Simplify.simplify (parse_or_die expr)))
  in
  let term = Term.(const run $ expr_arg ~position:0) in
  Cmd.v
    (Cmd.info "simplify" ~doc:"Symbolically simplify an expression, without evaluating it")
    term

let diff_cmd =
  let var_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"VAR") in
  let run expr var =
    match Cas.Diff.diff var (parse_or_die expr) with
    | Ok d -> print_endline (Cas.Pretty.to_string (Cas.Simplify.simplify d))
    | Error err ->
        Printf.eprintf "%s\n" (Cas.Diff.error_to_string err);
        exit 1
  in
  let term = Term.(const run $ expr_arg ~position:0 $ var_arg) in
  Cmd.v
    (Cmd.info "diff" ~doc:"Symbolically differentiate EXPR with respect to VAR")
    term

let cli =
  let doc = "a mini computer-algebra system" in
  let info = Cmd.info "mcas" ~version:"0.1.0" ~doc in
  Cmd.group info [ eval_cmd; simplify_cmd; diff_cmd ]

(* --------- TUI --------- *)

let run_tui () =
  let term = Notty_unix.Term.create () in
  (* We draw our own reverse-video cursor highlight inside the input line
     (see Tui.View.render_input), so the real terminal cursor stays hidden. *)
  Notty_unix.Term.cursor term None;
  let rec loop (m : Tui.Model.t) =
    let _cols, rows = Notty_unix.Term.size term in
    Notty_unix.Term.image term (Tui.View.render ~rows m);
    if m.Tui.Model.should_quit then Notty_unix.Term.release term
    else
      match Notty_unix.Term.event term with
      | `Key key -> loop (Tui.Update.on_key m key)
      | `Resize _ -> loop m
      | `End -> Notty_unix.Term.release term
      | `Mouse _ | `Paste _ -> loop m
  in
  loop Tui.Model.initial

let () =
  if Array.length Sys.argv <= 1 then run_tui () else exit (Cmd.eval cli)
