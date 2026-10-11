(** Update: handle a key event and produce a new model. *)

open Model

let insert_char (m : t) (c : char) : t =
  let before = String.sub m.input 0 m.cursor in
  let after = String.sub m.input m.cursor (String.length m.input - m.cursor) in
  set_input m (before ^ String.make 1 c ^ after) (m.cursor + 1)

let delete_backward (m : t) : t =
  if m.cursor = 0 then m
  else
    let before = String.sub m.input 0 (m.cursor - 1) in
    let after = String.sub m.input m.cursor (String.length m.input - m.cursor) in
    set_input m (before ^ after) (m.cursor - 1)

let delete_forward (m : t) : t =
  if m.cursor >= String.length m.input then m
  else
    let before = String.sub m.input 0 m.cursor in
    let after =
      String.sub m.input (m.cursor + 1) (String.length m.input - m.cursor - 1)
    in
    set_input m (before ^ after) m.cursor

let move_left (m : t) : t =
  if m.cursor > 0 then { m with cursor = m.cursor - 1 } else m

let move_right (m : t) : t =
  if m.cursor < String.length m.input then { m with cursor = m.cursor + 1 }
  else m

let move_home (m : t) : t = { m with cursor = 0 }
let move_end (m : t) : t = { m with cursor = String.length m.input }

let eval_line (m : t) : t =
  let line = String.trim m.input in
  if line = "" then m
  else
    let m = push m (Input line) in
    let m = set_input m "" 0 in
    match Cas.Statement.parse line with
    | Error { Combinators.pos; msg } ->
        let line_n, col = Combinators.line_col line pos in
        push m
          (Error
             (Printf.sprintf "syntax error at line %d, col %d: %s" line_n col msg))
    | Ok stmt -> (
        match stmt with
        | Cas.Statement.Quit -> { m with should_quit = true }
        | Cas.Statement.Help -> push m (Info Cas.Statement.help_text)
        | Cas.Statement.Vars ->
            let vars = Cas.Env.to_list m.env in
            if vars = [] then push m (Info "(no variables defined)")
            else
              List.fold_left
                (fun m (name, v) ->
                  push m
                    (Info
                       (Printf.sprintf "  %s = %s" name (Cas.Pretty.format_num v))))
                m vars
        | Cas.Statement.Clear -> { m with history = [] }
        | Cas.Statement.Reset ->
            let m = { m with env = Cas.Env.default } in
            push m (Info "environment reset (pi, e restored)")
        | Cas.Statement.Assign (name, e) -> (
            match Cas.Eval.eval m.env e with
            | Ok v ->
                let env = Cas.Env.add name v m.env in
                let env = Cas.Env.add "ans" v env in
                let m = { m with env } in
                push m
                  (Output
                     (Printf.sprintf "%s = %s" name (Cas.Pretty.format_num v)))
            | Error err -> push m (Error (Cas.Eval.error_to_string err)))
        | Cas.Statement.Eval e -> (
            match Cas.Eval.eval m.env e with
            | Ok v ->
                let env = Cas.Env.add "ans" v m.env in
                let m = { m with env } in
                push m (Output (Cas.Pretty.format_num v))
            | Error err -> push m (Error (Cas.Eval.error_to_string err)))
        | Cas.Statement.Simplify e ->
            let s = Cas.Simplify.simplify e in
            push m (Output (Cas.Pretty.to_string s))
        | Cas.Statement.Diff (e, var) -> (
            match Cas.Diff.diff var e with
            | Ok d ->
                let s = Cas.Simplify.simplify d in
                push m (Output (Cas.Pretty.to_string s))
            | Error err -> push m (Error (Cas.Diff.error_to_string err))))

let on_key (m : t) (key : Notty.Unescape.key) : t =
  match key with
  | `Enter, _ -> eval_line m
  | `Backspace, _ -> delete_backward m
  | `Delete, _ -> delete_forward m
  | `Arrow `Left, _ -> move_left m
  | `Arrow `Right, _ -> move_right m
  | `Home, _ -> move_home m
  | `End, _ -> move_end m
  | `ASCII c, _ when c >= ' ' && c <> '\127' -> insert_char m c
  | `ASCII ('C' | 'c'), mods when List.mem `Ctrl mods ->
      { m with should_quit = true }
  | `Escape, _ -> { m with should_quit = true }
  | _ -> m
