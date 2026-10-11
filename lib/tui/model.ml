(** The TUI's entire state as one immutable value -- an Elm/TEA-style model. *)

type history_entry =
  | Input of string
  | Output of string
  | Error of string
  | Info of string

type t = {
  input : string;
  cursor : int;
  history : history_entry list;
  env : Cas.Env.t;
  should_quit : bool;
}

let initial : t =
  {
    input = "";
    cursor = 0;
    history =
      [
        Info "mcas 0.1.0 -- mini computer-algebra system";
        Info "Type an expression, or :help for commands. :q to quit.";
      ];
    env = Cas.Env.default;
    should_quit = false;
  }

let push (m : t) (entry : history_entry) : t =
  { m with history = m.history @ [ entry ] }

let set_input (m : t) (s : string) (cursor : int) : t =
  { m with input = s; cursor }
