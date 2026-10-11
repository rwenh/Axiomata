(** Variable bindings: name maps to a single float. *)

module M = Map.Make (String)

type t = float M.t

let empty : t = M.empty

(** The starting environment for a fresh session: two constants every
    calculator has. *)
let default : t = empty |> M.add "pi" Float.pi |> M.add "e" (exp 1.0)

let find (name : string) (env : t) : float option = M.find_opt name env
let add (name : string) (value : float) (env : t) : t = M.add name value env
let remove (name : string) (env : t) : t = M.remove name env

(** Bindings in alphabetical order (for the TUI's [:vars] command). *)
let to_list (env : t) : (string * float) list = M.bindings env
