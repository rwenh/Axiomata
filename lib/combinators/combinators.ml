(* See combinators.mli for the public API and design notes. *)

type state = { src : string; pos : int }
type error = { pos : int; msg : string }
type 'a t = state -> ('a * state, error) result

(* --- core --- *)

let return (x : 'a) : 'a t = fun st -> Ok (x, st)

let fail (msg : string) : 'a t = fun st -> Error { pos = st.pos; msg }

let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
  fun st -> match p st with Error _ as e -> e | Ok (x, st') -> f x st'

let map (f : 'a -> 'b) (p : 'a t) : 'b t =
  fun st -> match p st with Error _ as e -> e | Ok (x, st') -> Ok (f x, st')

module Infix = struct
  let ( >>= ) = bind
  let ( >>| ) p f = map f p
  let ( <$> ) f p = map f p
  let ( <*> ) pf pa = bind pf (fun f -> map f pa)
  let ( <* ) p q = bind p (fun x -> map (fun _ -> x) q)
  let ( *> ) p q = bind p (fun _ -> q)

  let ( <|> ) p q =
    fun st -> match p st with Ok _ as r -> r | Error _ -> q st
end

module Let_syntax = struct
  let ( let* ) = bind
  let ( let+ ) p f = map f p

  let ( and+ ) p q =
    fun st ->
      match p st with
      | Error _ as e -> e
      | Ok (x, st') -> (
          match q st' with
          | Error _ as e -> e
          | Ok (y, st'') -> Ok ((x, y), st''))
end

open Infix
open Let_syntax

(* --- recursive grammars --- *)

let fix (f : 'a t -> 'a t) : 'a t =
  let rec p st = f p st in
  p

let delay (f : unit -> 'a t) : 'a t = fun st -> f () st

(* --- chaining binary operators --- *)

let chainl1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  let rec go x =
    (let* f = op in
     let* y = p in
     go (f x y))
    <|> return x
  in
  p >>= go

let chainr1 (p : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
  let rec go () =
    let* x = p in
    (let* f = op in
     let* y = go () in
     return (f x y))
    <|> return x
  in
  go ()

(* --- chars and strings --- *)

let satisfy (pred : char -> bool) : char t =
  fun st ->
    if st.pos >= String.length st.src then
      Error { pos = st.pos; msg = "unexpected end of input" }
    else
      let c = st.src.[st.pos] in
      if pred c then Ok (c, { st with pos = st.pos + 1 })
      else Error { pos = st.pos; msg = Printf.sprintf "unexpected character %C" c }

let char (expected : char) : char t =
  fun st ->
    if st.pos >= String.length st.src then
      Error
        { pos = st.pos; msg = Printf.sprintf "expected %C, reached end of input" expected }
    else
      let c = st.src.[st.pos] in
      if Char.equal c expected then Ok (c, { st with pos = st.pos + 1 })
      else
        Error
          { pos = st.pos; msg = Printf.sprintf "expected %C but found %C" expected c }

let any_char : char t = satisfy (fun _ -> true)

let string (expected : string) : string t =
  fun st ->
    let len = String.length expected in
    if st.pos + len > String.length st.src then
      Error
        { pos = st.pos; msg = Printf.sprintf "expected %S, reached end of input" expected }
    else if String.equal (String.sub st.src st.pos len) expected then
      Ok (expected, { st with pos = st.pos + len })
    else Error { pos = st.pos; msg = Printf.sprintf "expected %S" expected }

let one_of (chars : string) : char t =
  satisfy (fun c -> String.contains chars c)

let none_of (chars : string) : char t =
  satisfy (fun c -> not (String.contains chars c))

let take_while (pred : char -> bool) : string t =
  fun st ->
    let len = String.length st.src in
    let i = ref st.pos in
    while !i < len && pred st.src.[!i] do
      incr i
    done;
    Ok (String.sub st.src st.pos (!i - st.pos), { st with pos = !i })

let take_while1 (pred : char -> bool) : string t =
  fun st ->
    match take_while pred st with
    | Ok (s, st') when String.length s > 0 -> Ok (s, st')
    | _ -> Error { pos = st.pos; msg = "expected at least one character" }

let skip_while (pred : char -> bool) : unit t =
  fun st ->
    let len = String.length st.src in
    let i = ref st.pos in
    while !i < len && pred st.src.[!i] do
      incr i
    done;
    Ok ((), { st with pos = !i })

(* --- repetition and choice --- *)

let many (p : 'a t) : 'a list t =
  fun st ->
    let rec go acc st =
      match p st with
      | Error _ -> Ok (List.rev acc, st)
      | Ok (x, st') ->
          if st'.pos = st.pos then Ok (List.rev (x :: acc), st')
          else go (x :: acc) st'
    in
    go [] st

let many1 (p : 'a t) : 'a list t =
  p >>= fun x -> many p >>| fun xs -> x :: xs

let sep_by1 ~(sep : 'b t) (p : 'a t) : 'a list t =
  p >>= fun x -> many (sep *> p) >>| fun xs -> x :: xs

let sep_by ~(sep : 'b t) (p : 'a t) : 'a list t =
  fun st ->
    match sep_by1 ~sep p st with
    | Ok _ as r -> r
    | Error _ -> Ok ([], st)

let optional (p : 'a t) : 'a option t =
  fun st ->
    match p st with
    | Ok (x, st') -> Ok (Some x, st')
    | Error _ -> Ok (None, st)

let option (default : 'a) (p : 'a t) : 'a t =
  fun st ->
    match p st with
    | Ok _ as r -> r
    | Error _ -> Ok (default, st)

let between (op : 'l t) (cl : 'r t) (p : 'a t) : 'a t = op *> p <* cl

let rec choice (ps : 'a t list) : 'a t =
  match ps with
  | [] -> fail "no alternative matched"
  | [ p ] -> p
  | p :: rest -> p <|> choice rest

let peek_char : char option t =
  fun st ->
    if st.pos >= String.length st.src then Ok (None, st)
    else Ok (Some st.src.[st.pos], st)

let end_of_input : unit t =
  fun st ->
    if st.pos >= String.length st.src then Ok ((), st)
    else Error { pos = st.pos; msg = "expected end of input" }

let spaces : unit t =
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let lexeme (p : 'a t) : 'a t = p <* spaces

let token (s : string) : string t = lexeme (string s)

(* --- running a parser --- *)

let parse (p : 'a t) (s : string) : ('a, error) result =
  match (p <* end_of_input) { src = s; pos = 0 } with
  | Ok (x, _) -> Ok x
  | Error e -> Error e

let line_col (s : string) (pos : int) : int * int =
  let line = ref 1 and col = ref 1 in
  let last = min pos (String.length s) - 1 in
  for i = 0 to last do
    if Char.equal s.[i] '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)
