(** A small, self-contained parser combinator library in the spirit of
    Angstrom: a parser is a first-class value built out of small pieces and
    wired together with a handful of higher-order functions.

    A parser of type ['a t] consumes some prefix of an input string and
    either succeeds with a value of type ['a], or fails with an {!error}.

    {2 Backtracking}
    [( <|> )], {!choice}, {!optional}, and friends always backtrack:
    if the left/first alternative fails, the right/next one is tried from
    the same starting position, regardless of how much input the failed
    attempt consumed. *)

type 'a t

type error = { pos : int; msg : string }

val parse : 'a t -> string -> ('a, error) result

val line_col : string -> int -> int * int

val return : 'a -> 'a t
val fail : string -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
end

module Let_syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val fix : ('a t -> 'a t) -> 'a t
val delay : (unit -> 'a t) -> 'a t

val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
val chainr1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

val satisfy : (char -> bool) -> char t
val char : char -> char t
val any_char : char t
val string : string -> string t
val one_of : string -> char t
val none_of : string -> char t

val take_while : (char -> bool) -> string t
val take_while1 : (char -> bool) -> string t
val skip_while : (char -> bool) -> unit t

val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t

val sep_by : sep:'b t -> 'a t -> 'a list t
val sep_by1 : sep:'b t -> 'a t -> 'a list t

val optional : 'a t -> 'a option t
val option : 'a -> 'a t -> 'a t
val between : 'l t -> 'r t -> 'a t -> 'a t
val choice : 'a t list -> 'a t

val peek_char : char option t
val end_of_input : unit t

val spaces : unit t
val lexeme : 'a t -> 'a t
val token : string -> string t
