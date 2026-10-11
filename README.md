# mcas -- a mini computer-algebra system

A terminal tool for parsing, evaluating, symbolically simplifying, and
differentiating arithmetic expressions -- built on a parser-combinator
library written from scratch, in the style of Angstrom.

```
> x = 3
  x = 3
> :diff x^2 + sin(x) wrt x
  2 * x + cos(x)
> :simplify (x + 0) * 1
  x
> sqrt(-1)
! domain error: sqrt of negative number -1
```

## Layout

```
mcas/
├── lib/
│   ├── combinators/   a general-purpose parser combinator library
│   ├── cas/           the expression grammar, evaluator, simplifier,
│   │                  differentiator, pretty-printer, and REPL statement
│   │                  layer -- all built on lib/combinators
│   └── tui/           the terminal UI's state (Model), key handling
│                      (Update), and rendering (View), built on Notty
├── bin/               the executable: launches the TUI with no args,
│                      or runs a one-shot `eval`/`simplify`/`diff` subcommand
└── test/              an Alcotest suite covering all three libraries
```

Each library is independent enough to reuse on its own -- `combinators`
doesn't know `cas` exists, and `cas` doesn't know `tui` exists.

## Building and running

Everything here is a normal dune/opam project:

```
opam install . --deps-only --with-test
dune build
dune test
dune exec bin/main.exe          # launch the interactive TUI
```

Or, for one-shot use without a live terminal:

```
dune exec bin/main.exe -- eval "2 + 3 * 4"
dune exec bin/main.exe -- simplify "(x + 0) * 1"
dune exec bin/main.exe -- diff "x^2 + 3*x" x
```

## The expression language

```
expr   := term (('+' | '-') term)*
term   := unary (('*' | '/') unary)*
unary  := '-'* power
power  := atom ('^' unary)?
atom   := number | ident ('(' expr ')')? | '(' expr ')'
```

- Precedence and associativity match ordinary mathematical convention:
  `*`/`/` bind tighter than `+`/`-`; unary minus binds *looser* than `^`,
  so `-2^2` is `-4`, not `4`; `^` is right-associative, so `2^3^2` is
  `2^(3^2) = 512`.
- Multiplication is never implicit -- write `2*x`, not `2x`.
- Functions are single-argument: `sin cos tan sqrt ln log exp abs`.
  `log` is base 10; `ln` is natural log.
- `pi` and `e` are bound in the starting environment.

The TUI (and the `bin/main.exe` executable in general) layers a small
statement grammar on top of this for assignment and meta-commands --
`x = 5`, `:diff EXPR wrt VAR`, `:simplify EXPR`, `:vars`, `:clear`,
`:reset`, `:help`, `:quit`/`:q` -- see `lib/cas/statement.ml`. The last
bare expression's value is remembered as `ans`.

## Design notes

A few decisions worth knowing about before extending this:

- **`Combinators.t` is fully abstract**, and backtracks unconditionally on
  `<|>`/`choice` (no "commit once you've consumed input" the way Angstrom
  or Parsec do it) -- simpler to reason about, at some efficiency cost
  that's irrelevant at this scale. Because the type is abstract, tying a
  recursive knot needs `fix` (single self-recursive production) or
  `delay` + a ref cell (mutual recursion among several named productions,
  which is what `lib/cas/parser.ml` does for `expr`/`term`/`power`/`unary`/
  `atom`) -- an ordinary `let rec` doesn't typecheck here, since
  `choice`/`between`/etc. are ordinary function applications, not
  manifest `fun`s. Both combinators' doc comments in `combinators.mli`
  explain why.
- **Numbers are plain `float`s**, not exact rationals. Simplest thing that
  works, but it means e.g. `1/3 + 1/3` simplifies numerically rather than
  to an exact `2/3`. Swapping in `Q` from `zarith` for exact rational
  arithmetic would be the natural upgrade if that ever matters.
- **The simplifier is intentionally not exhaustive.** It does constant
  folding plus a handful of identities (`x+0`, `x*1`, `x*0`, `x-x`, `x/x`,
  `x^0`, `x^1`, double negation) -- enough to keep differentiation output
  readable, not a general algebraic simplifier. It won't, for instance,
  notice that `x * (1/x)` is `1` (only the direct `x/x` form), or collect
  like terms, or factor anything.
- **Syntax and semantics are separate.** The parser accepts *any*
  `ident(expr)` as a function call; it doesn't check the name against a
  known-function list. `Eval` and `Diff` each independently decide which
  names they know how to handle, and report `Unknown_function`/
  `Unbound_variable` otherwise. Nothing here ever raises an exception for
  a malformed or semantically invalid expression -- parse errors,
  evaluation errors, and differentiation errors are all ordinary `result`
  values.
- **The TUI follows the Elm/TEA pattern**: `Model.t` is one immutable
  record, `Update.on_key : Model.t -> Notty.Unescape.key -> Model.t` is a
  pure function, and `View.render : rows:int -> Model.t -> Notty.image` is
  another. `bin/main.ml` is the only place that touches a real terminal --
  it's a tight `render; wait for an event; update; repeat` loop. This is
  what makes `test/test_tui.ml` able to drive the whole input-handling and
  statement-execution logic (typing, backspace, history recall, running a
  line, quitting) without a live terminal at all.

## Possible extensions

- Exact rational arithmetic instead of `float` (see above).
- Multi-argument functions (`log(x, base)`), which needs a small grammar
  change in `lib/cas/parser.ml`'s function-call production.
- A more aggressive simplifier: collecting like terms, factoring,
  recognizing more identities.
- Symbolic integration -- differentiation's much harder inverse.
- `.mli` files for the `cas` modules once their APIs feel settled
  (`combinators.mli` is the one place this project currently bothers,
  since it's the most reusable and general piece).
