(lang dune 3.0)

(name mcas)

(generate_opam_files true)

(package
 (name mcas)
 (synopsis
  "A TUI mini computer-algebra system, built on a hand-rolled parser-combinator library")
 (description
  "\
mcas parses, evaluates, symbolically simplifies, and differentiates simple\n\
arithmetic expressions, wrapped in a small terminal UI. Everything from the\n\
parser-combinator library on up -- the expression grammar, the evaluator,\n\
the simplifier, and the differentiator -- is written from scratch, leaning\n\
on OCaml's algebraic data types and pattern matching throughout.")
 (depends
  (ocaml
   (>= 4.14))
  notty
  cmdliner
  (alcotest
   (and
    :with-test
    (>= 1.5)))))
