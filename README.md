Hatt
====

[Hatt] is a command-line program which prints truth tables for expressions in
classical propositional logic, and a library allowing its parser, evaluator,
truth table generator and other functionality to be used in other programs.

Installation
------------

Hatt is available from [Hackage]. To install it with `cabal-install`, update
your list of known packages and then install Hatt.

    $ cabal update
    $ cabal install hatt

To build it from source, `cd` into the directory containing the Hatt source
files, including `hatt.cabal`, and run `cabal install`.

Valid Hatt expressions
----------------------

The following are all valid expression forms which can be parsed by Hatt, where
`ϕ` and `ψ` are metalinguistic variables standing in for any valid expression.
Hatt accepts the standard Unicode symbols for logical connectives. This is
useful when pasting example formulae from the web or using Hatt in `--pretty`
mode.

* Variables: `P`, `Q`, `a`, `b` etc.---basically anything in the character
  class `[a-zA-Z]`
* Negation: `~ϕ` or `¬ϕ`
* Conjunction: `(ϕ & ψ)` or `(ϕ ∧ ψ)`
* Disjunction: `(ϕ | ψ)` or `(ϕ ∨ ψ)`
* Conditional: `(ϕ -> ψ)` or `(ϕ → ψ)`
* Biconditional: `(ϕ <-> ψ)` or `(ϕ ↔ ψ)`

Parentheses are not required around top-level formulae, regardless of whether
the primary connective is binary. For example, the expression `a | b` is valid
and will be parsed correctly, as would `p <-> (q & ~r)`, although the
parenthesised versions of both these expressions (`(a | b)` and
`(p <-> (q & ~r))`) are also fine.

Standard [operator precedence] for logical operators is supported. All the
binary connectives associate to the right, so `a -> b -> c` is interpreted as
`a -> (b -> c)`. To override these rules, use parentheses as normal. Note that
right-associativity only matters for the material conditional: all the other
logical operators are associative, so `a * (b * c)` is equivalent to
`(a * b) * c` where `*` is conjunction, disjunction or the biconditional.

Using the `hatt` command-line program
-------------------------------------

The default mode is interactive: you start the program, enter expressions at
the prompt, and their truth tables are printed. Here's an example session.

    $ hatt
    Entering interactive mode. Type `help` if you don't know what to do!
    > A | B
    A B | (A | B)
    -------------
    T T | T
    T F | T
    F T | T
    F F | F
    > p -> (q & ~r)
    p q r | (p -> (q & ~r))
    -----------------------
    T T T | F
    T T F | T
    T F T | F
    T F F | F
    F T T | T
    F T F | T
    F F T | T
    F F F | T
    > e <-> f
    e f | (e <-> f)
    ---------------
    T T | T
    T F | F
    F T | F
    F F | T
    > exit

The `--evaluate` flag lets you pass a single expression to be evaluated
directly.

    $ hatt --evaluate="P -> (Q | ~R)"
    P Q R | (P -> (Q | ~R))
    -----------------------
    T T T | F
    T T F | F
    T F T | F
    T F F | F
    F T T | F
    F T F | F
    F F T | T
    F F F | F

By default, `hatt` will print ASCII representations of expressions. If you have
a Unicode-capable terminal, try passing the `--pretty` option to pretty-print
expressions using the more common logical symbols.

    $ hatt --evaluate="P -> (Q | ~R)" --pretty
    P Q R | (P → (Q ∨ ¬R))
    ----------------------
    T T T | F
    T T F | F
    T F T | F
    T F F | F
    F T T | F
    F T F | F
    F F T | T
    F F F | F

You can enable pretty-printing while in interactive mode by using the `pretty`
command.

If you pass the `--coloured` flag, `hatt` will colour the truth values in the
tables which it prints: green for true, red for false. You can enable colouring
during interactive mode by using the `colour` command.

You can print out the normal forms of expressions too, by prefixing an
expression with `nnf`, `dnf` or `cnf`.

    $ hatt --pretty
    > nnf ~(P -> (Q & R))
    (P ∧ (¬Q ∨ ¬R))

The three supported normal forms are [negation normal form], [conjunctive normal
form] and [disjunctive normal form].

Using Hatt in other programs
----------------------------

Hatt exposes the `Data.Logic.Propositional` module, which provides a simple API
for parsing, evaluating, and printing truth tables. It also includes the
`Data.Logic.Propositional.NormalForms` module which exports functions to convert
logical expressions into normal forms. Details of the foregoing can be found in
the [library documentation].

[Hatt]:    http://extralogical.net/projects/hatt
[Hackage]: http://hackage.haskell.org/
[operator precedence]:
  http://en.wikipedia.org/wiki/Logical_connective#Order_of_precedence
[negation normal form]: http://en.wikipedia.org/wiki/Negation_normal_form
[conjunctive normal form]: http://en.wikipedia.org/wiki/Conjunctive_normal_form
[disjunctive normal form]: http://en.wikipedia.org/wiki/Disjunctive_normal_form
[library documentation]: http://hackage.haskell.org/package/hatt
