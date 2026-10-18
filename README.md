# Prolog Interpreter

Used in <https://github.com/fmidue/prolog-graph-lib> and <https://github.com/fmidue/prolog-programming-task>.

## Supported predicates

The interpreter supports the following built-in predicates and control constructs.
For explanations of their standard Prolog semantics, see the
[SWI-Prolog reference manual](https://www.swi-prolog.org/pldoc/man?section=builtin).

| Predicate | Description |
| --- | --- |
| `=/2`, `\=/2` | Unification and non-unifiability |
| `true/0`, `false/0`, `fail/0` | Succeed or fail unconditionally |
| `!/0` | Cut |
| `,/2`, `;/2` | Conjunction and disjunction |
| `not/1`, `\+/1` | Negation as failure |
| `is/2` | Arithmetic evaluation and assignment |
| `< /2`, `> /2`, `=< /2`, `>= /2`, `=:=/2`, `=\=/2` | Arithmetic comparisons |
| `@</2`, `@>/2`, `@=</2`, `@>=/2`, `==/2` | Standard term comparisons and term identity |
| `sort/2` | Sort a list and remove duplicates |
| `=../2` | Univ: convert between a compound term and a list |
| `atom/1`, `number/1` | Type tests |
| `char_code/2` | Convert between a one-character atom and its character code |
| `phrase/2`, `phrase/3` | Apply a DCG rule to a list |
| `asserta/1`, `assertz/1`, `retract/1` | Add and remove clauses dynamically |
