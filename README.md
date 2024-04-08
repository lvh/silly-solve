# silly-solve

A silly little multiple equation solver based on term rewriting. Intended for
monotonic spreadsheet-like systems of equations where you can solve everything
by doing enough substitution, simplification and evaluation.

I needed this because I ran into an issue with [expresso][] where providing
additional superfluous variables confused it. It would apply certain equations
and be unable to resolve some variables. Trying to execute with all subsets of
equations seemed like way more of a hassle than just writing a simple solver,
since I really just wanted some very straightforward expressions. Plus, you
might even be able to use this in combination with expresso: this is silly but
makes progress, expresso is sophisticated but sometimes get stuck; perhaps they
can help each other out.

(I tried amending expresso but its sophistication also made it pretty dense;
because the equations I wanted to solve are so straightforward, it felt easier
to just write a bit of [meander][] to make progress.)

[expresso]: https://github.com/clojure-numerics/expresso
[meander]: https://github.com/noprompt/meander

Goals:

* Given some simple equations with enough constants that they can be solved
  step-by-step and in some sense monotonically, solve them

Non-goals:

* Hitting any metrics of simplification or aesthetics, e.g. `(+ -4 x)` vs `(- x 4)`
* Full-on Gaussian solvers
* Full-on other solvers (e.g. logarithms, differentials...)

## Installation

Download from https://github.com/io.lvh/silly-solve

## Usage

Run the project's tests:

    $ clojure -X:test:runner

## License

Copyright © Laurens Van Houtven

Distributed under the Eclipse Public License version 1.0.
