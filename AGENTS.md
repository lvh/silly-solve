# AGENTS.md - LLM Coding Guide for silly-solve

## Project Overview

**silly-solve** is a Clojure/ClojureScript library that implements a simple equation solver based on term rewriting. It solves monotonic systems of equations (spreadsheet-like) through iterative substitution, simplification, and evaluation using the [Meander](https://github.com/noprompt/meander) pattern matching library.

### Purpose
- Solve simple equations step-by-step via constant propagation
- Handle cases where more sophisticated solvers (like expresso) get stuck
- Support both symbols and keywords as variables

### Non-Goals
- Aesthetic simplification (e.g., `(+ -4 x)` vs `(- x 4)`)
- Full Gaussian elimination
- Complex math (logarithms, differentials, etc.)

## Project Structure

```
silly-solve/
├── deps.edn                           # Clojure deps.edn build configuration
├── src/
│   └── io/lvh/silly_solve.cljc        # Main source (ClojureScript-compatible)
└── test/
    └── io/lvh/silly_solve_test.clj    # Test suite
```

### Key Files

| File | Purpose |
|------|---------|
| `src/io/lvh/silly_solve.cljc` | Core solver implementation (~140 lines) |
| `test/io/lvh/silly_solve_test.clj` | Comprehensive test suite (~205 lines) |
| `deps.edn` | Dependencies and test runner configuration |

## Architecture

### Core Algorithm

The solver works via iterative refinement in `solve-for-consts`:

1. **Find constants** - Extract `var = constant` bindings from equations
2. **Propagate constants** - Substitute known values into remaining equations
3. **Simplify** - Apply rewrite rules to reduce expressions
4. **Repeat** - Until solved or stuck

### Key Components

- **`simplify`** - Bottom-up term rewriting strategy that:
  - Reduces unary operations: `(+ x)` → `x`
  - Eliminates neutral elements: `(+ 0 x)` → `x`, `(* 1 x)` → `x`
  - Evaluates constant expressions: `(+ 1 2)` → `3`
  - Handles commutative ops by folding constants: `(+ 1 x 2)` → `(+ 3 x)`
  - Inverts subtraction/division: `(- a b c)` → `(+ a (- b) (- c))`
  - Removes tautologies: `(= x x)` → `nil`

- **`solve-for-consts`** - Main entry point, returns `[remaining-eqns solved-consts]`

### Supported Operations

| Op | Commutative | Invertible | Notes |
|----|-------------|------------|-------|
| `+` | Yes | - | Folds constants |
| `*` | Yes | - | Folds constants |
| `-` | No | Yes (via `+`) | Inverts to addition |
| `/` | No | Yes (via `*`) | Inverts to multiplication |
| `**` | No | No | Exponentiation |
| `max` | Yes | No | - |
| `min` | Yes | No | - |

## Dependencies

- **Clojure 1.11.2** - Core language
- **ClojureScript 1.11.132** - For .cljc compatibility
- **meander/epsilon** - Pattern matching DSL for rewrite rules
- **io.github.noprompt/meander** - Git dependency (workaround for issue #245)
- **clojure.math.numeric-tower** - For `expt` function

## Testing Strategy

### Running Tests

```bash
clojure -X:test:runner
```

### Test Structure

Tests are organized by functionality with comprehensive coverage:

| Test | What it covers |
|------|----------------|
| `invert-tests` | Subtraction/division inversion |
| `find-consts-tests` | Extracting constant bindings from equations |
| `propagate-consts-tests` | Substituting constants into expressions |
| `simplify-tests` | All rewrite rules (unary, neutral elements, arithmetic) |
| `keywords-work-too-tests` | Both symbols and keywords as variables |
| `min-max-tests` | Min/max operations |
| `equality-const-to-front` | Normalizing equation form |
| `deal-with-multi-valued-equality-tests` | Equations like `(= :a :b :c 10)` |

### Testing Guidelines for LLM Agents

1. **Always run tests after changes**: `clojure -X:test:runner`
2. **Test private functions** using `#'namespace/fn` syntax (see test file)
3. **Use `t/are`** for table-driven tests with multiple cases
4. **Use `traced-simplify`** (`r/trace ss/simplify`) for debugging rewrite rules
5. **Add tests for edge cases** when modifying operations

### Test Patterns

```clojure
;; Table-driven tests with t/are
(t/are [input expected] (= expected (function input))
  '(+ 1) 1
  '(+ 2) 2)

;; Testing private functions
(#'ss/invert '- [1 2 3])

;; Testing complete solve
(ss/solve-for-consts '[(= x 3) (= y (* 2 x))])
;; => [[] {x 3, y 6}]
```

## Development Guidelines

### Adding New Operations

1. Add entry to `ops` vector in `silly_solve.cljc`:
   ```clojure
   {::symbol 'new-op ::fn impl-fn ::commutative true}  ; or
   {::symbol 'new-op ::fn impl-fn ::invertible-with 'inverse-op}
   ```

2. Add tests for the new operation

3. If operation has special simplification rules, add to `simplify` rewrite

### Common Patterns

- **Reader conditionals** for Clojure/ClojureScript differences:
  ```clojure
  #?(:clj  [clojure-only ...]
     :cljs [cljs-only ...])
  ```

- **Variables** can be symbols or keywords (checked via `variable?`)

- **Equations** are S-expressions: `'(= x (* 2 y))`

### Debugging

Use `r/trace` to debug rewrite rule applications:
```clojure
(def traced-simplify (r/trace ss/simplify))
(traced-simplify '(+ 1 2 x))
```

## Code Style

- Use `->>` threading for pipelines
- Private helpers use `^:private` metadata
- Meander patterns use `?var` for logic variables, `!var` for memory variables
- Operations defined declaratively in `ops` vector

## Potential Improvements (for reference)

Areas noted in code comments that could be enhanced:

1. Neutral element handling with multiple variables (see commented test)
2. More aesthetic output forms
3. Additional mathematical operations

## Quick Reference

```clojure
;; Main API
(require '[io.lvh.silly-solve :as ss])

(ss/solve-for-consts '[(= x 3)
                       (= y (* 2 x))
                       (= z (+ x y))])
;; => [[] {x 3, y 6, z 9}]

;; With initial constants
(ss/solve-for-consts '[(= y (* 2 x))] '{x 3})
;; => [[] {x 3, y 6}]

;; Just simplify an expression
(ss/simplify '(+ 1 2 x 3))
;; => (+ 6 x)
```
