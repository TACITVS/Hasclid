# Hasclid: A Haskell-based Euclidean Geometry Theorem Prover

![Haskell Logo](https://img.shields.io/badge/Haskell-5E5086?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal Build](https://img.shields.io/badge/Built%20with-Cabal-blueviolet?style=for-the-badge)

## Project Description

**Hasclid** (v9.1) is an advanced, interactive theorem prover for Euclidean geometry built in Haskell. It bridges the gap between geometric intuition and rigorous algebraic proof through intelligent solver selection and a two-phase proving architecture.

### Two-Phase Solving Architecture

**Phase 1: Fast Geometric Reasoning (GeoSolver)**
- Symbolic constraint propagation for geometric problems
- Handles symbolic parameters (e.g., side length 'S')
- Explores multiple geometric configurations (branches)
- Returns results in milliseconds for solvable cases
- Falls back to Phase 2 when constraints are insufficient

**Phase 2: Algebraic Solvers (Automatic Selection)**

When GeoSolver cannot decide, the system automatically selects the optimal algebraic method:

1.  **Gröbner Bases (Buchberger's Algorithm):** General-purpose algebraic equation solver. Robust but slower for pure geometry.
2.  **Wu's Method (Characteristic Sets):** Optimized for geometric theorem proving. Uses triangularization, 10-100x faster than Gröbner for coordinate geometry.
3.  **CAD (Cylindrical Algebraic Decomposition):** Real inequality solving using polynomial discriminants and sign analysis (1D-2D support).
4.  **Sturm Sequences:** Real root counting for univariate polynomial inequalities.
5.  **Integer Solver:** Linear interval solving with optional bounded brute-force search.
6.  **Modular Arithmetic:** Probabilistic consistency checking over finite fields.

Hasclid operates as a Read-Eval-Print Loop (REPL), providing a flexible environment to define points, script theorems, and explore algebraic geometry interactively.

## 📚 Documentation

**v9.1:** Complete formal language specification with intelligent multi-solver architecture!

- **[Tutorial](docs/TUTORIAL.md)** - Learn Euclid in 30 minutes
- **[Language Reference](docs/LANGUAGE.md)** - Complete specification
- **[Formal Grammar](docs/GRAMMAR.bnf)** - BNF grammar
- **[Docs Overview](docs/README.md)** - Documentation index

## Features

*   **Symbolic Algebra Engine**: Custom sparse multivariate polynomial implementation with rational coefficients.
*   **Geometric Primitives**: First-class support for:
    *   `dist2`: Squared Euclidean distance.
    *   `collinear`: Collinearity checks (determinant-based).
    *   `dot`: Vector dot products.
    *   `circle`: Circle equation constraints.
    *   **v9.1:** `midpoint`, `perpendicular`, `parallel` primitives.
*   **Advanced Proof Logic**:
    *   **Equality**: Automatically reduced using computed Groebner Bases of the hypothesis ideal.
    *   **Inequality**: Verified using root counting (Sturm sequences) and interval bisection.
*   **Interactive REPL**:
    *   **:point**: Define 2D/3D points.
    *   **:assume**: Add arbitrary polynomial constraints.
    *   **:lemma**: Prove and store intermediate results to the theory context.
    *   **:solve**: Find valid regions for variables satisfying an inequality (1D and 2D support).
    *   **:project**: Visualization of CAD projections (discriminants).
    *   **v9.1:** **:verbose** - Show detailed proof explanations.
    *   **v9.1:** **:save-lemmas / :load-lemmas** - Build reusable theorem libraries.
*   **Scripting**: Load and verify complex multi-step theorems from `.euclid` files.

## 🏆 Notable Achievements

### Morley's Theorem (December 2025)
**First automated proof of Morley's Theorem in HASCLID!**

**Theorem**: In any triangle, the three points of intersection of adjacent angle trisectors form an equilateral triangle.

**Status**: ✅ **PROVED** for right isosceles triangle using:
- **Rational function support** (division elimination via disjunctive transformation)
- **Polynomial formulation strategy** (pure polynomial goals)
- **Groebner basis computation** (<10 second proof time)
- **Symmetry exploitation** (reduces complexity exponentially)

**Key Innovation**: Constant division optimization (`1/3 → 0.333...`) eliminates timeouts, making complex geometric proofs with rational coordinates computationally feasible.

**Files**:
- `examples/morley_final.euclid` - Complete proof script
- `src/RationalElim.hs` - Rational function preprocessing module

**Impact**: HASCLID now joins elite automated theorem provers capable of proving famous classical geometry theorems with rational function support.

## Installation

Requires **GHC** (Haskell Compiler) and **Cabal**.

1.  **Clone:**
    ```bash
    git clone https://github.com/TACITVS/Hasclid.git
    cd Hasclid
    ```

2.  **Build:**
    ```bash
    cabal update
    cabal build
    ```

3.  **Run:**
    ```bash
    cabal run prover
    ```

## Usage & Commands

Start the REPL with `cabal run prover`.

### Basic Commands
*   `:point <Name> <x> <y> [z]`: Define a point. Coordinates can be numbers, fractions (`1/2`), or symbolic variables.
    *   *Example:* `:point A 0 0` (Origin)
    *   *Example:* `:point B x y` (Arbitrary 2D point)
*   `:assume (<op> <lhs> <rhs>)`: Add a constraint to the current theory.
    *   *Example:* `:assume (= (dist2 A B) (dist2 A C))`
*   `:list`: Show active assumptions and proven lemmas.
*   `:list-lemmas`: Show stored lemmas separately.
*   `:reset`: Clear all assumptions (keeps lemmas and cache).
*   `:soft-reset`: Clear all assumptions (keeps lemmas, cache preserved).
*   `:clear`: Full factory reset (clears everything).
*   `:load <file>`: Run a `.euclid` script (executes commands and formulas).
*   `:solve <file>`: Solve each formula in the specified file (batch processing).
*   `:save-lemmas <file>`: Save proven lemmas to a file.
*   `:load-lemmas <file>`: Load lemmas from a file.

### Proof & Solver Commands
*   `(op lhs rhs)`: Entering a raw formula uses automatic solver selection (GeoSolver → best algebraic solver).
    *   *Example:* `(= (dist2 A B) (dist2 A C))`
*   `:auto (<op> <lhs> <rhs>)`: Explicitly request automatic solver selection (same as direct formula).
*   `:prove (<op> <lhs> <rhs>)`: Force Gröbner basis solver.
*   `:wu (<op> <lhs> <rhs>)`: Force Wu's method (works only for equality goals).
*   `:lemma (<op> <lhs> <rhs>)`: Proves a statement. If valid, adds it to the persistent knowledge base (Lemmas) for future proofs.
*   `:counterexample <formula>` or `:ce <formula>`: Search for a counterexample to the given formula.
*   `:construct <formula>`: Search for a satisfying assignment for the given formula.

### Advanced Configuration
*   `:verbose`: Toggle detailed proof explanations (shows proof steps, assumptions used, etc.).
*   `:auto-simplify`: Toggle automatic expression simplification.
*   `:set-timeout <seconds>`: Set solver timeout (default: 30s). Prevents hanging on hard problems.
*   `:show-timeout`: Display current timeout setting.
*   `:optimize on|off`: Toggle Buchberger optimization for Gröbner basis computation.
*   `:set-strategy <name>`: Set selection strategy for S-polynomial pairs (normal|sugar|minimal).
*   `:bruteforce on|off`: Enable/disable bounded brute-force search for integer constraint goals.
*   `:set-order <order>`: Set term ordering (grevlex|lex|gradedlex).
*   `:show-order`: Display current term ordering.
*   `:cache-stats`: Show Gröbner basis cache hit/miss statistics.
*   `:clear-cache`: Clear the Gröbner basis cache.

## Codebase Structure

### Core Modules

*   **`src/Main.hs`**: REPL loop, state management, command dispatch, and timeout handling.
*   **`src/Expr.hs`**: Core AST for expressions and formulas. Multivariate polynomial engine with sparse map representation and rational arithmetic.
*   **`src/Parser.hs`**: Robust S-expression parser with macro support for the prefix notation language.
*   **`src/Error.hs`**: Error types and formatting for user-friendly error messages.
*   **`src/Validation.hs`**: Input validation and degeneracy checking (coincident points, zero-length segments).

### Solvers & Proving Engines

*   **`src/GeoSolver.hs`**: **Phase 1 fast-path solver.** Symbolic geometric constraint propagation with branching support.
*   **`src/Prover.hs`**: **Phase 2 algebraic proving.** Gröbner basis computation, integer solver, quantifier handling, and proof tracing.
*   **`src/Wu.hs`**: Wu's characteristic set method for geometric theorem proving. Includes triangularization and constructive existence proofs.
*   **`src/SolverRouter.hs`**: Intelligent solver selection system. Analyzes problems and dispatches to optimal solver (GeoSolver → Wu/Gröbner/CAD).
*   **`src/ProblemAnalyzer.hs`**: Problem classification engine. Extracts variables, estimates complexity, detects geometric features.
*   **`src/Modular.hs`**: Modular arithmetic solver for probabilistic consistency checking over finite fields.
*   **`src/CounterExample.hs`**: Counterexample and witness finding using strategic variable assignments.

### Gröbner Basis Implementation

*   **`src/Core/GB.hs`**: Optimized Gröbner basis core with S-polynomial computation and reduction.
*   **`src/BuchbergerOpt.hs`**: Selection strategies for Buchberger's algorithm (normal, sugar, minimal).
*   **`src/Cache.hs`**: Gröbner basis caching system to avoid recomputation.
*   **`src/TermOrder.hs`**: Term ordering implementations (grevlex, lex, gradedlex) for polynomial rewriting.

### CAD & Inequalities

*   **`src/CAD.hs`**: Recursive polynomial representation, pseudo-division, discriminants, and resultants.
*   **`src/CADLift.hs`**: CAD lifting phase with sign determination and quantifier elimination.
*   **`src/Sturm.hs`**: Sturm sequence computation for real root counting and interval isolation.
*   **`src/Positivity.hs`**: Multi-method positivity checking (Sturm, heuristics, SOS detection).

### Utilities & Transformations

*   **`src/Algebraic.hs`**: Algebraic number operations and symbolic square root handling.
*   **`src/Linearizer.hs`**: Linearization utilities for constraint simplification.
*   **`src/SqrtElim.hs`**: Square root elimination via polynomial substitution for CAD preprocessing.

### Examples & Tests

*   **`examples/`**: Collection of `.euclid` scripts demonstrating geometric theorems and stress tests.
*   **`test/Spec.hs`**: Hspec test suite with QuickCheck properties for regression testing.

## Example Session

```text
Euclid> :point A 0 0
Defined 2D Point A at (0, 0)

Euclid> :point B 10 0
Defined 2D Point B at (10, 0)

Euclid> :solve (> (dist2 A B) 50) x
...
```

## License

MIT License.
