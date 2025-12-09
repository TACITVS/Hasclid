# Hasclid: A Haskell-based Euclidean Geometry Theorem Prover

![Haskell Logo](https://img.shields.io/badge/Haskell-5E5086?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal Build](https://img.shields.io/badge/Built%20with-Cabal-blueviolet?style=for-the-badge)

## Project Description

**Hasclid** (v9.1) is an advanced, interactive theorem prover for Euclidean geometry built in Haskell. It bridges the gap between geometric intuition and rigorous algebraic proof by translating geometric constraints into polynomial ideals.

The engine combines three powerful algebraic techniques to automate proofs:
1.  **Groebner Bases (Buchberger's Algorithm):** For verifying equality constraints (e.g., proving two segments are congruent or points are collinear).
2.  **Sturm's Theorem:** For rigorous positivity checking of univariate polynomials. This allows Hasclid to handle inequalities (e.g., distance > 0) without relying on numerical approximations.
3.  **Cylindrical Algebraic Decomposition (CAD) (Partial):** Utilizes polynomial discriminants and resultants to "project" multivariate problems into lower dimensions, enabling the solving of inequalities involving multiple variables.

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
*   `:reset`: Clear all assumptions (keeps lemmas).
*   `:clear`: Full factory reset.
*   `:load <file>`: Run a `.euclid` script.

### Proof & Solver Commands
*   `(op lhs rhs)`: Entering a raw formula attempts to prove it immediately.
    *   *Example:* `(= (dist2 A B) (dist2 A C))`
*   `:lemma (<op> <lhs> <rhs>)`: Proves a statement. If valid, adds it to the persistent knowledge base (Lemmas) for future proofs.
*   `:solve (<op> <lhs> <rhs>) <var1> [var2]`:
    *   **1D**: Solves for `var1` (e.g., find ranges where a polynomial is positive).
    *   **2D**: Uses CAD lifting to find regions of `(var1, var2)` that satisfy the condition.
*   `:project <expr> <var>`: Computes the discriminant (shadow) of a polynomial with respect to `var`.

## Codebase Structure

*   **`src/Expr.hs`**: Core AST for expressions and the Multivariate Polynomial engine (arithmetic, sparse map representation).
*   **`src/Prover.hs`**: The brain of Hasclid. Implements Buchberger's algorithm for Groebner Bases and orchestrates the logic for proving equalities and inequalities.
*   **`src/Sturm.hs`**: Implements Sturm sequences for real root counting and interval isolation. Critical for the inequality solver.
*   **`src/CAD.hs`**: Cylindrical Algebraic Decomposition tools. Implements recursive polynomials, pseudo-division, subresultants, and discriminants.
*   **`src/Parser.hs`**: A robust S-Expression parser for the prefix notation used in the REPL.
*   **`src/Main.hs`**: REPL loop, state management, and command dispatch.
*   **`theorems/`**: A collection of `.euclid` scripts demonstrating proofs (e.g., Thales, Apollonius, Stewart's Theorem).

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
