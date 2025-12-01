# Hasclid: A Haskell-based Euclidean Geometry Theorem Prover

![Haskell Logo](https://img.shields.io/badge/Haskell-5E5086?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal Build](https://img.shields.io/badge/Cabal-Build-blueviolet?style=for-the-badge&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMjggMTI4Ij48cGF0aCBkPSJNNi42IDYuNGwxNi45IDE2LjkgNS4zLTUuMy0xNi45LTE2LjljLTEuMi0xLjItMy4yLTEuMi00LjUgMHptNjUuOC02LjNsNDkuNyA0OS44Yy0yLjUgNS41LTQuNyA5LjEtMTAuNyA1LjYtNy42LTQuNS01LjUtMTAuNi01LjUtMTcuNXMxMy42LTE1LjMgMTMuNi0xNS4zTDM2LjggODguNEw2OC40IDQ3N2MuNS0uNyAxLjctMS43IDIuNS0xLjcgMC0uNSAxLjEtMS4xIDEuNy0xLjh2LTEuMWMuNS0uNyAxLjUtLjggMi41LS44LjUgMCAxLjYtLjYgMi4xLS42di0uNWMuOS0uNSAxLjYtLjggMi42LS44LjUgMCAxLjYtLjYgMi4xLS42di0uNWMuOS0uNyAxLjUtLjcgMi41LS43LjYgMCAxLjUtLjYgMi4xLS42di0uNWMuOS0uNyAxLjUtLjcgMi41LS43LjcgMCAxLjYtLjUgMi4yLS41di0uNWMuOS0uNyAxLjYtLjcgMi41LS43LjYgMCAxLjQtLjUgMi0uNXYtLjZjMS4yLS41IDEuNy0xLjEgMi41LTEuMXMuOC0uNSAxLjYtLjVWMzVjLjctLjUgMS0uOCAxLjUtLjguNiAwIDEuNS0uNCAyLS40di0uNmMuNi0uNSAxLjMtLjUgMi4xLS41LjQgMCAxLjQtLjMgMS44LS40di0uNmMuNi0uNCAxLjItLjQgMi4xLS40LjUgMCAxLjMtLjMgMS43LS40di0uNnYtLjNjMS4zLS41IDEuNi0xIDEuOC0xLjQuMi0uMy42LS42IDEuMi0uNjVWMzJjMS4zLS40IDEuNy0uNyAyLjUtLjcuNiAwIC41LS4zIDEuMS0uM3MuOS0uNiAxLjUtLjZWMzFjMS4zLS4zIDEuNS0uNiAyLS42LjcgMCAxLjItLjMgMS42LS4zLjMgMCAxLjQtLjYgMS42LS43IDEtLjMgMS4zLS41IDEuNi0uNSAxLjEgMCAxLjQtLjUgMS42LS41IDEuNiAwIDEuOC0uNC42LS41VjI4YzEuNC0uMyAxLjctLjcgMi0uN3YtLjNjMS4xLS40IDEuNC0uNiAxLjctLjYgMCAwIC42LS40IDEtLjRzLjctLjUgMS0uNVYyNS45YzEuMi0uNSAxLjMtLjYgMS42LS42IDEuMiAwIDEuMy0uMyAxLjYtLjM2VjI0LjRjLjktLjUgMS4zLS47IDEuNy0uNyAwLS42LjUtMS4xIDEtMS41bC41LS41VjIyLjFjLjgtLjYgMS4yLS44IDEuOC0uOC41IDAgMS4xLS40IDEuNC0uNHYtLjVjLjktLjYgMS40LS47IDEuOS0uNy40IDAgMS4xLS40IDEuNC0uNFYxOS40YzEuMi0uNiAxLjYtLjkgMi4xLS45LjcgMCAuNy0uMyAxLjItLjMuNCAwIC45LS4zIDEuMy0uM3YtLjVjLjktLjYgMS40LS43IDEuOC0uNy40IDAgMS0uNCAxLjMtLjQ1VjE2LjJjMS4yLS42IDEuNi0xIDIuMS0xLjEuNiAwIC43LS4zIDEuMi0uMy4zIDAgLjgtLjMgMS4yLS4zdjLS41YzEuMi0uNiAxLjYtLjcgMi0uOC40IDAgMS4yLS40IDEuNS0uNFYxMy45YzEuMi0uNiAxLjYtLjggMi0uOC41IDAgMS0uMyAxLjMtLjMuMyAwIC43LS4zIDEtLjN2LS41YzEuMS0uNiAxLjQtLjggMS44LS44LjUgMCAx...

## Project Description

**Hasclid** (v7.2) is an advanced, interactive theorem prover for Euclidean geometry built in Haskell. It bridges the gap between geometric intuition and rigorous algebraic proof by translating geometric constraints into polynomial ideals.

The engine combines three powerful algebraic techniques to automate proofs:
1.  **Groebner Bases (Buchberger's Algorithm):** For verifying equality constraints (e.g., proving two segments are congruent or points are collinear).
2.  **Sturm's Theorem:** For rigorous positivity checking of univariate polynomials. This allows Hasclid to handle inequalities (e.g., distance > 0) without relying on numerical approximations.
3.  **Cylindrical Algebraic Decomposition (CAD) (Partial):** Utilizes polynomial discriminants and resultants to "project" multivariate problems into lower dimensions, enabling the solving of inequalities involving multiple variables.

Hasclid operates as a Read-Eval-Print Loop (REPL), providing a flexible environment to define points, script theorems, and explore algebraic geometry interactively.

## Features

*   **Symbolic Algebra Engine**: Custom sparse multivariate polynomial implementation with rational coefficients.
*   **Geometric Primitives**: First-class support for:
    *   `dist2`: Squared Euclidean distance.
    *   `collinear`: Collinearity checks (determinant-based).
    *   `dot`: Vector dot products.
    *   `circle`: Circle equation constraints.
*   **Advanced Proof Logic**:
    *   **Equality**: Automatically reduced using computed Groebner Bases of the hypothesis ideal.
    *   **Inequality**: Verified using root counting (Sturm sequences) and interval bisection.
*   **Interactive REPL**:
    *   **:point**: Define 2D/3D points.
    *   **:assume**: Add arbitrary polynomial constraints.
    *   **:lemma**: Prove and store intermediate results to the theory context.
    *   **:solve**: Find valid regions for variables satisfying an inequality (1D and 2D support).
    *   **:project**: Visualization of CAD projections (discriminants).
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