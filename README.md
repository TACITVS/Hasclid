# Hasclid: A Haskell-based Euclidean Geometry Theorem Prover

![Haskell Logo](https://img.shields.io/badge/Haskell-5E5086?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal Build](https://img.shields.io/badge/Cabal-Build-blueviolet?style=for-the-badge&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMjggMTI4Ij48cGF0aCBkPSJNNi42IDYuNGwxNi45IDE2LjkgNS4zLTUuMy0xNi45LTE2LjljLTEuMi0xLjItMy4yLTEuMi00LjUgMHptNjUuOC02LjNsNDkuNyA0OS44Yy0yLjUgNS41LTQuNyA5LjEtMTAuNyA1LjYtNy42LTQuNS01LjUtMTAuNi01LjUtMTcuNXMxMy42LTE1LjMgMTMuNi0xNS4zTDM2LjggODguNEw2OC40IDQ3N2MuNS0uNyAxLjctMS43IDIuNS0xLjcgMC0uNSAxLjEtMS4xIDEuNy0xLjh2LTEuMWMuNS0uNyAxLjUtLjggMi41LS44LjUgMCAxLjYtLjYgMi4xLS42di0uNWMuOS0uNSAxLjYtLjggMi42LS44LjUgMCAxLjYtLjYgMi4xLS42di0uNWMuOS0uNyAxLjUtLjcgMi41LS43LjYgMCAxLjUtLjYgMi4xLS42di0uNWMuOS0uNyAxLjUtLjcgMi41LS43LjcgMCAxLjYtLjUgMi4yLS41di0uNWMuOS0uNyAxLjYtLjcgMi41LS43LjYgMCAxLjQtLjUgMi0uNXYtLjZjMS4yLS41IDEuNy0xLjEgMi41LTEuMXMuOC0uNSAxLjYtLjVWMzVjLjctLjUgMS0uOCAxLjUtLjguNiAwIDEuNS0uNCAyLS40di0uNmMuNi0uNSAxLjMtLjUgMi4xLS41LjQgMCAxLjQtLjMgMS44LS40di0uNmMuNi0uNCAxLjItLjQgMi4xLS40LjUgMCAxLjMtLjMgMS43LS40di0uNnYtLjNjMS4zLS41IDEuNi0xIDEuOC0xLjQuMi0uMy42LS42IDEuMi0uNjVWMzJjMS4zLS40IDEuNy0uNyAyLjUtLjcuNiAwIC41LS4zIDEuMS0uM3MuOS0uNiAxLjUtLjZWMzFjMS4zLS4zIDEuNS0uNiAyLS42LjcgMCAxLjItLjMgMS42LS4zLjMgMCAxLjQtLjYgMS42LS43IDEtLjMgMS4zLS41IDEuNi0uNSAxLjEgMCAxLjQtLjUgMS42LS41IDEuNiAwIDEuOC0uNC42LS41VjI4YzEuNC0uMyAxLjctLjcgMi0uN3YtLjNjMS4xLS40IDEuNC0uNiAxLjctLjYgMCAwIC42LS40IDEtLjRzLjctLjUgMS0uNVYyNS45YzEuMi0uNSAxLjMtLjYgMS42LS42IDEuMiAwIDEuMy0uMyAxLjYtLjM2VjI0LjRjLjktLjUgMS4zLS43IDEuNy0uNyAwLS42LjUtMS4xIDEtMS41bC41LS41VjIyLjFjLjgtLjYgMS4yLS44IDEuOC0uOC41IDAgMS4xLS40IDEuNC0uNHYtLjVjLjktLjYgMS40LS43IDEuOS0uNy40IDAgMS4xLS40IDEuNC0uNFYxOS40YzEuMi0uNiAxLjYtLjkgMi4xLS45LjcgMCAuNy0uMyAxLjItLjMuNCAwIC45LS4zIDEuMy0uM3YtLjVjLjktLjYgMS40LS43IDEuOC0uNy40IDAgMS0uNCAxLjMtLjQ1VjE2LjJjMS4yLS42IDEuNi0xIDIuMS0xLjEuNiAwIC43LS4zIDEuMi0uMy4zIDAgLjgtLjMgMS4yLS4zdjLS41YzEuMi0uNiAxLjYtLjcgMi0uOC40IDAgMS4yLS40IDEuNS0uNFYxMy45YzEuMi0uNiAxLjYtLjggMi0uOC41IDAgMS0uMyAxLjMtLjMuMyAwIC43LS4zIDEtLjN2LS41YzEuMS0uNiAxLjQtLjggMS44LS44LjUgMCAxLS4zIDEuMy0uMy4zIDAgLjYtLjMgMS0uM3YtLjZjLjctLjUgMS0uNyAxLjYtLjcuNCAwIC44LS4zIDEuMi0uM3YyNy40YzAgNS42LTQuNCAxMC41LTEwLjIgMTAuNkg3LjdBNy4yIDcuMiAwIDAgMSA2LjYgNjEuMkw2LjYgNi40em0wIDU0LjljLTYuOSAwLTExLjgtNS0xMS44LTExLjZzNC45LTExLjYgMTEuOC0xMS42YzUuOCAwIDEwLjEgMy43IDExLjUgOC41LTIuNiAxLjMtNS43IDItOC43IDItMi42IDAtNS41LS44LTguNC0xLjgtMi42IDItNC42IDUuMy00LjYgOC44IDAgMy41IDIuMSA1LjcgNS44IDUuN3MzLjctMi4yIDMuNy01LjZoLTQuOHYtNS40aDEwLjF2OS40Yy0xLjMgNS41LTUuNSA4LjctMTAuNiA4LjdaTTQxLjMgMjguM3YyNS44aC02LjhWNDQuNmgtNi40VjI4LjNoLTQuMVY2MS44aDE1LjFjOC41IDAgMTIuNC01LjYgMTIuNC0xMS42IDAtNi41LTQuNy0xMS45LTEyLjQtMTEuOVptLTQuMSA3LjRjMy4zIDAgNS41IDIuNiA1LjUgNS41cy0yLjEgNS41LTUuNSA1LjVoLTcuNnYtMTF6Ii8+PC9zdmc+)

## Project Description

Hasclid is a powerful Haskell-based theorem prover designed for Euclidean geometry and inequalities. It leverages advanced algebraic techniques, including **Groebner Bases** for proving equalities and **Sturm's Theorem** for robust positivity checks crucial for inequalities.

This project aims to provide a clean, extensible, and efficient framework for exploring and verifying geometric and algebraic theorems. It operates as a Read-Eval-Print Loop (REPL), allowing users to define points, assume geometric properties or algebraic equations, and then attempt to prove new formulas.

## Features

*   **Symbolic Expression Handling**: Define variables, constants, and perform arithmetic operations.
*   **Geometric Primitives**: Built-in support for:
    *   `dist2`: Squared distance between two points.
    *   `collinear`: Check if three points are collinear.
    *   `dot`: Dot product of two vectors (defined by four points).
    *   `circle`: Define a point on a circle.
*   **REPL Interface**: Interactive command-line interface for theorem proving.
*   **Assumption Management**: Add assumptions (`:assume`) to build up a geometric theory.
*   **Point Definition**: Easily define 2D and 3D points (`:point`).
*   **Script Loading**: Load sequences of commands from `.euclid` files (`:load`).
*   **Equality Proving (Groebner Bases)**: Utilizes Buchberger's algorithm to prove polynomial equalities.
*   **Inequality Proving (Sturm's Theorem)**: Employs Sturm's theorem for reliable positivity checks on univariate polynomials for `>=` and `>` relations. Includes a heuristic for multivariate polynomials.
*   **Modular Design**: Clear separation of concerns with modules for Expression (AST), Parsing, Prover Logic, and Sturm's Theorem implementation.

## Installation

To build and run Hasclid, you will need a [Haskell toolchain](https://www.haskell.org/downloads/) installed, specifically **GHC** and **Cabal**.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/TACITVS/Hasclid.git
    cd Hasclid
    ```

2.  **Build the project:**
    ```bash
    cabal update
    cabal build
    ```
    Cabal will resolve and download the necessary dependencies (like `containers`) and compile the project.

## Usage

Once built, you can run the theorem prover directly from your terminal:

```bash
cabal run prover
```

This will start the REPL:

```
==========================================
   Euclid Geometric Prover v4.1 (Final)
   Type :help for commands.
==========================================
Euclid> 
```

### Commands

Type `:help` in the REPL for a list of available commands:

*   `:point A x y z` / `:point A x y`: Define a 3D or 2D point `A` with coordinates `(x, y, z)` or `(x, y, 0)`.
*   `:assume (= xA 0)`: Manually assume a formula. Formulas are given in S-expression prefix notation.
*   `:load file.euclid`: Load and execute commands from a `.euclid` script file.
*   `:list`: List all currently assumed formulas in the theory.
*   `:clear`: Clear the current theory (remove all assumptions).
*   `exit` / `quit`: Exit the REPL.

### Example

Here's a simple interaction to prove that `A` is at the origin:

```
Euclid> :point A 0 0
Defined 2D Point A at (0, 0)
Euclid> (= (dist2 A A) 0)
RESULT: PROVED (Equality Holds (Groebner Normal Form is 0))
Diff Normal Form: 0
Euclid> 
```

### Running Example Theorems

The `theorems/` directory contains various `.euclid` files demonstrating Hasclid's capabilities. You can load and run them:

```
Euclid> :load theorems/thales.euclid
Euclid> :load theorems/apollonius.euclid
```

## File Structure

The project is organized as follows:

*   `src/`: Contains all Haskell source code (`.hs` files) for the prover's logic.
*   `theorems/`: Stores example theorem definitions and scripts (`.euclid` files) that can be loaded into the REPL.
*   `prover.cabal`: The Cabal project file, defining dependencies, executables, and build instructions.
*   `.gitignore`: Specifies files and directories that Git should ignore (e.g., build artifacts).

## Contributing

Contributions are welcome! Please feel free to open issues or pull requests on the [GitHub repository](https://github.com/TACITVS/Hasclid).

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
