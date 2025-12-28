# Hasclid Geometric Theorem Prover - Language Reference

**Version 9.4**
**Language Type:** Domain-Specific Language (DSL) for Automated Geometric Theorem Proving
**Paradigm:** Declarative, Logic-based
**Computational Model:** Algebraic (Gröbner Basis + Wu's Method + CAD + SOS)

> **New in v9.4**: Full nth-root support with coefficient tracking (sqrt, cbrt, root n)

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Language Philosophy](#2-language-philosophy)
3. [Lexical Structure](#3-lexical-structure)
4. [Type System](#4-type-system)
5. [Expressions](#5-expressions)
6. [Geometric Primitives](#6-geometric-primitives)
7. [Formulas](#7-formulas)
8. [REPL Commands Reference](#8-repl-commands-reference)
9. [Proof Methods](#9-proof-methods)
10. [Inequality Proving](#10-inequality-proving)
11. [Proof Explanations](#11-proof-explanations)
12. [Solver Configuration](#12-solver-configuration)
13. [File Formats](#13-file-formats)
14. [Complete Examples](#14-complete-examples)

---

## 1. Introduction

Hasclid is a domain-specific language for expressing and proving geometric theorems using automated algebraic methods. The language combines:

- **Coordinate Geometry**: Points in 2D/3D space
- **Multiple Proof Methods**: Gröbner bases, Wu's method, CAD, Sum-of-Squares
- **Symbolic Computation**: Exact rational arithmetic
- **Inequality Proving**: Advanced SOS decomposition for polynomial inequalities

### Design Goals

1. **Expressiveness**: Natural notation for geometric concepts
2. **Precision**: Exact symbolic computation, no floating-point errors
3. **Automation**: Automatic proof generation via multiple methods
4. **Extensibility**: Lemma libraries for reusable theorems
5. **Explainability**: Human-readable proof explanations in ASCII or LaTeX

---

## 2. Language Philosophy

### Declarative Style
You **declare** what is true, and the system **proves** consequences:

```lisp
:point A 0 0        -- Declare point A at origin
:point B 3 0        -- Declare point B at (3,0)
(= (dist2 A B) 9)   -- Prove that distance² = 9
```

### S-Expression Syntax
All expressions use prefix notation (Lisp-style):

```lisp
(+ 1 2 3)           -- Addition: 1 + 2 + 3 = 6
(* 2 (+ x 1))       -- Multiplication: 2 * (x + 1)
(= (^ x 2) 4)       -- Equation: x² = 4
```

### Coordinate Approach
Geometric objects are represented via coordinates:

- **Point P** → Three variables: `xP`, `yP`, `zP`
- **Line AB** → Direction vector: `(xB - xA, yB - yA, zB - zA)`
- **Distance** → Euclidean distance formula

---

## 3. Lexical Structure

### Identifiers
```
identifier ::= letter (letter | digit)*
```

**Valid:** `A`, `P1`, `Midpoint`, `triangle_ABC`
**Invalid:** `1Point` (starts with digit), `x-coord` (hyphen not allowed)

### Numbers
```
rational ::= integer ["/" positive_integer]
```

**Examples:**
- `42` (integer)
- `-17` (negative integer)
- `3/4` (rational)
- `-22/7` (negative rational)

**Note:** All arithmetic is exact rational arithmetic. No floating-point!

### Comments
```
line-comment ::= "--" <text> | "//" <text> | "#" <text> | ";" <text>
block-comment ::= "#|" <text> "|#"
```

**Example:**
```lisp
; This is a comment
:point A 0 0  ; Inline comment
#| This whole section is ignored |#
```

### Root Selection Helper
```lisp
(root-between var poly lo hi)
```

Sugar for `(and (= poly 0) (> var lo) (< var hi))`. Use it to bind a specific root of a polynomial within an interval.

### Whitespace
Spaces, tabs, and newlines separate tokens but are otherwise insignificant.

---

## 4. Type System

Hasclid has an implicit type system:

### Types

1. **Rational** - Exact rational numbers (ℚ)
2. **Integer** - Exact integers (ℤ)
3. **Expression** - Symbolic polynomial expressions
4. **Point** - Identifier representing a geometric point
5. **Formula** - Relational expression (equation or inequality)

### Type Rules

```
+ : Expr × Expr → Expr
* : Expr × Expr → Expr
dist2 : Point × Point → Expr
= : Expr × Expr → Formula
>= : Expr × Expr → Formula
int : String → Expr
```

### Implicit Coercions

- **Rational → Expression**: `3` automatically becomes `Const(3/1)`
- **Point → Variables**: Point `A` creates `xA`, `yA`, `zA`

---

## 5. Expressions

### Grammar

```bnf
<expr> ::= <var>                    -- Variable
         | <const>                  -- Constant
         | (+ <expr>+)              -- Addition
         | (- <expr> <expr>)        -- Subtraction
         | (* <expr>+)              -- Multiplication
         | (/ <expr> <expr>)        -- Division
         | (^ <expr> <nat>)         -- Power
         | <geometric-primitive>    -- Geometry
         | (int <var>)              -- Integer Variable
         | (int-const <int>)        -- Integer Constant
```

### Arithmetic Operators

| Operator | Arity | Example | Meaning |
|----------|-------|---------|---------|
| `+` | n-ary | `(+ 1 2 3)` | Sum: 1+2+3 |
| `-` | 1 or 2 | `(- 5 2)` | Subtraction: 5-2 |
| `*` | n-ary | `(* 2 x y)` | Product: 2·x·y |
| `/` | 2 | `(/ 10 3)` | Division: 10/3 |
| `^` | 2 | `(^ x 3)` | Power: x³ |
| `x`, `y`, `z` | 1 | `(x A)` | Coordinate x of point A |
| `(root n e)` | Nth root | `(root 4 16)` |
| `sin`, `cos`, `tan` | 1 | `(sin x)` | Trigonometric functions |
| `asin`, `acos`, `atan` | 1 | `(atan x)` | Inverse trigonometric functions |

### Comparison Operators

**Note:** Exponent must be a natural number constant.

### Examples

```lisp
(+ (* 2 x) 5)              -- 2x + 5
(- (^ x 2) 1)              -- x² - 1
(/ (+ a b) 2)              -- (a + b) / 2
(* (+ x 1) (- x 1))        -- (x + 1)(x - 1)
```

---

## 6. Geometric Primitives

All geometric primitives return **symbolic expressions** (polynomials).

### 6.1 Distance Squared

```lisp
(dist2 P Q)
```

**Returns:** `(xP - xQ)² + (yP - yQ)² + (zP - zQ)²`

**Example:**
```lisp
:point A 0 0
:point B 3 4
(= (dist2 A B) 25)    -- Proves AB² = 25
```

### 6.2 Collinear Points

```lisp
(collinear A B C)
```

**Returns:** Cross product magnitude = 0 (for 2D)

**Meaning:** Points A, B, C lie on the same line

**Formula:** `(xB - xA)(yC - yA) - (yB - yA)(xC - xA) = 0`

**Example:**
```lisp
:point A 0 0
:point B 1 1
:point C 2 2
(= (collinear A B C) 0)   -- Proves collinearity
```

### 6.3 Dot Product

```lisp
(dot A B C D)
```

**Returns:** `AB · CD` (dot product of vectors)

**Formula:** `(xB - xA)(xD - xC) + (yB - yA)(yD - yC) + (zB - zA)(zD - zC)`

**Example:**
```lisp
:point O 0 0
:point X 1 0
:point Y 0 1
(= (dot O X O Y) 0)    -- Proves OX ⊥ OY
```

### 6.4 Circle

```lisp
(circle P C r)
```

**Returns:** Distance from P to C minus r²

**Meaning:** Point P lies on circle with center C and radius r

**Formula:** `dist2(P, C) - r²`

**Example:**
```lisp
:point P 3 4
:point O 0 0
(= (circle P O 5) 0)   -- P is on circle radius 5 centered at O
```

### 6.5 Midpoint

```lisp
(midpoint A B M)
```

**Returns:** Constraint that M is midpoint of AB

**Formula:** `(2xM - xA - xB)² + (2yM - yA - yB)² + (2zM - zA - zB)²`

**Example:**
```lisp
:point A 0 0
:point B 4 0
:point M 2 0
:assume (= (midpoint A B M) 0)
(= (dist2 A M) (dist2 M B))    -- AM = MB
```

### 6.6 Perpendicular

```lisp
(perpendicular A B C D)
```

**Returns:** Dot product of vectors AB and CD

**Meaning:** Line AB is perpendicular to line CD

**Example:**
```lisp
:point A 0 0
:point B 1 0
:point C 0 0
:point D 0 1
(= (perpendicular A B C D) 0)   -- AB ⊥ CD
```

### 6.7 Parallel

```lisp
(parallel A B C D)
```

**Returns:** Magnitude squared of cross product AB × CD

**Meaning:** Line AB is parallel to line CD

**Example:**
```lisp
:point A 0 0
:point B 1 0
:point C 0 1
:point D 1 1
(= (parallel A B C D) 0)   -- AB ∥ CD
```

---

## 7. Formulas

Formulas are **relational expressions** that can be proved or assumed.

### Grammar

```bnf
<formula> ::= (= <expr> <expr>)    -- Equality
            | (>= <expr> <expr>)   -- Greater-or-equal
            | (> <expr> <expr>)    -- Strictly greater
            | (<= <expr> <expr>)   -- Less-or-equal
            | (< <expr> <expr>)    -- Strictly less
```

### Semantics

| Relation | Proves | Method |
|----------|--------|--------|
| `=` | Algebraic equality | Gröbner basis / Wu's method |
| `>=` | Non-negativity | SOS decomposition / CAD / Sturm |
| `>` | Strict positivity | CAD / Sturm sequences |
| `<=` | Non-positivity | Transformed to `>=` |
| `<` | Strict negativity | Transformed to `>` |

### Examples

```lisp
(= x 5)                    -- x equals 5
(= (+ x y) 10)             -- x + y = 10
(>= (^ x 2) 0)             -- x² ≥ 0 (always true)
(> (dist2 A B) 0)          -- Distance is positive
```

---

## 8. REPL Commands Reference

Commands start with `:` and control the prover's behavior.

### 8.1 Geometry Commands

#### Define Point (2D)
```lisp
:point <name> <x> <y>
```

Creates point with `z = 0`.

**Example:**
```lisp
:point A 3 4     -- A at (3, 4, 0)
:point B x y     -- B at symbolic (x, y, 0)
:point C 1/2 3/4 -- C at (0.5, 0.75, 0)
```

#### Define Point (3D)
```lisp
:point <name> <x> <y> <z>
```

**Example:**
```lisp
:point P 1 2 3   -- P at (1, 2, 3)
```

#### Define Point Inside Triangle
```lisp
:inside <P> <A> <B> <C>
```

Constrains point P to lie strictly inside triangle ABC using barycentric coordinates.

**Example:**
```lisp
:point A 0 0
:point B 3 0
:point C 0 3
:inside P A B C   -- P is inside triangle ABC
```

This adds constraints that the barycentric coordinates `ba_u`, `ba_v`, `ba_w` are all positive and sum to 1.

### 8.2 Assumption and Lemma Commands

#### Add Assumption
```lisp
:assume <formula>
```

Adds a formula as an axiom (assumed true).

**Example:**
```lisp
:assume (= x 5)
:assume (= (midpoint A B M) 0)
:assume (>= a 0)               -- a is non-negative
```

#### Prove and Store as Lemma
```lisp
:lemma <formula>
```

Stores the formula for future use (without proving it).

**Example:**
```lisp
:lemma (= (dist2 A B) 9)
```

### 8.3 Proof Commands

#### Auto-Prove (Recommended)
```lisp
:auto <formula>
```

or simply:

```lisp
<formula>
```

Attempts to prove the formula using the best available method automatically.

**Example:**
```lisp
:auto (= (dist2 A B) 25)
(= (dist2 A B) 25)          -- Same as above
```

#### Prove Explicitly
```lisp
:prove <formula>
```

Attempts to prove using the standard solver pipeline.

#### Wu's Method
```lisp
:wu <formula>
```

Uses Wu's characteristic set method for equality proofs.

**Example:**
```lisp
:wu (= (collinear A B C) 0)
```

#### Induction Proof
```lisp
:induction <formula>
```

Attempts to prove a universal property `(forall ((int n)) P(n))` using structural induction.

**Example:**
```lisp
:declare-int n
:induction (forall ((int n)) (= (sum i 0 n i) (/ (* n (+ n 1)) 2)))
```

### 8.4 Macro Commands

#### Define Macro
```lisp
:macro <name> <params> = <body>
```

or

```lisp
:defmacro <name> <params> = <body>
```

Defines a reusable macro that expands during parsing.

**Example:**
```lisp
:macro sq x = (^ x 2)
:macro cube x = (* x x x)
:macro dist_sq A B = (+ (sq (- (x A) (x B))) (sq (- (y A) (y B))))

-- Now use them:
(= (sq 3) 9)                   -- 3² = 9
(>= (+ (sq a) (sq b) (sq c)) 0)  -- a² + b² + c² ≥ 0
```

#### List Macros
```lisp
:list-macros
```

Shows all defined macros.

#### Clear Macros
```lisp
:clear-macros
```

Removes all macros.

### 8.5 Integer Arithmetic Commands

#### Declare Integer Variables
```lisp
:declare-int <var1> <var2> ...
```

Declares variables as integers for integer-specific solving.

**Example:**
```lisp
:declare-int n k
(>= (* n (+ n 1)) 0)   -- n(n+1) ≥ 0 for integers
```

#### Toggle Brute-Force Search
```lisp
:bruteforce on|off
```

Enables/disables bounded brute-force search for integer goals.

#### Lagrange's Four-Square Theorem
```lisp
:lagrange <n>
```

Finds four squares that sum to n (Lagrange's theorem).

**Example:**
```lisp
:lagrange 42
-- Output: Lagrange sum of 4 squares for 42: [1, 1, 4, 4]
```

### 8.6 Lemma Library Commands

#### Save Lemmas
```lisp
:save-lemmas <filename>
```

Saves all proven lemmas to a file.

**Example:**
```lisp
:save-lemmas geometry_basics.lemmas
```

#### Load Lemmas
```lisp
:load-lemmas <filename>
```

Loads lemmas from a file.

#### List Lemmas
```lisp
:list-lemmas
```

Shows all stored lemmas.

#### Clear Lemmas
```lisp
:clear-lemmas
```

Removes all stored lemmas.

### 8.7 State Management Commands

| Command | Description |
|---------|-------------|
| `:reset` | Clear assumptions (keep lemmas and cache) |
| `:soft-reset` | Reset theory only (keep everything else) |
| `:clear` | Full reset (clear everything) |
| `:list` | Show current theory (assumptions) |
| `:history` | Show command history |
| `:clean` / `:cls` | Clear screen |

### 8.8 File Commands

#### Load Script
```lisp
:load <filename>
```

Executes all commands from a `.euclid` file.

**Example:**
```lisp
:load examples/pythagorean.euclid
```

#### Solve File
```lisp
:solve <filename>
```

Solves each formula in the file independently.

### 8.9 Help and Exit

| Command | Description |
|---------|-------------|
| `:help` | Show all commands |
| `:q` / `:quit` / `quit` / `exit` | Exit the prover |

---

## 9. Proof Methods

Hasclid uses multiple proof methods automatically selected based on the goal type.

### 9.1 Gröbner Basis Method

**For:** Polynomial equalities

**How it works:**
1. Convert formula `f = g` to polynomial `p = f - g`
2. Apply substitutions from assumptions
3. Compute Gröbner basis of constraint ideal
4. Reduce `p` to normal form using the basis
5. If normal form is 0, the equality is proved

**Soundness:** Based on Hilbert's Nullstellensatz

**Example:**
```lisp
:point A 0 0
:point B 3 0
:point C 0 4
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))  -- Pythagorean theorem: 9 + 16 = 25
```

### 9.2 Wu's Method

**For:** Polynomial equalities with geometric hypotheses

**How it works:**
1. Compute characteristic set of hypotheses
2. Pseudo-divide conclusion by characteristic set
3. If remainder is 0 (under non-degeneracy conditions), proved

**When to use:** Complex geometric theorems with multiple hypotheses

**Example:**
```lisp
:wu (= (collinear M N P) 0)
```

### 9.3 CAD (Cylindrical Algebraic Decomposition)

**For:** Polynomial inequalities, especially multivariate

**How it works:**
1. Decompose space into cylindrical cells
2. Check sign of polynomial in each cell
3. Verify inequality holds in all relevant cells

**Example:**
```lisp
:solve (> (+ (* x x) (* y y) -1) 0) x y
```

### 9.4 Sturm Sequences

**For:** Univariate polynomial positivity

**How it works:**
1. Compute Sturm sequence of polynomial
2. Count sign changes to determine root locations
3. Verify positivity on relevant intervals

---

## 10. Inequality Proving

Hasclid has advanced capabilities for proving polynomial inequalities using Sum-of-Squares (SOS) decomposition.

### 10.1 Basic Inequality Proofs

```lisp
-- Square is always non-negative
(>= (^ x 2) 0)

-- Sum of squares is non-negative
(>= (+ (^ x 2) (^ y 2)) 0)
```

### 10.2 Three-Variable Symmetric Inequalities

Hasclid can automatically prove classic symmetric inequalities:

```lisp
-- Prove: a² + b² + c² ≥ ab + bc + ca
(>= (- (+ (^ a 2) (^ b 2) (^ c 2))
       (+ (* a b) (* b c) (* c a)))
    0)
```

**SOS Decomposition Found:**
```
½(a - b)² + ½(b - c)² + ½(c - a)²
```

### 10.3 AM-GM Inequality

Two-variable AM-GM:

```lisp
:assume (>= x 0)
:assume (>= y 0)
(>= (- (+ x y) (* 2 (sqrt (* x y)))) 0)
```

### 10.4 Cauchy-Schwarz Pattern

```lisp
-- (a² + b²)(c² + d²) ≥ (ac + bd)²
(>= (- (* (+ (^ a 2) (^ b 2)) (+ (^ c 2) (^ d 2)))
       (^ (+ (* a c) (* b d)) 2))
    0)
```

**SOS Decomposition (Lagrange Identity):**
```
(ad - bc)²
```

### 10.5 Cyclic Inequalities

For n variables in cyclic patterns:

```lisp
-- 4-variable cyclic: a² + b² + c² + d² ≥ ab + bc + cd + da
(>= (- (+ (^ a 2) (^ b 2) (^ c 2) (^ d 2))
       (+ (* a b) (* b c) (* c d) (* d a)))
    0)
```

### 10.6 Robust Pattern Matching

The SOS prover handles:

- **Variable renaming:** `x, y, z` or `alpha, beta, gamma`
- **Term reordering:** Any order of terms
- **Coefficient scaling:** `2(a² + b² + c²) ≥ 2(ab + bc + ca)`
- **Asymmetric cases:** Falls back to Cholesky decomposition

---

## 11. Proof Explanations

Hasclid can generate human-readable proof explanations in ASCII or LaTeX format.

### 11.1 Explanation Commands

#### Show Explanation
```lisp
:explain
```

Shows step-by-step explanation of the last proof.

#### Set Format
```lisp
:format ascii|latex
```

Sets the output format for explanations.

**ASCII example:**
```
Step 1: Expanding definitions
  The goal polynomial is: a² + b² + c² - ab - bc - ca

Step 2: Finding SOS decomposition
  This factors as: (1/2)(a-b)² + (1/2)(b-c)² + (1/2)(c-a)²

Conclusion: Since this is a sum of squares, it is >= 0. QED
```

**LaTeX example:**
```latex
\textbf{Step 1: Expanding definitions}
The goal polynomial is: $a^2 + b^2 + c^2 - ab - bc - ca$

\textbf{Step 2: Finding SOS decomposition}
This factors as: $\frac{1}{2}(a-b)^2 + \frac{1}{2}(b-c)^2 + \frac{1}{2}(c-a)^2$

\textbf{Conclusion:} Since this is a sum of squares, it is $\geq 0$. $\square$
```

#### Set Detail Level
```lisp
:detail minimal|brief|normal|verbose
```

| Level | Description |
|-------|-------------|
| `minimal` | Equations only |
| `brief` | Key steps |
| `normal` | Standard explanation (default) |
| `verbose` | Full details with intermediate computations |

#### Export to File
```lisp
:export <filename>
```

Exports the proof explanation to a file.

**Example:**
```lisp
:format latex
:detail normal
:export proof.tex    -- Creates LaTeX file
:export proof.txt    -- Creates text file
```

### 11.2 Verbose Mode

```lisp
:verbose
```

Toggles verbose mode for detailed proof output including:
- All substitutions applied
- Gröbner basis computed
- Reduction steps
- SOS decomposition details

---

## 12. Solver Configuration

### 12.1 Timeout Settings

```lisp
:set-timeout <seconds>
```

Sets the solver timeout (default: 30 seconds, max: 600 seconds).

```lisp
:show-timeout
```

Shows current timeout setting.

**Example:**
```lisp
:set-timeout 120   -- 2 minutes for complex proofs
```

### 12.2 Term Ordering

```lisp
:set-order <order>
```

Sets the monomial ordering for Gröbner basis computation.

| Order | Description |
|-------|-------------|
| `grevlex` | Graded reverse lexicographic (default, usually fastest) |
| `lex` | Lexicographic (good for elimination) |
| `gradedlex` | Graded lexicographic |

```lisp
:show-order
```

Shows current term ordering.

### 12.3 Gröbner Backend

```lisp
:set-gb-backend buchberger|f5
```

| Backend | Description |
|---------|-------------|
| `buchberger` | Classic Buchberger algorithm |
| `f5` | Faugère's F5 algorithm (default, faster for large systems with signature-based reduction) |

The F5 algorithm uses signature-based criteria to avoid redundant S-polynomial reductions, making it significantly faster than both Buchberger and F4 for most polynomial systems.

### 12.4 Optimization Settings

```lisp
:optimize on|off
```

Toggles Buchberger optimization (selection strategy, sugar criterion).

```lisp
:set-strategy normal|sugar|minimal
```

Sets the S-polynomial selection strategy:
- `normal`: Standard selection
- `sugar`: Sugar criterion (often faster)
- `minimal`: Minimal degree first

### 12.5 Proof Mode

```lisp
:proof-mode sound|unsafe
```

| Mode | Description |
|------|-------------|
| `sound` | Only use sound methods (no heuristics) |
| `unsafe` | Allow heuristics for faster proofs |

### 12.6 Cache Management

```lisp
:cache-stats    -- Show Gröbner cache statistics
:clear-cache    -- Clear the cache
```

### 12.7 Auto-Simplification

```lisp
:auto-simplify
```

Toggles automatic expression simplification.

---

## 13. File Formats

### 13.1 Script Files (.euclid)

**Format:** Plain text, UTF-8 encoding

**Structure:**
```lisp
-- Comments start with --
<command>
<command>
...
```

**Example (pythagorean.euclid):**
```lisp
-- Pythagorean Theorem Proof
-- A right triangle with legs 3 and 4 has hypotenuse 5

:point A 0 0
:point B 3 0
:point C 0 4

-- Prove Pythagorean theorem
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))
```

### 13.2 Lemma Files (.lemmas)

**Format:** Plain text, S-expressions

**Structure:**
```lisp
-- Lemma Library
-- Saved: <count> lemmas
-- File: <filename>

<formula>
<formula>
...
```

---

## 14. Complete Examples

### Example 1: Pythagorean Theorem

```lisp
-- Define a right triangle
:point A 0 0
:point B 3 0
:point C 0 4

-- Prove AB² + AC² = BC²
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))
-- Output: RESULT: PROVED
```

### Example 2: Midpoint Theorem

```lisp
:point A 0 0
:point B 4 0
:point M xM yM

-- M is the midpoint of AB
:assume (= (midpoint A B M) 0)

-- Prove AM = MB
(= (dist2 A M) (dist2 M B))
-- Output: RESULT: PROVED
```

### Example 3: Collinearity

```lisp
:point A 0 0
:point B 1 2
:point C 2 4

(= (collinear A B C) 0)
-- Output: RESULT: PROVED (points are collinear: slope 2)
```

### Example 4: Three-Variable Inequality

```lisp
-- Prove: a² + b² + c² ≥ ab + bc + ca for all real a, b, c

(>= (- (+ (^ a 2) (^ b 2) (^ c 2))
       (+ (* a b) (* b c) (* c a)))
    0)

-- Output: RESULT: PROVED
-- SOS Decomposition: ½(a-b)² + ½(b-c)² + ½(c-a)²
```

### Example 5: Using Macros

```lisp
-- Define useful macros
:macro sq x = (^ x 2)
:macro norm_sq P Q = (+ (sq (- (x P) (x Q))) (sq (- (y P) (y Q))))

:point A 0 0
:point B 5 0
:point C 0 12

-- Use macros in proofs
(= (norm_sq A B) 25)    -- 5² = 25
(= (norm_sq A C) 144)   -- 12² = 144
(= (norm_sq B C) 169)   -- 13² = 169 (5-12-13 right triangle)
```

### Example 6: Inequality with Assumptions

```lisp
-- Prove (x + y)² ≥ 4xy for all real x, y

(>= (- (sq (+ x y)) (* 4 x y)) 0)

-- This expands to: x² + 2xy + y² - 4xy = x² - 2xy + y² = (x-y)²
-- Output: RESULT: PROVED
-- SOS Decomposition: (x - y)²
```

### Example 7: Cauchy-Schwarz Inequality

```lisp
-- (a² + b²)(c² + d²) ≥ (ac + bd)²

:macro lhs = (* (+ (sq a) (sq b)) (+ (sq c) (sq d)))
:macro rhs = (sq (+ (* a c) (* b d)))

(>= (- lhs rhs) 0)
-- Output: RESULT: PROVED
-- SOS Decomposition: (ad - bc)²
```

### Example 8: Exporting a Proof

```lisp
:point A 0 0
:point B 3 4

-- Prove and explain
(= (dist2 A B) 25)

-- Configure and export
:format latex
:detail normal
:export pythagorean_proof.tex

-- Output: Proof explanation exported to: pythagorean_proof.tex
```

### Example 9: Complex Geometric Theorem (Simson Line)

```lisp
-- Simson's Theorem: For point P on circumcircle of ABC,
-- the feet of perpendiculars from P to the sides are collinear

:point A 0 0
:point B 1 0
:point C xC yC

-- P on circumcircle (parametric)
:point P xP yP
:assume (= (circle P O r) 0)

-- Perpendicular feet D, E, F
:point D xD yD
:point E xE yE
:point F xF yF

:assume (= (perpendicular P D A B) 0)
:assume (= (collinear A D B) 0)
-- ... additional constraints

:wu (= (collinear D E F) 0)
```

### Example 10: Integer Arithmetic

```lisp
:declare-int n

-- Sum of first n integers formula
:induction (forall ((int n)) (= (sum i 1 n i) (/ (* n (+ n 1)) 2)))

-- Lagrange's four squares
:lagrange 100
-- Output: [0, 0, 6, 8] (0² + 0² + 6² + 8² = 100)
```

---

## Appendix: Quick Reference Card

### Geometric Primitives
```lisp
(dist2 A B)               -- Distance² between A and B
(collinear A B C)         -- A, B, C are collinear (returns polynomial)
(dot A B C D)             -- Dot product AB · CD
(circle P C r)            -- P on circle center C radius r
(midpoint A B M)          -- M is midpoint of AB
(perpendicular A B C D)   -- AB ⊥ CD
(parallel A B C D)        -- AB ∥ CD
```

### Essential Commands
```lisp
:point A x y [z]          -- Define point
:assume (= expr 0)        -- Add assumption
:assume (>= expr 0)       -- Add inequality assumption
:lemma (= expr 0)         -- Store lemma
:prove (= lhs rhs)        -- Prove equality
:auto (>= lhs rhs)        -- Auto-prove inequality
:wu (= lhs rhs)           -- Wu's method
:verbose                  -- Toggle detailed output
:explain                  -- Show proof explanation
:format ascii|latex       -- Set explanation format
:detail brief|normal|verbose -- Set detail level
:export <file>            -- Export proof to file
```

### Configuration Commands
```lisp
:set-timeout <seconds>    -- Set solver timeout
:set-order grevlex|lex    -- Set term ordering
:set-gb-backend f5        -- Use F5 algorithm (default)
:optimize on              -- Enable optimization
:set-strategy sugar       -- Use sugar strategy
```

### State Commands
```lisp
:list                     -- Show assumptions
:list-lemmas              -- Show lemmas
:reset                    -- Clear assumptions
:clear                    -- Full reset
:help                     -- Show help
:q                        -- Quit
```

---

**End of Language Reference**
