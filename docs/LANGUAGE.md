# Euclid Geometric Theorem Prover - Language Reference

**Version 7.3**
**Language Type:** Domain-Specific Language (DSL) for Automated Geometric Theorem Proving
**Paradigm:** Declarative, Logic-based
**Computational Model:** Algebraic (Gröbner Basis + CAD)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Language Philosophy](#language-philosophy)
3. [Lexical Structure](#lexical-structure)
4. [Type System](#type-system)
5. [Expressions](#expressions)
6. [Geometric Primitives](#geometric-primitives)
7. [Formulas](#formulas)
8. [Commands](#commands)
9. [Mathematical Semantics](#mathematical-semantics)
10. [File Formats](#file-formats)
11. [Examples](#examples)

---

## 1. Introduction

Euclid is a domain-specific language for expressing and proving geometric theorems using automated algebraic methods. The language combines:

- **Coordinate Geometry**: Points in 2D/3D space
- **Algebraic Methods**: Gröbner bases for equality proofs
- **Symbolic Computation**: Exact rational arithmetic
- **CAD Methods**: Cylindrical Algebraic Decomposition for inequalities

### Design Goals

1. **Expressiveness**: Natural notation for geometric concepts
2. **Precision**: Exact symbolic computation, no floating-point errors
3. **Automation**: Automatic proof generation via Gröbner bases
4. **Extensibility**: Lemma libraries for reusable theorems

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

- Line comments strip the rest of the line.
- Block comments are non-nested and removed even mid-line.

**Example:**
```lisp
; This is a comment
:point A 0 0  ; Inline comment
#| This whole section is ignored |#
```

### Root selection helper

```
(root-between var poly lo hi)
```

Sugar for `(and (= poly 0) (> var lo) (< var hi))`. Use it to bind a specific root of a polynomial within an interval (e.g., picking an interior trisector slope).

### Whitespace
Spaces, tabs, and newlines separate tokens but are otherwise insignificant.

---

## 4. Type System

Euclid has an implicit type system:

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
| `sum` | 4 | `(sum i 1 n i)` | Summation $\sum_{i=1}^n i$ |

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

---

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

---

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

---

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

---

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

---

### 6.6 Perpendicular

```lisp
(perpendicular A B C D)
```

**Returns:** Dot product of vectors AB and CD

**Meaning:** Line AB is perpendicular to line CD

**Formula:** Same as `(dot A B C D)`

**Example:**
```lisp
:point A 0 0
:point B 1 0
:point C 0 0
:point D 0 1
(= (perpendicular A B C D) 0)   -- AB ⊥ CD
```

---

### 6.7 Parallel

```lisp
(parallel A B C D)
```

**Returns:** Magnitude squared of cross product AB × CD

**Meaning:** Line AB is parallel to line CD

**Formula:** `|AB × CD|² = 0`

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
```

### Semantics

| Relation | Proves | Method |
|----------|--------|--------|
| `=` | Algebraic equality | Gröbner basis |
| `>=` | Non-negativity | Sturm sequences / SOS |
| `>` | Strict positivity | Sturm sequences |

### Examples

```lisp
(= x 5)                    -- x equals 5
(= (+ x y) 10)             -- x + y = 10
(>= (^ x 2) 0)             -- x² ≥ 0 (always true)
(> (dist2 A B) 0)          -- Distance is positive
```

---

## 8. Commands

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
```

#### Define Point (3D)
```lisp
:point <name> <x> <y> <z>
```

**Example:**
```lisp
:point P 1 2 3   -- P at (1, 2, 3)
```

**Coordinates can be:**
- **Constants:** `:point A 0 0`
- **Rationals:** `:point B 1/2 3/4`
- **Variables:** `:point C x y` (symbolic)

#### Define Macro
```lisp
:macro <name> <param1> <param2> ... = <body>
```

Defines a reusable macro that expands during parsing.

**Example:**
```lisp
:macro sq x = (^ x 2)
:macro dist_sq A B = (+ (sq (- (x A) (x B))) (sq (- (y A) (y B))))
```

**Note:** You can access point coordinates using `(x P)`, `(y P)`, `(z P)`.

#### Add Assumption
```lisp
:assume <formula>
```

Adds a formula as an axiom (assumed true).

**Example:**
```lisp
:assume (= x 5)
:assume (= (midpoint A B M) 0)
```

---

### 8.2 Proof Commands

#### Prove Formula
```lisp
<formula>
```

Attempts to prove the formula using current assumptions.

**Example:**
```lisp
:point A 0 0
:point B 3 0
(= (dist2 A B) 9)    -- Attempts proof
```

#### Prove and Store as Lemma
```lisp
:lemma <formula>
```

Proves the formula and stores it for future use.

**Example:**
```lisp
:lemma (= (dist2 A B) 9)
```

#### Prove by Induction
```lisp
:induction <formula>
```

Attempts to prove a universal property `(forall ((int n)) P(n))` using structural induction.
- **Base Case:** Proves `P(0)`.
- **Step Case:** Assumes `P(k)`, proves `P(k+1)`.
- Automatically handles `sum` recurrence relations.

**Example:**
```lisp
:declare-int n
:induction (forall ((int n)) (= (sum i 0 n i) (/ (* n (+ n 1)) 2)))
```

#### Toggle Verbose Mode
```lisp
:verbose
```

Shows detailed proof steps (substitutions, Gröbner basis, etc.).

**Output:**
```
==========================================
PROOF EXPLANATION:
==========================================

Used Assumptions (9):
  * xA = 0
  * yA = 0
  ...

Proof Steps:
  1. Used substitution: xA -> 0
  2. Computed Groebner basis (1 polynomials)
  3. Reduced to normal form: 0 (PROOF COMPLETE)
==========================================
```

---

### 8.3 Lemma Library Commands

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

Loads lemmas from a file and adds them to the current context.

**Example:**
```lisp
:load-lemmas geometry_basics.lemmas
```

#### Clear Lemmas
```lisp
:clear-lemmas
```

Removes all stored lemmas (assumptions remain).

---

### 8.4 Utility Commands

| Command | Description |
|---------|-------------|
| `:help` | Show all commands |
| `:load-lemmas <file>` | Load lemmas from a file |
| `:declare-int <vars>` | Declare variables as integers |
| `:list` | List assumptions and lemmas |
| `:history` | Show command history |
| `:clean` / `:cls` | Clear screen |
| `:reset` | Clear assumptions (keep lemmas) |
| `:clear` | Full reset (clear everything) |
| `:load <file>` | Execute script file |
| `:q` / `quit` / `exit` | Quit the prover |

---

### 8.5 Advanced Commands (CAD)

#### Project Polynomial
```lisp
:project <expr> <var>
```

Computes the CAD projection (discriminant) eliminating `var`.

**Example:**
```lisp
:project (+ (* x x) (* y y) -1) x
```

#### Solve Inequality (1D/2D)
```lisp
:solve <formula> <var1> [<var2>]
```

Finds sample points satisfying the inequality.

**Example:**
```lisp
:solve (> (+ (* x x) -4) 0) x          -- 1D
:solve (> (+ (* x x) (* y y) -1) 0) x y  -- 2D
```

---

## 9. Mathematical Semantics

### 9.1 Algebraic Representation

All geometric constructs are translated to **multivariate polynomials** over ℚ.

**Example:**
```
Point A at (a₁, a₂, a₃)  →  {xA = a₁, yA = a₂, zA = a₃}
dist2(A,B)               →  (xB - xA)² + (yB - yA)² + (zB - zA)²
```

### 9.2 Proof Method

**For Equalities:**

1. Convert formula `f = g` to polynomial `p = f - g`
2. Apply substitutions from assumptions
3. Compute **Gröbner basis** of constraint ideal
4. Reduce `p` to **normal form** using the basis
5. If normal form is 0, then `f = g` is **proved**

**Soundness:** Based on Hilbert's Nullstellensatz

**For Inequalities:**

- **1D Polynomials:** Use **Sturm sequences** to count real roots
- **Multivariate:** Cylindrical Algebraic Decomposition (CAD)
- **Heuristic:** Sum-of-Squares (SOS) decomposition

### 9.3 Variable Substitution

Variables bound by `:point` commands create **substitution map**:

```
:point A 2 3  →  {xA ↦ 2, yA ↦ 3, zA ↦ 0}
```

These are substituted before proof.

### 9.4 Constraint Assumptions

Constraint assumptions non-substitution assumptions (e.g., `(= (midpoint A B M) 0)`) become **ideal generators** for Gröbner basis computation.

### 9.5 Integer Arithmetic

Variables declared as **integers** (`:declare-int` or `(int x)`) restrict the domain to $\mathbb{Z}$.

**Proof Strategy:**
1.  **Linear Solver:** Uses Interval Analysis and a tailored Simplex-like method for linear integer arithmetic.
2.  **Algebraic Fallback:** For polynomial equalities (e.g., $x^4 + y^4 = z^4$), the system falls back to the **Gröbner Basis** engine. Since $\mathbb{Z} \subset \mathbb{R}$, any algebraic identity proving true over $\mathbb{R}$ is automatically true over $\mathbb{Z}$.

---

## 10. File Formats

### 10.1 Script Files (.euclid)

**Format:** Plain text, UTF-8 encoding

**Structure:**
```lisp
-- Comments start with --
<command>
<command>
...
```

**Example:**
```lisp
; Right Triangle Proof
:point A 0 0
:point B 3 0
:point C 0 4

; Prove Pythagorean theorem
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))
```

### 10.2 Lemma Files (.lemmas)

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

**Example:**
```lisp
-- Lemma Library
-- Saved: 2 lemmas
-- File: basics.lemmas

(= (dist2 A B) 9)
(= (perpendicular A B A C) 0)
```

---

## 11. Examples

### Example 1: Basic Distance Proof

```lisp
:point A 0 0
:point B 3 4
(= (dist2 A B) 25)   -- Proves 3² + 4² = 25
```

### Example 2: Midpoint Property

```lisp
:point A 0 0
:point B 4 0
:point M 2 0
:assume (= (midpoint A B M) 0)
(= (dist2 A M) (dist2 M B))   -- Proves AM = MB
```

### Example 3: Perpendicularity

```lisp
:point O 0 0
:point X 1 0
:point Y 0 1
(= (perpendicular O X O Y) 0)  -- Proves OX ⊥ OY
```

### Example 4: Lemma Library Workflow

```lisp
-- Prove and save
:point A 0 0
:point B 3 0
:lemma (= (dist2 A B) 9)
:save-lemmas my_theorems.lemmas

-- Later session
:clear-lemmas
:load-lemmas my_theorems.lemmas
-- Lemma is now available!
```

### Example 5: Collinearity

```lisp
:point A 0 0
:point B 1 2
:point C 2 4
(= (collinear A B C) 0)   -- Proves A, B, C are collinear
```

---

## Appendix: Quick Reference Card

### Geometric Primitives
```lisp
(dist2 A B)               -- Distance² between A and B
(collinear A B C)         -- A, B, C are collinear
(dot A B C D)             -- Dot product AB · CD
(circle P C r)            -- P on circle center C radius r
(midpoint A B M)          -- M is midpoint of AB
(perpendicular A B C D)   -- AB ⊥ CD
(parallel A B C D)        -- AB ∥ CD
```

### Commands
```lisp
:point A x y [z]          -- Define point
:assume (= expr 0)        -- Add assumption
:lemma (= expr 0)         -- Prove and store
:verbose                  -- Toggle detailed output
:save-lemmas file.lemmas  -- Save lemmas
:load-lemmas file.lemmas  -- Load lemmas
:list                     -- Show all assumptions/lemmas
:help                     -- Show help
```

---

**End of Language Reference**

### Root selection helper\n\n(root-between var poly lo hi) is sugar for (and (= poly 0) (> var lo) (< var hi)). Use it to bind a specific root of a polynomial inside an interval (e.g., selecting an interior trisector).\n
