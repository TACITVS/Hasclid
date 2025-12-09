# Euclid Theorem Prover - Tutorial

**Learn the Euclid language in 30 minutes**

Welcome to Euclid! This tutorial will teach you how to express and prove geometric theorems using automated algebraic methods.

---

## Table of Contents

1. [Getting Started](#lesson-1-getting-started)
2. [Your First Proof](#lesson-2-your-first-proof)
3. [Working with Points](#lesson-3-working-with-points)
4. [Geometric Primitives](#lesson-4-geometric-primitives)
5. [Assumptions and Context](#lesson-5-assumptions-and-context)
6. [Lemmas and Reusability](#lesson-6-lemmas-and-reusability)
7. [Advanced Features](#lesson-7-advanced-features)
8. [Troubleshooting](#lesson-8-troubleshooting)

---

## Lesson 1: Getting Started

### Starting the Prover

```bash
cabal run prover
```

You'll see:
```
==================================================
   Euclid Geometric Prover v9.1
   Now with Proof Explanations!
   Type :help for commands.
==================================================
Euclid>
```

### Your First Command

Type `:help` to see all available commands:

```
Euclid> :help
```

### Quitting

Type `:q`, `quit`, or `exit`:

```
Euclid> :q
Goodbye.
```

**Try it:** Start the prover, type `:help`, then `:q`.

---

## Lesson 2: Your First Proof

Let's prove something simple: **The distance from (0,0) to (3,4) is 5**.

### Step 1: Define Points

```lisp
:point A 0 0
:point B 3 4
```

Output:
```
Defined 2D Point A at (0, 0)
Defined 2D Point B at (3, 4)
```

### Step 2: State What You Want to Prove

We want to prove: `dist²(A, B) = 25`

In Euclid, write:
```lisp
(= (dist2 A B) 25)
```

**Explanation:**
- `(dist2 A B)` - squared distance between A and B
- `(= ... 25)` - equals 25
- Parentheses use prefix notation: `(operation arg1 arg2)`

### Step 3: See the Result

```
RESULT: PROVED (Equality Holds (Groebner Normal Form is 0))
Diff Normal Form: 0
```

**🎉 Congratulations! You proved your first theorem!**

### Complete Session

```lisp
:point A 0 0
:point B 3 4
(= (dist2 A B) 25)
```

**Try it:** Run this exact sequence in the prover.

---

## Lesson 3: Working with Points

### 2D Points

```lisp
:point P 3 4
```

This creates:
- `xP = 3`
- `yP = 4`
- `zP = 0` (automatically)

### 3D Points

```lisp
:point Q 1 2 3
```

This creates:
- `xQ = 1`
- `yQ = 2`
- `zQ = 3`

### Symbolic Coordinates

You can use variables instead of numbers:

```lisp
:point R a b
```

This creates:
- `xR = a` (symbolic)
- `yR = b` (symbolic)
- `zR = 0`

### Rational Coordinates

Euclid uses exact arithmetic!

```lisp
:point S 1/2 3/4
```

This creates:
- `xS = 1/2` (exactly, not 0.5)
- `yS = 3/4` (exactly, not 0.75)

**Try it:** Define points with different coordinate types.

---

## Lesson 4: Geometric Primitives

### Distance Squared

`(dist2 A B)` - Squared Euclidean distance

```lisp
:point A 0 0
:point B 3 0
(= (dist2 A B) 9)    -- Proves 3² = 9
```

**Why squared?** To avoid square roots (keep everything polynomial).

### Perpendicularity

`(perpendicular A B C D)` - AB ⊥ CD

```lisp
:point O 0 0
:point X 1 0
:point Y 0 1
(= (perpendicular O X O Y) 0)   -- Proves OX ⊥ OY
```

**Meaning:** The dot product of vectors OX and OY is zero.

### Parallel Lines

`(parallel A B C D)` - AB ∥ CD

```lisp
:point A 0 0
:point B 1 0
:point C 0 1
:point D 1 1
(= (parallel A B C D) 0)   -- Proves AB ∥ CD
```

**Meaning:** The cross product of vectors AB and CD is zero.

### Collinearity

`(collinear A B C)` - A, B, C are on the same line

```lisp
:point A 0 0
:point B 1 1
:point C 2 2
(= (collinear A B C) 0)   -- Proves collinearity
```

### Midpoint

`(midpoint A B M)` - M is the midpoint of AB

```lisp
:point A 0 0
:point B 4 0
:point M 2 0
:assume (= (midpoint A B M) 0)
(= (dist2 A M) (dist2 M B))   -- Proves AM = MB
```

**Try it:** Prove that the perpendicular sides of a rectangle are perpendicular!

---

## Lesson 5: Assumptions and Context

### Adding Assumptions

Use `:assume` to add axioms:

```lisp
:assume (= x 5)
```

Now `x` is substituted with `5` in all proofs.

### Viewing Current State

Use `:list` to see all assumptions and lemmas:

```lisp
:list
```

Output:
```
Active Assumptions:
1: xA = 0
2: yA = 0
3: x = 5

Proven Lemmas:
  (None)
```

### Resetting

- `:reset` - Clear assumptions (keep lemmas)
- `:clear` - Clear everything (full reset)

### Example: Right Triangle

```lisp
-- Define right triangle
:point A 0 0
:point B 3 0
:point C 0 4

-- Prove sides are perpendicular
(= (perpendicular A B A C) 0)

-- Prove Pythagorean theorem
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))
```

**Try it:** Run this example and see both proofs succeed!

---

## Lesson 6: Lemmas and Reusability

### Proving and Storing Lemmas

Use `:lemma` to prove and save theorems:

```lisp
:point A 0 0
:point B 3 0
:lemma (= (dist2 A B) 9)
```

Output:
```
LEMMA ESTABLISHED: Equality Holds (Groebner Normal Form is 0)
(Saved)
```

### Saving to File

```lisp
:save-lemmas my_theorems.lemmas
```

Output:
```
Saved 1 lemmas to my_theorems.lemmas
```

### Loading from File

```lisp
:load-lemmas my_theorems.lemmas
```

Output:
```
Loaded 1 lemmas from my_theorems.lemmas
Total lemmas: 1
```

### Clearing Lemmas

```lisp
:clear-lemmas
```

### Complete Workflow

```lisp
-- Session 1: Prove and save
:point A 0 0
:point B 3 0
:lemma (= (dist2 A B) 9)
:save-lemmas basics.lemmas
:q

-- Session 2: Load and use
:load-lemmas basics.lemmas
:list    -- See loaded lemma
```

**Try it:** Prove a lemma, save it, restart the prover, and load it back!

---

## Lesson 7: Advanced Features

### Verbose Mode

See detailed proof steps:

```lisp
:verbose
:point A 0 0
:point B 3 0
(= (dist2 A B) 9)
```

Output shows:
```
==========================================
PROOF EXPLANATION:
==========================================

Used Assumptions (6):
  * xB = 3
  * yB = 0
  * zB = 0
  * xA = 0
  * yA = 0
  * zA = 0

Proof Steps:
  1. Used substitution: xB -> 3
  2. Used substitution: yB -> 0
  ...
  7. Reduced to normal form: 0 (PROOF COMPLETE)

Groebner Basis Size: 0
==========================================
```

### Script Files

Create a file `my_proof.euclid`:

```lisp
-- My First Proof Script
:point A 0 0
:point B 3 4
(= (dist2 A B) 25)
```

Load and run:

```lisp
:load my_proof.euclid
```

### Inequality Solving (Advanced)

Solve `x² - 4 > 0`:

```lisp
:solve (> (+ (* x x) -4) 0) x
```

Project polynomial (eliminate variable):

```lisp
:project (+ (* x x) (* y y) -1) x
```

---

## Lesson 8: Troubleshooting

### Common Errors

#### Parse Error

```lisp
(= dist2 A B 9)    -- ❌ Wrong!
```

**Fix:** Use correct syntax:
```lisp
(= (dist2 A B) 9)  -- ✓ Correct
```

#### Point Not Defined

```lisp
(= (dist2 A B) 9)  -- ❌ If A or B not defined
```

**Fix:** Define points first:
```lisp
:point A 0 0
:point B 3 0
(= (dist2 A B) 9)  -- ✓ Works now
```

#### Proof Failed

```lisp
:point A 0 0
:point B 3 0
(= (dist2 A B) 100)   -- ❌ False!
```

Output:
```
RESULT: FALSE (LHS /= RHS (Normal Form: -91))
```

**Meaning:** The statement is not provable (it's false).

### Getting Help

- `:help` - Show all commands
- `:list` - Show current assumptions and lemmas
- `:history` - See command history
- `:verbose` - Enable detailed proof output

---

## Practice Exercises

### Exercise 1: Triangle

Prove that a triangle with sides 3-4-5 is a right triangle.

**Hint:** Show that one angle is 90° using perpendicularity.

<details>
<summary>Solution</summary>

```lisp
:point A 0 0
:point B 3 0
:point C 0 4
(= (perpendicular A B A C) 0)   -- Right angle at A
(= (dist2 A B) 9)               -- Side AB = 3
(= (dist2 A C) 16)              -- Side AC = 4
(= (dist2 B C) 25)              -- Hypotenuse BC = 5
```
</details>

### Exercise 2: Midpoint

Prove that the midpoint of a line segment is equidistant from both endpoints.

**Hint:** Use `:assume` with `(midpoint A B M)`.

<details>
<summary>Solution</summary>

```lisp
:point A 0 0
:point B 6 0
:point M 3 0
:assume (= (midpoint A B M) 0)
(= (dist2 A M) (dist2 M B))
```
</details>

### Exercise 3: Square

Define a square with side length 2 and prove its properties.

<details>
<summary>Solution</summary>

```lisp
-- Define square ABCD
:point A 0 0
:point B 2 0
:point C 2 2
:point D 0 2

-- Prove sides are equal
(= (dist2 A B) (dist2 B C))
(= (dist2 B C) (dist2 C D))

-- Prove sides are perpendicular
(= (perpendicular A B B C) 0)

-- Prove opposite sides are parallel
(= (parallel A B D C) 0)
```
</details>

---

## Next Steps

1. **Read the Language Reference** (`docs/LANGUAGE.md`) for complete details
2. **Check the Grammar** (`docs/GRAMMAR.bnf`) for formal syntax
3. **Explore Examples** in `examples/` and `test_*.euclid` files
4. **Build Lemma Libraries** for your favorite theorems
5. **Contribute** by proving classic geometric theorems!

---

## Quick Reference

### Essential Commands
```lisp
:point A x y         -- Define 2D point
:assume (= expr 0)   -- Add assumption
(= expr1 expr2)      -- Prove equality
:lemma (= expr 0)    -- Prove and save
:verbose             -- Toggle details
:save-lemmas file    -- Save lemmas
:load-lemmas file    -- Load lemmas
:list                -- Show state
:help                -- Show help
:q                   -- Quit
```

### Geometric Primitives
```lisp
(dist2 A B)              -- Distance²
(perpendicular A B C D)  -- AB ⊥ CD
(parallel A B C D)       -- AB ∥ CD
(collinear A B C)        -- Collinear points
(midpoint A B M)         -- Midpoint
(dot A B C D)            -- Dot product
(circle P C r)           -- Point on circle
```

---

**Happy Proving! 📐**

For questions or issues, see the [Language Reference](LANGUAGE.md).
