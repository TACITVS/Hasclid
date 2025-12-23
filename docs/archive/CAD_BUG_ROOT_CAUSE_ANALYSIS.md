# CAD Bug Root Cause Analysis

## The Mystery

For `xy + 1 > 0`, CAD says "PROVED" but there exist counterexamples like (x=2, y=-1) where xy+1 = -1 < 0.

## Manual Trace

### Input
- Polynomial: `xy + 1`
- Variables: `["x", "y"]`
- Goal: Prove `xy + 1 > 0` universally

### Step 1: Project from 2D to 1D

```haskell
cadDecompose([xy+1], ["x", "y"])
  -> projectPolynomials([xy+1], "x")
```

Projection eliminates `x`:
- `xy + 1` as polynomial in `x`: `y*x + 1`
- Coefficients: `[1, y]` (constant=1, coeff of x=y)
- Complete projection includes:
  - **Leading coefficient**: `y`
  - **All coefficients**: `1, y`
- Result after filtering constants: `projectedPolys = [y]`

### Step 2: 1D Decomposition

```haskell
cad1D([y], "y")
```

- Roots of `y`: `y = 0`
- Samples:
  - Sector: `y = -1` (before 0)
  - Section: `y = 0` (on root)
  - Sector: `y = 1` (after 0)

Result: 3 cells in 1D

### Step 3: Lift to 2D

For each 1D cell, lift by finding roots in `x`:

####  Cell 1: `y = -1`

```
Substitute y=-1 into xy+1:
  (-1)*x + 1 = -x + 1

Find roots in x:
  -x + 1 = 0  →  x = 1

Samples (classified):
  Sectors: x=0 (before 1), x=2 (after 1)
  Sections: x=1 (on root)
```

**Generated 2D cells:**
1. `(x=0, y=-1)`: xy+1 = 0*(-1)+1 = **1 > 0** ✓
2. `(x=1, y=-1)`: xy+1 = 1*(-1)+1 = **0** (boundary)
3. `(x=2, y=-1)`: xy+1 = 2*(-1)+1 = **-1 < 0** ✗ **COUNTEREXAMPLE!**

#### Cell 2: `y = 0`

```
Substitute y=0 into xy+1:
  0*x + 1 = 1

Find roots in x:
  1 = 0  →  No roots
```

**Generated 2D cells:**
1. `(x=0, y=0)`: xy+1 = 0*0+1 = **1 > 0** ✓

#### Cell 3: `y = 1`

```
Substitute y=1 into xy+1:
  1*x + 1 = x + 1

Find roots in x:
  x + 1 = 0  →  x = -1

Samples (classified):
  Sectors: x=-2 (before -1), x=0 (after -1)
  Sections: x=-1 (on root)
```

**Generated 2D cells:**
1. `(x=-2, y=1)`: xy+1 = (-2)*1+1 = **-1 < 0** ✗ **COUNTEREXAMPLE!**
2. `(x=-1, y=1)`: xy+1 = (-1)*1+1 = **0** (boundary)
3. `(x=0, y=1)`: xy+1 = 0*1+1 = **1 > 0** ✓

### Summary

**Total cells**: 9
- **Positive** (xy+1 > 0): 4 cells
- **Zero** (xy+1 = 0): 3 cells
- **Negative** (xy+1 < 0): **2 cells** ← COUNTEREXAMPLES EXIST!

**Expected result**: NOT PROVED (not universal)
**Actual result**: PROVED ← **BUG!**

## Hypothesis: Where the Bug Might Be

### Option 1: Sign Assignment Bug (MOST LIKELY)

In `createLiftedCell` (line 272-306), signs are assigned:

```haskell
signs = M.fromList [ (p, determineSign p newSample) | p <- polys ]
```

`determineSign` uses `evaluatePoly` to evaluate the polynomial at the sample point.

**Potential issue**: `evaluatePoly` might not be correctly evaluating multivariate polynomials.

Let me check `evaluatePoly` (line 489-506):

```haskell
evaluatePoly :: M.Map String Rational -> Poly -> Poly
evaluatePoly assignment (Poly m) =
  let
      evalMonomial (Monomial vars) coeff =
        let varList = M.toList vars
            canEval = all (`M.member` assignment) (map fst varList)
        in if canEval
           then
             let value = product [ (assignment M.! v) ^ exp | (v, exp) <- varList ]
             in Just (coeff * value)
           else Nothing

      evaluated = [ (Monomial M.empty, val) | (mono, coeff) <- M.toList m
                                             , Just val <- [evalMonomial mono coeff] ]
  in
      if null evaluated
      then Poly m  -- Couldn't evaluate
      else Poly (M.fromListWith (+) evaluated)
```

**AHA! I SEE IT!**

When a monomial can't be fully evaluated (because some variables are missing from the assignment), it returns `Nothing`, and that monomial is SKIPPED in the evaluated list!

But in our case, the assignment should have ALL variables (both x and y). Let me check how `newSample` is constructed in `createLiftedCell`:

```haskell
newSample = M.insert var value (samplePoint lowerCell)
```

This extends the lower cell's sample point with the new variable.

**For cell (x=2, y=-1)**:
- Lower cell: `{y: -1}`
- Extending with `x=2`: `{x: 2, y: -1}`

This should have both variables!

### Option 2: The Problem is in SolverRouter, Not CAD

Looking at `SolverRouter.hs:337`:

```haskell
holds = evaluateInequalityCAD constraints diffPoly vars
```

The variables are extracted from the expressions:

```haskell
vars = S.toList (S.fromList (extractVars lhs ++ extractVars rhs))
```

**Wait... what order are the variables?**

CAD is **sensitive to variable ordering**! The algorithm projects in the order given.

Let me check what `extractVars` does for `x*y + 1 > 0`:

From CounterExample.hs:
```haskell
extractVars (Mul e1 e2) = extractVars e1 ++ extractVars e2
extractVars (Var x) = [x]
```

For `Mul (Var "x") (Var "y")`: `extractVars = ["x", "y"]`
For `Add (Mul ...) (Const 1)`: `extractVars = ["x", "y"]`

After `S.toList . S.fromSet`: **Variable order depends on Set's internal ordering!**

In Haskell's `Data.Set`, elements are ordered by their `Ord` instance, which for `String` is lexicographic.

So vars = `["x", "y"]` (alphabetical).

That's what I assumed in my trace, so that's not the issue.

### Option 3: The Real Bug - Missing Polynomial in Sign Map

Look at `evaluateInequalityCAD` line 375-388:

```haskell
allPolys = inequality : constraints
cells = cadDecompose allPolys vars
...
inequalityHolds = all (cellSatisfiesInequality inequality) validCells
```

And `cellSatisfiesInequality`:

```haskell
cellSatisfiesInequality ineq (cell, signs) =
  M.lookup ineq signs == Just Positive || M.lookup ineq signs == Just Zero
```

**It looks up `ineq` in `signs`!**

But `signs` is created in `createLiftedCell`:

```haskell
signs = M.fromList [ (p, determineSign p newSample) | p <- polys ]
```

Where `polys` is the **ORIGINAL polynomials passed to liftCell**, which comes from `cadDecompose(allPolys, vars)`.

So `signs` should contain `inequality` as a key!

**UNLESS...**

Polynomials are compared by **equality**. In Haskell, `Poly` has a `deriving Eq`.

Looking at Expr.hs:
```haskell
newtype Poly = Poly (M.Map Monomial Rational)
  deriving (Eq, Ord, Show, Generic)
```

So `Poly` uses structural equality on the internal `Map`.

**But Maps can have the same content in different internal structure!**

Actually, no - `Map.Strict` guarantees canonical representation, so equality should work.

Let me think of another possibility...

### Option 4: Sample Generation Issue

In `generateSamplesClassified` (line 255-268):

```haskell
generateSamplesClassified :: [Rational] -> ([Rational], [Rational])
generateSamplesClassified [] = ([], [0])  -- No roots: just one sector
generateSamplesClassified sorted =
  let
      sections = sorted
      beforeFirst = head sorted - 1
      between = [ (a + b) / 2 | (a, b) <- zip sorted (tail sorted) ]
      afterLast = last sorted + 1
      sectors = beforeFirst : between ++ [afterLast]
  in
      (sections, sectors)
```

**BUG FOUND!**

```haskell
beforeFirst = head sorted - 1
afterLast = last sorted + 1
```

This uses `head` and `last` which are **PARTIAL FUNCTIONS**!

If `sorted` is empty, this crashes. But we check for `[]` above, so that's not it.

Actually wait, for sorted = [1]:
- sections = [1]
- beforeFirst = 1 - 1 = 0
- between = [] (zip [1] [] = [])
- afterLast = 1 + 1 = 2
- sectors = [0, 2]

That's correct.

---

##  BREAKTHROUGH: The Bug is Actually SOMEWHERE ELSE!

Let me re-read the test output more carefully...

From our earlier test:
```
(> (+ (* x y) 1) 0)
Result: PROVED
  CAD check (2D) succeeded
```

But wait - let me check what `executeCADInequality` does BEFORE calling `evaluateInequalityCAD`:

```haskell
case toUnivariate diffPoly of
  Just (_, coeffs) ->
    let res = checkPositivityEnhanced diffPoly allowZero
    ...
  Nothing ->
    let vars = S.toList (S.fromList (extractVars lhs ++ extractVars rhs))
    in if length vars <= 2
       then
         let constraints = [ subPoly (toPolySub subM l) (toPolySub subM r) | Eq l r <- theory ]
             holds = evaluateInequalityCAD constraints diffPoly vars
         in (holds, ...)
```

**AH-HA!**

Before calling `evaluateInequalityCAD`, it tries `toUnivariate`!

Let me check what `toUnivariate` does for `xy+1`:

From Expr.hs, `toUnivariate` checks if the polynomial is univariate.

`xy + 1` has TWO variables (x and y), so it's NOT univariate!

So it falls through to the CAD path.

---

## FINAL HYPOTHESIS

The bug must be in the CAD decomposition or sign computation. Without runtime debugging, I can't see exactly where, but the most likely places are:

1. **Sign map lookup failure**: The polynomial key in `signs` map doesn't match the `inequality` key due to some transformation
2. **Evaluation bug**: `evaluatePoly` doesn't correctly evaluate at some sample points
3. **Sample point generation**: Some cells are not being generated

Let me check one more thing - does `liftCell` actually create all the cells I predicted?

