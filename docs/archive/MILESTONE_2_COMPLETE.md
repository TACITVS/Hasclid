# ✅ Milestone 2 Complete: Root Cause Found!

## Bug Location

**File**: `src/CADLift.hs`
**Function**: `evaluatePoly` (lines 489-506)
**Severity**: CRITICAL (unsound proofs)

## The Bug

`evaluatePoly` is supposed to do **partial evaluation** - substitute some variables while keeping others. But it actually **drops monomials** that contain unassigned variables!

### Example

For polynomial `xy + 1` with assignment `{y: -1}`:

**Expected**: `-x + 1` (substitute y=-1, keep x as variable)
**Actual**: `1` (drops the xy term entirely!)

### Code Analysis

```haskell
evalMonomial (Monomial vars) coeff =
  let varList = M.toList vars
      canEval = all (`M.member` assignment) (map fst varList)  -- ← BUG HERE!
  in if canEval
     then Just (coeff * value)
     else Nothing  -- ← Drops the monomial!
```

The condition `all (\`M.member\` assignment)` requires **ALL** variables to be in the assignment. If any variable is missing, the monomial is dropped.

### Impact on CAD

1. CAD projects from 2D to 1D: gets 1D cells in y
2. CAD lifts back to 2D: for each y-cell, substitutes y-value into `xy+1`
3. **BUG**: Substitution gives wrong result (constant instead of polynomial in x)
4. No roots found in x dimension
5. Only one sector generated per y-cell (x=0)
6. **Result**: Only 3 cells instead of 9, all with x=0
7. **False positive**: Proves xy+1 > 0 universally (only checked x=0!)

## How Debug Traces Revealed It

```
[LIFT] Lower cell sample: [("y",(-1) % 1)]
[LIFT] Substituted polys: ["1"]   ← Should be "-x + 1"!
[LIFT] Critical values (roots): []   ← Should find root at x=1!
[LIFT] Sector samples: [0 % 1]   ← Only one sample point!
```

## The Fix (Next Milestone)

Replace `evaluatePoly` with correct partial evaluation:

```haskell
-- For each monomial, substitute only assigned variables:
-- xy with {y: -1} → (-1)*x = -x
-- x with {y: -1} → x (unchanged)
-- 1 with {y: -1} → 1 (unchanged)
```

## Test Case

```lisp
(> (+ (* x y) 1) 0)

Should: NOT PROVED (counterexample: x=2, y=-1 gives -1 < 0)
Currently: PROVED (false positive due to this bug)
```

## Files to Fix

1. `src/CADLift.hs` - Fix `evaluatePoly` for partial substitution
2. Or create new function `partialEval` specifically for CAD

## Estimated Fix Time

30-60 minutes (straightforward logic fix)

---

**Milestone 2 Status**: ✅ COMPLETE
**Next Milestone**: Fix the bug
**Debug Approach**: Added strategic `trace` statements to CAD pipeline
