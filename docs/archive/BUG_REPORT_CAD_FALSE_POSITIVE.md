# 🐛 CAD False Positive Bug Report

## Date: 2025-12-03
## Severity: **CRITICAL** (Unsound proof)

---

## Summary

The CAD solver incorrectly proves `xy + 1 > 0` as universally true, when it is actually false for certain values (e.g., x=-1, y=2 gives -1 < 0).

**This is a soundness bug**: The prover claims theorems are true when they are NOT.

---

## Reproduction

### Test Case 1: False Positive
```lisp
(> (+ (* x y) 1) 0)
```

**Expected Result**: NOT PROVED (or at least unknown)
**Actual Result**: PROVED ✗
**Reason Given**: "CAD check (2D) succeeded"

### Test Case 2: Counterexample (Works Correctly)
```lisp
:assume (= x -1)
:assume (= y 2)
(> (+ (* x y) 1) 0)
```

**Expected Result**: NOT PROVED
**Actual Result**: NOT PROVED ✓
**Reason Given**: "Constant inequality fails"

**Evaluation**: (-1)(2) + 1 = -2 + 1 = -1, and -1 > 0 is FALSE.

---

## Analysis

### What Works
- When given **concrete values** that form a counterexample, CAD correctly evaluates and rejects
- Univariate inequalities work correctly (using Sturm's theorem)
- Simple inequalities like `x² ≥ 0` work correctly

### What Fails
- **Multivariate inequalities without constraints** are sometimes incorrectly proved
- The bug appears specifically in `evaluateInequalityCAD` (CADLift.hs:375-388)

### Root Cause Hypothesis

Looking at `evaluateInequalityCAD`:
```haskell
evaluateInequalityCAD :: [Poly] -> Poly -> [String] -> Bool
evaluateInequalityCAD constraints inequality vars =
  let
      -- Decompose with both constraints and inequality
      allPolys = inequality : constraints
      cells = cadDecompose allPolys vars

      -- Check if inequality holds in all cells satisfying constraints
      validCells = filter (cellSatisfiesConstraints constraints) cells
      inequalityHolds = all (cellSatisfiesInequality inequality) validCells
  in
      inequalityHolds
```

**Problem 1**: When `constraints = []` (no assumptions), ALL cells should be valid. But `cellSatisfiesConstraints` checks if constraints are zero:
```haskell
cellSatisfiesConstraints :: [Poly] -> (CADCell, SignAssignment) -> Bool
cellSatisfiesConstraints constraints (cell, signs) =
  all (\p -> M.lookup p signs == Just Zero) constraints
```

When `constraints = []`, the `all` returns `True` (vacuous truth), so `validCells = cells`. This is CORRECT.

**Problem 2**: `cellSatisfiesInequality` checks:
```haskell
cellSatisfiesInequality :: Poly -> (CADCell, SignAssignment) -> Bool
cellSatisfiesInequality ineq (cell, signs) =
  M.lookup ineq signs == Just Positive || M.lookup ineq signs == Just Zero
```

This checks if the inequality polynomial is **non-negative** in the cell.

**The bug must be in the CAD decomposition or sign assignment logic.**

---

## Impact

### High Severity
- **Unsound proofs**: The system proves false theorems
- **Trust破坏**: Users cannot rely on "PROVED" results for inequalities
- **Silent failure**: No warning that the result might be wrong

### Affected Features
- All multivariate inequalities without explicit constraints
- CAD-based inequality proving
- Automatic solver when routing to CAD

---

## Workaround

Until fixed, users should:
1. **Manually verify** inequality proofs by testing counterexamples
2. **Avoid trusting** CAD-based inequality proofs without verification
3. **Use explicit constraints** when possible (seems to work better)

---

## Test Results Summary

From `test_cad_correctness.euclid`:

| Test | Formula | Expected | Actual | Status |
|------|---------|----------|--------|--------|
| 1.1 | xy+1 > 0 | NOT PROVED | PROVED | ❌ BUG |
| 1.2 | xy+1 > 0 (x=-1,y=2) | NOT PROVED | NOT PROVED | ✅ OK |
| 2.1 | x²+y² ≥ 0 | PROVED | PROVED | ✅ OK |
| 2.2 | x² ≥ 0 | PROVED | PROVED | ✅ OK |
| 2.4 | Triangle ineq | PROVED | PROVED | ✅ OK |
| 3.1 | x > 0 | NOT PROVED | NOT PROVED | ✅ OK |
| 3.2 | x-1 > 0 | NOT PROVED | NOT PROVED | ✅ OK |
| 3.3 | xy > 1 | NOT PROVED | NOT PROVED | ✅ OK |
| 5.2 | x²-y² ≥ 0 | NOT PROVED | NOT PROVED | ✅ OK |

**Failure Rate**: 1/9 explicit failures (11%), but this is a CRITICAL failure.

---

## Next Steps

1. ✅ **Document the bug** (this file)
2. ⏭️ **Investigate CAD decomposition** for xy+1
3. ⏭️ **Fix sign assignment logic**
4. ⏭️ **Add regression tests**
5. ⏭️ **Add warning messages** for CAD results

---

## Files Involved

- `src/CADLift.hs` (lines 375-396) - evaluateInequalityCAD
- `src/CADLift.hs` (lines 65-81) - cadDecompose
- `src/CADLift.hs` (lines 108-133) - liftCell
- `src/CADLift.hs` (lines 274-306) - createLiftedCell
- `src/SolverRouter.hs` (lines 307-339) - executeCADInequality (caller)

---

**Report by**: Automated analysis
**Verified**: Manual testing
**Priority**: P0 (Critical soundness issue)
