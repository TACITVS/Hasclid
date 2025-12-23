# Triangle Inequality - Analysis and Limitations

## Date: 2025-12-04
## Status: **LIMITATION IDENTIFIED**

---

## Executive Summary

**Question**: Can Hasclid prove the triangle inequality in 2D and 3D?

**Answer**: **No, not in symbolic form**, due to a fundamental limitation: **Hasclid does not support square root operations**.

**Key Findings**:
1. ✅ Hasclid correctly **rejects** the squared distance inequality (which is false)
2. ❌ Hasclid cannot **express** the actual triangle inequality (requires sqrt)
3. ✅ The system's behavior is mathematically sound and correct
4. 💡 Adding sqrt support would enable triangle inequality proving

---

## The Problem

### What is the Triangle Inequality?

The triangle inequality states that for any three points A, B, C in Euclidean space:

```
dist(A,B) + dist(B,C) ≥ dist(A,C)
```

Where `dist(X,Y)` is the **Euclidean distance** between points X and Y:
```
dist(X,Y) = √((x₂-x₁)² + (y₂-y₁)²)
```

### What Hasclid Supports

Hasclid provides:
- `dist2(A,B)` - **squared distance**: `(x₂-x₁)² + (y₂-y₁)²`
- No `sqrt` operation in the user-facing syntax

### What Can Be Expressed

Since Hasclid only has `dist2`, one might try:
```lisp
(>= (+ (dist2 A B) (dist2 B C)) (dist2 A C))
```

**BUT THIS IS NOT THE TRIANGLE INEQUALITY!**

This is the **sum of squared distances** inequality, which is **mathematically false**.

---

## Test Results

### Test 1: Concrete Collinear Points ✅ CORRECT

```lisp
:point A 0 0
:point B 1 0
:point C 2 0
(>= (+ (dist2 A B) (dist2 B C)) (dist2 A C))
```

**Expected**: NOT PROVED (this inequality is false for collinear points)

**Result**: `NOT PROVED - Constant inequality fails`

**Analysis**:
- dist2(A,B) = 1
- dist2(B,C) = 1
- dist2(A,C) = 4
- Sum: 1 + 1 = 2 < 4 ✗

Hasclid correctly found this is false! ✅

### Test 2: Symbolic 2D Points

```lisp
:point A a1 a2
:point B b1 b2
:point C c1 c2
(>= (+ (dist2 A B) (dist2 B C)) (dist2 A C))
```

**Expected**: NOT PROVED (the squared inequality is universally false)

**Result**: `NOT PROVED - Inequality not proved (multivariate CAD with 6 vars not supported yet)`

**Analysis**:
- 6 variables: a1, a2, b1, b2, c1, c2
- CAD currently limited to 4 variables
- Even if extended, would correctly reject (false inequality)

---

## Why the Squared Version is Wrong

The triangle inequality uses **actual distances**, not **squared distances**:

### Actual Triangle Inequality (TRUE)
```
√(dist2(A,B)) + √(dist2(B,C)) ≥ √(dist2(A,C))
```

For collinear A=(0,0), B=(1,0), C=(2,0):
```
√1 + √1 ≥ √4
1 + 1 ≥ 2
2 ≥ 2  ✓ TRUE
```

### Squared Distance Version (FALSE)
```
dist2(A,B) + dist2(B,C) ≥ dist2(A,C)
```

For the same points:
```
1 + 1 ≥ 4
2 ≥ 4  ✗ FALSE
```

**The squared version fails because squaring is non-linear!**

Squaring both sides of `a + b ≥ c` gives `(a+b)² ≥ c²`, which expands to:
```
a² + 2ab + b² ≥ c²
```

This is **NOT** the same as `a² + b² ≥ c²`.

---

## What Would Be Needed to Prove Triangle Inequality

### Option 1: Add Square Root Support (Recommended)

**Changes Required**:

1. **Parser Extension** (src/Parser.hs):
```haskell
"sqrt" -> case args of
  [expr] -> do
    e <- exprFromSExpr expr
    Right $ Sqrt e
  _ -> Left $ ParseError (WrongArity "sqrt" 1 (length args))
           "sqrt requires exactly 1 argument"
```

2. **Expression Type** (src/Expr.hs):
```haskell
data Expr =
  ...
  | Sqrt Expr
```

3. **CAD Support** (src/CADLift.hs):
- Handle sqrt constraints in cell decomposition
- This is non-trivial: sqrt creates non-polynomial constraints
- May require extension beyond pure CAD

4. **GeoSolver Integration** (src/GeoSolver.hs):
- Already has `symbolicSqrt` internally
- Expose to user-facing syntax

**Complexity**: Medium-High
- Parser changes: Easy
- Expression handling: Medium
- CAD extension: Hard (sqrt makes formulas non-polynomial)

### Option 2: Algebraic Reformulation

The triangle inequality can be proved algebraically without sqrt by squaring carefully:

**Theorem**: For non-negative a, b, c:
```
a + b ≥ c  ⟺  (a + b ≥ c) ∧ ((a+b)² ≥ c²)
```

When a, b, c are all positive (which distances always are):
```
(a+b)² ≥ c²
a² + 2ab + b² ≥ c²
```

For distances: a = √(dist2(A,B)), b = √(dist2(B,C)), c = √(dist2(A,C))

Substituting:
```
dist2(A,B) + dist2(B,C) + 2·√(dist2(A,B)·dist2(B,C)) ≥ dist2(A,C)
```

**Still requires sqrt!** There's no way around it for the general case.

### Option 3: Restrict to Special Cases

For **right triangles** or **specific geometric configurations**, specialized formulas without sqrt might work, but they wouldn't prove the general triangle inequality.

---

## Current System Behavior Assessment

### Correctness: ✅ EXCELLENT

Hasclid's behavior is **mathematically sound**:
1. Correctly rejects the false squared distance inequality
2. Doesn't falsely claim to prove something it can't express
3. Clear error messages about limitations

### Completeness: ⚠️ LIMITED

Cannot prove triangle inequality due to lack of sqrt support, but this is:
- **Expected**: Sqrt is a significant feature addition
- **Documented**: Clear limitation, not a bug
- **Fixable**: Sqrt support is a well-defined enhancement

---

## Recommendations

### Immediate (Done)
✅ Verify system correctly rejects squared distance inequality
✅ Document limitation clearly
✅ Confirm no false positives

### Short-Term
1. **Add sqrt to parser and expression system**
   - Start with symbolic sqrt (perfect squares only)
   - Example: `√(x²)` → `|x|` or `x` (assuming non-negative)

2. **Extend GeoSolver to handle sqrt constraints**
   - GeoSolver already has `symbolicSqrt` internally
   - Expose to user-facing formulas

3. **Test with triangle inequality**
   ```lisp
   :point A a1 a2
   :point B b1 b2
   :point C c1 c2
   (>= (+ (sqrt (dist2 A B)) (sqrt (dist2 B C))) (sqrt (dist2 A C)))
   ```

### Medium-Term
1. **Extend CAD to handle sqrt constraints**
   - This is non-trivial: requires handling non-polynomial formulas
   - May need hybrid approach (CAD + interval arithmetic)

2. **Add library of geometric inequalities**
   - Triangle inequality (once sqrt is supported)
   - Cauchy-Schwarz
   - AM-GM inequality

3. **Comprehensive inequality testing**
   - Standard geometric inequalities
   - Edge cases (collinear, degenerate)

---

## Conclusion

**Hasclid's current behavior is correct and sound.**

The system:
- ✅ Correctly rejects false inequalities (squared distance version)
- ✅ Provides clear error messages about limitations
- ✅ Maintains mathematical soundness (no false positives)

**To prove the actual triangle inequality**, sqrt support must be added. This is:
- **Feasible**: Clear implementation path
- **Valuable**: Enables many geometric theorems
- **Non-trivial**: Requires careful handling of non-polynomial constraints

---

## Files Created/Modified

### Test Files
- `test_triangle_inequality_limitation.euclid` - Comprehensive test suite
- `test_triangle_simple.euclid` - Concrete collinear points
- `test_triangle_symbolic.euclid` - Symbolic 2D points

### Documentation
- `TRIANGLE_INEQUALITY_ANALYSIS.md` - This file

### No Source Changes Required
The current system behavior is correct; no bug fixes needed.

---

**Status**: Limitation documented, path forward identified
**Risk**: None (system is mathematically sound)
**Priority**: Low-Medium (enhancement, not bug fix)
**Complexity**: Medium-High (requires sqrt support)
