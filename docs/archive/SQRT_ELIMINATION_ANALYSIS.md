# Square Root Elimination Analysis
## Critical Bug Found in SqrtElim.hs

**Date**: 2025-12-09
**Issue**: Duplicate auxiliary variables for identical sqrt expressions
**Severity**: HIGH - Prevents proving theorems with square roots

---

## The Problem

### Current Implementation (SqrtElim.hs:156-172)

```haskell
elimExpr (Sqrt e) = do
  e' <- elimExpr e
  case e of
    Pow t 2 -> do
      t' <- elimExpr t
      addConstraint (Ge t' (Const 0))
      return t'
    _ -> do
      v <- freshVar  -- ← BUG: Creates NEW variable every time!
      let vExpr = Var v
          eqConstraint = Eq (Pow vExpr 2) e'
          geConstraint = Ge vExpr (Const 0)
          radicandConstraint = Ge e' (Const 0)
      addConstraint eqConstraint
      addConstraint geConstraint
      addConstraint radicandConstraint
      return vExpr
```

### What Happens

When processing `sqrt(d2) * sqrt(d2)`:

1. First `sqrt(d2)`:
   - Creates `sqrt_aux1`
   - Adds constraints: `sqrt_aux1^2 = d2`, `sqrt_aux1 >= 0`, `d2 >= 0`

2. Second `sqrt(d2)`:
   - Creates `sqrt_aux2` (NEW variable for SAME expression!)
   - Adds constraints: `sqrt_aux2^2 = d2`, `sqrt_aux2 >= 0`, `d2 >= 0`

3. Goal becomes: `sqrt_aux1 * sqrt_aux2 = d2`

4. **Problem**: No constraint links `sqrt_aux1` and `sqrt_aux2`!
   - Solver would need to derive: `sqrt_aux1^2 = sqrt_aux2^2 ∧ both positive ⟹ sqrt_aux1 = sqrt_aux2`
   - This requires sophisticated reasoning beyond current solvers

---

## Test Results

### ✅ WORKS: Concrete sqrt products
```lisp
:assume (= a 4)
:assume (= b 9)
(= (* (sqrt a) (sqrt b)) 6)  -- PROVED
```
**Why**: Geometric propagation evaluates concrete values

### ❌ FAILS: Symbolic sqrt identity
```lisp
:assume (= d2 (+ (^ (- x2 x1) 2) (^ (- y2 y1) 2)))
(= (* (sqrt d2) (sqrt d2)) d2)  -- NOT PROVED
```
**Why**: Creates two different auxiliary variables for the same `sqrt(d2)`

### ❌ KILLED: Ptolemy with sqrt
```lisp
(= (* (sqrt dAC2) (sqrt dBD2))
   (+ (* (sqrt dAB2) (sqrt dCD2)) (* (sqrt dBC2) (sqrt dDA2))))
```
**Why**: Creates 6+ auxiliary variables, exploding the problem space

---

## Impact on Ptolemy's Theorem

### With Sqrt Elimination (FAILED)
- 8 base variables: xA, yA, xB, yB, xC, yC, xD, yD
- 6 distance squared variables: dAB2, dBC2, dCD2, dDA2, dAC2, dBD2
- 6 sqrt auxiliary variables: sqrt_aux1 through sqrt_aux6
- 18 additional constraints (3 per sqrt)
- **Total**: 20+ variables, 28+ constraints
- **Result**: Gröbner basis computation KILLED (too large)

### With Double-Squaring (PROVED)
- 16 variables total
- 12 constraints
- Degree 4 polynomial
- **Result**: PROVED in ~2.5 minutes

---

## The Core Issue

**SqrtElim lacks memoization**. It should:

1. ✅ Check if we've seen this exact sqrt expression before
2. ✅ Reuse the same auxiliary variable for identical expressions
3. ✅ Only create new variables for genuinely new sqrt terms

### Current Behavior
```
sqrt(d2) → sqrt_aux1 (with constraints)
sqrt(d2) → sqrt_aux2 (NEW! But d2 hasn't changed!)
```

### Desired Behavior
```
sqrt(d2) → sqrt_aux1 (with constraints)
sqrt(d2) → sqrt_aux1 (REUSE! Same expression)
```

---

## Why This Matters

From user critique:
> "It was a clever trick to square in order to avoid the shortcoming of the square root, but what kind of program calls itself a genetic theorem prover that doesn't handle square roots ?"

**Current status**:
- ✅ Sqrt is in the AST (Expr.hs)
- ✅ SqrtElim module exists
- ✅ Le/Lt operators added (2025-12-09)
- ✅ Simple concrete cases work
- ❌ Symbolic sqrt creates duplicate auxiliary variables
- ❌ Cannot prove theorems with multiple sqrt occurrences

---

## Proposed Fix

### Implementation Strategy

1. **Add memoization to ElimM State**:
```haskell
type SqrtMemo = Map Expr String
type ElimM = State ([Formula], SqrtMemo)

elimExpr (Sqrt e) = do
  e' <- elimExpr e
  case e of
    Pow t 2 -> ...
    _ -> do
      (constraints, memo) <- get
      case Map.lookup e' memo of
        Just varName -> return (Var varName)  -- REUSE!
        Nothing -> do
          v <- freshVar
          let vExpr = Var v
          -- ... add constraints ...
          modify (\(cs, m) -> (cs, Map.insert e' v m))
          return vExpr
```

2. **Update freshVar to use counter**:
```haskell
freshVar :: ElimM String
freshVar = do
  (constraints, memo) <- get
  let idx = Map.size memo + 1
  return ("sqrt_aux" ++ show idx)
```

3. **Update addConstraint**:
```haskell
addConstraint :: Formula -> ElimM ()
addConstraint f = modify (\(cs, m) -> (f:cs, m))
```

---

## Alternative Approaches

### Option 1: Smart Sqrt Simplification (Before Elimination)
Recognize patterns like:
- `sqrt(a) * sqrt(a)` → `a` (with `a >= 0`)
- `sqrt(a) * sqrt(b)` → `sqrt(a*b)` (with `a,b >= 0`)
- `sqrt(a^2)` → `a` (with `a >= 0`)

### Option 2: Native Sqrt in Gröbner Basis
Extend polynomial algebra to handle sqrt as a special algebraic extension.

### Option 3: Hybrid Approach
- Use double-squaring for equality goals
- Use sqrt elimination for inequality goals
- Automatically choose based on goal type

---

## Workarounds (Current)

### For Equality Theorems
Use algebraic transformation (double-squaring):
```lisp
-- Instead of: pq = ac + bd
-- Square: p²q² = a²c² + 2ac·bd + b²d²
-- Rearrange: p²q² - a²c² - b²d² = 2ac·bd
-- Square again: (p²q² - a²c² - b²d²)² = 4a²b²c²d²
```

### For Concrete Cases
Let geometric propagation handle numerical evaluation.

---

## Recommendations

### Immediate (High Priority)
1. Fix SqrtElim memoization bug
2. Add tests for sqrt identity: `sqrt(x) * sqrt(x) = x`
3. Retry Ptolemy theorem with fixed sqrt elimination

### Short Term
1. Implement smart sqrt simplification (pre-elimination)
2. Add sqrt product rule: `sqrt(a) * sqrt(b) → sqrt(a*b)`
3. Optimize auxiliary variable generation

### Long Term
1. Consider native sqrt support in Gröbner basis
2. Benchmark sqrt elimination vs double-squaring
3. Develop automatic transformation selection

---

## Conclusion

The sqrt elimination module has a **critical bug** that prevents it from handling theorems with multiple sqrt occurrences. While the double-squaring workaround successfully proves Ptolemy's theorem, fixing the sqrt elimination bug is essential for HASCLID to legitimately claim "generic theorem proving" capabilities with square roots.

**Status**: Bug identified, fix designed, implementation pending

---

*Analysis completed: 2025-12-09*
*Next step: Implement memoization fix in SqrtElim.hs*
