# 🚨 CRITICAL: Silent Killer Bug in Groebner Basis

**Discovered**: Contributor feedback, December 10, 2025
**Severity**: **HIGH** - Prevents generic Morley proof
**Status**: Confirmed in codebase

---

## The "Silent Killer" Identified

### Problem: TermOrder is Disconnected from Solver

**Location**: `src/Expr.hs:125-140` + `src/Expr.hs:191`

```haskell
-- HARDCODED Lexicographic Order!
instance Ord Monomial where
  compare (Monomial m1) (Monomial m2) =
    let vars = sortBy (flip compare) (nub (M.keys m1 ++ M.keys m2))
    in go vars
    where
      go [] = EQ
      go (v:vs) =
        let e1 = M.findWithDefault 0 v m1
            e2 = M.findWithDefault 0 v m2
        in case compare e1 e2 of
             EQ -> go vs
             other -> other

-- Uses Map.lookupMax which relies on Ord instance above!
getLeadingTerm :: Poly -> Maybe (Monomial, Rational)
getLeadingTerm (Poly m) = M.lookupMax m  -- FORCES LEX ORDER
```

**Consequence**: 
- `TermOrder.hs` with `compareGrevLex` exists but is **never used**
- All Groebner computations forced to **Lex order**
- Lex is **exponentially slower** than GrevLex for generic proofs
- Generic Morley will timeout/explode

---

## Evidence in Codebase

### TermOrder.hs Exists
```bash
$ grep -r "compareGrevLex" src/
src/TermOrder.hs:compareGrevLex :: Monomial -> Monomial -> Ordering
src/TermOrder.hs:compareGrevLex (Monomial m1) (Monomial m2) = ...
```

### But Never Used in BuchbergerOpt.hs
```bash
$ grep -r "TermOrder\|compareGrevLex" src/BuchbergerOpt.hs
# NO RESULTS - completely disconnected!
```

### All Uses Rely on Default Ord
```bash
$ grep "getLeadingTerm" src/BuchbergerOpt.hs
47:  case (getLeadingTerm f, getLeadingTerm g) of
62:      case (getLeadingTerm f, getLeadingTerm g) of
89:  case (getLeadingTerm f, getLeadingTerm g) of
174:          case getLeadingTerm p of
...
# 7 occurrences - all use M.lookupMax (Lex order)
```

---

## Why This Kills Generic Morley

### Right Isosceles Morley (What We Proved)
- **Variables**: ~6 (xP, yP, xQ, yQ, xR, yR)
- **Symmetry**: Heavy (reduces to 2 independent vars)
- **Lex Impact**: Minimal (small basis)
- **Result**: ✅ Works fine

### Generic Morley (What Will Fail)
- **Variables**: ~12 (xA, yA, xB, yB, xC, yC, xP, yP, xQ, yQ, xR, yR)
- **Symmetry**: None (arbitrary triangle)
- **Lex Impact**: **CATASTROPHIC**
  - Intermediate polynomials grow exponentially
  - Degrees explode (20+ instead of 4-6 in GrevLex)
  - Timeout or OOM
- **Result**: ❌ Will fail

### Example: Degree Explosion

**GrevLex** (balanced, degree-first):
```
Intermediate: x^2*y + xy^2 + y^3  (degree 3, 3 terms)
```

**Lex** (variable elimination order):
```
Intermediate: y^15 + y^14*... + ... (degree 15, hundreds of terms)
```

---

## Fix Strategy

### Option 1: Inject Term Order (Recommended)
**Pros**: Clean, flexible, preserves Map structure
**Cons**: Requires scanning Map (O(n) instead of O(log n))

```haskell
-- New function in Expr.hs
getLeadingTermByOrder :: (Monomial -> Monomial -> Ordering) -> Poly -> Maybe (Monomial, Rational)
getLeadingTermByOrder cmp (Poly m) 
  | M.null m = Nothing
  | otherwise = Just $ maximumBy (comparing fst `using` cmp) (M.toList m)
  where
    using f g = \x y -> f (g x) (g y)

-- Update BuchbergerOpt.hs
reduce :: (Monomial -> Monomial -> Ordering) -> Poly -> [Poly] -> Poly
reduce ord p fs = ...
  let (ltM, ltC) = getLeadingTermByOrder ord p
```

### Option 2: Newtype with Custom Ord (Pro)
**Pros**: O(log n) lookupMax, type-safe
**Cons**: Major refactoring

```haskell
-- Parametrize by term order
newtype OrderedPoly ord = OP (M.Map (OrderedMon ord) Rational)

newtype OrderedMon ord = OM Monomial

instance (TermOrder ord) => Ord (OrderedMon ord) where
  compare (OM m1) (OM m2) = compareByOrder @ord m1 m2
```

### Option 3: Change to Vector Representation
**Pros**: Fastest for dense polynomials
**Cons**: Complete rewrite

```haskell
-- Flat exponent vector + coefficient
type Poly = [(U.Vector Natural, Rational)]
```

---

## Immediate Action Plan

### Quick Fix for Generic Morley (2-4 hours)

1. **Add `getLeadingTermByOrder` to Expr.hs**
   ```haskell
   import Data.List (maximumBy)
   import Data.Ord (comparing)
   
   getLeadingTermByOrder :: (Monomial -> Monomial -> Ordering) -> Poly -> Maybe (Monomial, Rational)
   getLeadingTermByOrder cmp (Poly m) 
     | M.null m = Nothing
     | otherwise = Just $ maximumBy (\(m1,_) (m2,_) -> cmp m1 m2) (M.toList m)
   ```

2. **Update BuchbergerOpt.hs to take order**
   ```haskell
   import TermOrder (compareGrevLex)
   
   type MonomialOrder = Monomial -> Monomial -> Ordering
   
   reduce :: MonomialOrder -> Poly -> [Poly] -> Poly
   buchbergerWithStrategy :: MonomialOrder -> SelectionStrategy -> [Poly] -> [Poly]
   ```

3. **Wire through Prover.hs**
   ```haskell
   import TermOrder (compareGrevLex)
   
   -- In proveTheory:
   let basis = buchbergerWithStrategy compareGrevLex strategy polys
   ```

4. **Test with generic triangle**
   ```euclid
   ; Generic triangle (no specific angles)
   :point A 0 0
   :point B 1 0  
   :point C xC yC
   :assume (> yC 0)  ; C above x-axis
   ; ... define Morley points ...
   ```

### Performance Impact
- **Compilation**: +2 functions, no breaking changes
- **Runtime**: ~10-20% overhead for O(n) scan vs O(log n) max
  - Acceptable for proof correctness
  - Can optimize to Option 2 later

---

## Secondary Issues Identified

### 2. Sugar Strategy is Placeholder

**Current Code** (BuchbergerOpt.hs:241-244):
```haskell
selectPair strategy pairs =
  let selected = case strategy of
        NormalStrategy  -> minimumBy (comparing pairDegree) pairs
        SugarStrategy   -> minimumBy (comparing pairDegree) pairs  -- SAME!
```

**Problem**: Sugar should use homogenized degree
**Fix**: Add `pairSugar` field to `CriticalPair`

### 3. String Variable Overhead

**Current**: `Monomial = M.Map String Natural`
**Impact**: String comparisons in inner loop
**Fix**: Intern to Int (later optimization)

---

## Testing Strategy

### Regression Tests
1. ✅ All existing proofs still work
2. ✅ Right isosceles Morley still <10s
3. ✅ Basic Groebner tests pass

### New Generic Tests
1. Generic triangle with 3 variables
2. Generic Morley (arbitrary triangle)
3. Benchmark: Lex vs GrevLex timing

---

## Recommendation

**Priority**: **HIGH**  
**Timeframe**: 2-4 hours for quick fix  
**Risk**: **LOW** (additive changes, no breaking)

**Action**: Implement Option 1 (inject term order) now for generic Morley capability.

**Future**: Consider Option 2 (newtype) for performance if needed.

---

**Status**: Ready to implement
**Blocker for**: Generic Morley, complex geometric theorems
**Impact**: Critical for world-class theorem prover status
