# GeoSolver Symbolic Support - Implementation Roadmap

## Current Status (v9.0)

### What Works ✅
- Multi-solver routing system (ProblemAnalyzer + SolverRouter)
- Wu's Method for geometric problems
- Router as default behavior
- GeoSolver skeleton created (src/GeoSolver.hs)

### What DOESN'T Work ❌
- **GeoSolver with symbolic parameters** (the critical missing piece!)
- Example: `examples/square_3_lines_proof.euclid` with symbolic S still hangs

## The Problem

`examples/square_3_lines_proof.euclid`:
```
:point A 0 0
:point B S 0
:point C S S
:point D x y
:assume (= (dist2 C D) (^ S 2))
:assume (= (perpendicular B C C D) 0)
(= (dist2 D A) (^ S 2))
```

**Mathematically** this is TRIVIAL (3 steps):
1. BC = (0, S) is vertical
2. BC ⊥ CD means CD horizontal → yD = S
3. dist(C,D) = S² with yD = S → xD ∈ {0, 2S}
4. Check dist(D,A): D=(0,S) → S²=S² ✓ but D=(2S,S) → 5S²≠S² → DISPROVE

**Current GeoSolver** tries to evaluate to `Rational`, fails on symbolic S, returns `GeoUnknown`, falls back to Wu → HANGS

## What Needs to Be Built

### Phase 1: Symbolic Expression Engine (2-3 hours)

**File**: `src/Expr.hs` (enhance existing) or `src/SymbolicSimplify.hs` (new)

**Core Functions Needed**:
```haskell
-- Substitute variables with expressions
substituteExpr :: String -> Expr -> Expr -> Expr
substituteExpr var value expr = ...
-- Example: substitute "S" with (Const 5) in (Var "S") → (Const 5)

-- Simplify expressions symbolically
simplifyExpr :: Expr -> Expr
-- S + 0 → S
-- S - S → 0
-- S * 1 → S
-- (Const a) + (Const b) → Const (a+b)

-- Check if two expressions are equal symbolically
exprEqualsSymbolic :: Expr -> Expr -> Bool
-- After simplification
```

**Test Cases**:
```haskell
simplifyExpr (Sub (Var "S") (Var "S")) == Const 0
simplifyExpr (Mul (Var "S") (Const 1)) == Var "S"
exprEqualsSymbolic (Var "S") (Add (Const 0) (Var "S")) == True
```

### Phase 2: Rewrite GeoSolver Core (2-3 hours)

**File**: `src/GeoSolver.hs`

**Change 1**: CoordMap stays as `M.Map String Expr` but never evaluates to Rational

**Change 2**: Rewrite `checkGoal` to work symbolically:
```haskell
-- OLD (broken):
checkGoal :: CoordMap -> Formula -> [String] -> (Maybe Bool, [String])
checkGoal kb (Eq lhs rhs) steps =
  case (evaluateExpr kb lhs, evaluateExpr kb rhs) of  -- FAILS on symbolic!
    (Just v1, Just v2) -> (Just (v1 == v2), ...)

-- NEW (symbolic):
checkGoal :: CoordMap -> Formula -> [String] -> (Maybe Bool, [String])
checkGoal kb (Eq lhs rhs) steps =
  let lhs' = substituteAll kb lhs  -- Replace all vars with their Expr
      rhs' = substituteAll kb rhs
      lhs'' = simplifyExpr lhs'
      rhs'' = simplifyExpr rhs'
  in if exprEqualsSymbolic lhs'' rhs''
     then (Just True, ...)
     else (Just False, ...)  -- Can prove NOT equal
```

**Change 3**: Handle multiple solutions from distance constraints:
```haskell
-- Distance gives TWO solutions: xD = xC + S or xD = xC - S
-- Need to try BOTH and find counter-example

propagateDistance :: CoordMap -> String -> String -> Expr
                  -> [(CoordMap, [String])]  -- Returns LIST of possibilities!
```

### Phase 3: Symbolic Equation Solving (2-3 hours)

**Pattern matching for simple cases**:
```haskell
solveQuadratic :: Expr -> Expr -> [Expr]
-- (x - a)² = b²  →  [a + b, a - b]
-- (x - Var "S")² = Pow (Var "S") 2  →  [Const 0, Mul (Const 2) (Var "S")]

solvePerpendicular :: Expr -> Expr -> Expr -> Expr -> Maybe (String, Expr)
-- Dot((0, S), (xD-S, yD-S)) = 0
-- S*(yD - S) = 0
-- → yD = S
```

### Phase 4: Multiple Solution Search (1-2 hours)

```haskell
-- Explore all branches from distance constraints
exploreSolutions :: CoordMap -> [CoordMap]
-- Try all combinations of ± for square roots

-- Check goal against all solutions
checkAllSolutions :: [CoordMap] -> Formula -> GeoResult
-- If ANY solution makes goal FALSE → DISPROVE
-- If ALL solutions make goal TRUE → PROVE
-- Otherwise → UNKNOWN
```

## Implementation Plan for Next Session

### Step 1: Add Symbolic Simplification (30-60 min)
- Add to `src/Expr.hs` or create `src/SymbolicSimplify.hs`
- Implement `simplifyExpr`, `substituteExpr`, `exprEqualsSymbolic`
- Write tests

### Step 2: Fix checkGoal to Work Symbolically (30-60 min)
- Rewrite `checkGoal` in `src/GeoSolver.hs`
- Test with symbolic expressions

### Step 3: Symbolic Perpendicular Propagation (60-90 min)
- Enhance `propagatePerpendicular` to derive yD = S symbolically
- Test: BC vertical + BC⊥CD → yC = yD

### Step 4: Symbolic Distance Solving (60-90 min)
- Implement `solveQuadratic` pattern matcher
- Handle multiple solutions
- Test: (xD - S)² = S² → xD ∈ {0, 2S}

### Step 5: Multi-Solution Search (60-90 min)
- Implement branch exploration
- Find counter-examples
- Test full square_3_lines_proof.euclid

### Step 6: Integration & Testing (30-60 min)
- Test with examples/square_3_lines_proof.euclid
- Verify it returns DISPROVED instantly
- Verify correct answer with trace

## Expected Result

```bash
$ cabal run prover < examples/square_3_lines_proof.euclid

RESULT: NOT PROVED
Solver: UseGeoSolver
Reason: Found counter-example via geometric reasoning

Derivation:
1. Line BC is vertical (xB = S, xC = S)
2. BC ⊥ CD implies CD is horizontal (yC = yD = S)
3. dist²(C,D) = S² with yD = S implies (xD - S)² = S²
4. Solving: xD ∈ {0, 2S}
5. Testing goal dist²(D,A) = S²:
   - D = (0, S): dist²(D,A) = S² ✓
   - D = (2S, S): dist²(D,A) = 5S² ✗ COUNTER-EXAMPLE!

Time: < 50ms (instant!)
```

## Total Estimate

- **Optimistic**: 5-6 hours of focused work
- **Realistic**: 6-8 hours with debugging
- **Pessimistic**: 8-10 hours if edge cases found

## Success Criteria

✅ examples/square_3_lines_proof.euclid solves in < 100ms
✅ Returns correct answer (NOT PROVED) with counter-example
✅ Shows clear derivation trace with symbolic reasoning
✅ Works with ANY symbolic parameter (not just S)

## Notes for Next Session

- The current GeoSolver in `src/GeoSolver.hs` is a SKELETON
- It works for concrete cases (S=1) but NOT symbolic
- Don't delete it - enhance it with symbolic support
- The architecture (two-phase, knowledge base, propagation) is CORRECT
- Only the evaluation layer needs to be rewritten

## Files to Modify

1. `src/Expr.hs` or new `src/SymbolicSimplify.hs` - Add symbolic operations
2. `src/GeoSolver.hs` - Rewrite evaluation to work symbolically
3. Test with `examples/square_3_lines_proof.euclid`

Good luck! This is the RIGHT approach - constraint propagation WILL work once symbolic support is added.
