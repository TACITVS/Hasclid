# GeoSolver Symbolic Handling Issues

## Identified Problems (2025-12-03)

### 1. Incomplete Perpendicularity Propagation
**File**: `src/GeoSolver.hs:169-179`
**Issue**: Only checks if first line (AB) is horizontal/vertical, not second line (CD)

**Example that fails**:
```
:point B S 0
:point C S S
:point D x y
:assume (= (perpendicular B C C D) 0)
```

BC is vertical (xB = xC = S), so CD should be horizontal (yC = yD).
But the code only checks `isHorizontal kb "B" "C"` and `isVertical kb "B" "C"`.
It should ALSO check if CD is horizontal/vertical and propagate to AB.

### 2. Missing General Perpendicularity
When neither line is axis-aligned, need to use dot product constraint:
- `(xB - xA)(xD - xC) + (yB - yA)(yD - yC) = 0`
- Can derive linear relationships between unknowns

### 3. No Symbolic Linear Equation Solver
**Missing**: `solveSymbolicLinear :: Expr -> Expr -> Maybe Expr`

Needed for cases like:
- `x + S = 2*S` → `x = S`
- `2*y - S = S` → `y = S`

Currently have `solveQuadratic` but not linear solver!

### 4. exprEqualsSymbolic Limitations
**File**: `src/Expr.hs:206-216`

Issues:
- No normalization of commutative operations: `a + b` ≠ `b + a`
- No associativity handling: `(a + b) + c` ≠ `a + (b + c)`
- Gives up on division: returns `False` immediately

## Fix Strategy

### Phase 1: Add Bidirectional Perpendicularity (Quick Win)
Modify `propagatePerpendicular` to check BOTH lines for horizontal/vertical orientation.

### Phase 2: Implement solveSymbolicLinear
Create function to solve `lhs = rhs` for a single unknown variable.

### Phase 3: Enhance exprEqualsSymbolic
Add normalization for commutativity and better polynomial comparison.

### Phase 4: General Perpendicularity
Use dot product expansion when neither line is axis-aligned.

## Test Cases Needed

1. ✅ Horizontal line ⊥ unknown → vertical
2. ✅ Vertical line ⊥ unknown → horizontal
3. ✅ Unknown line ⊥ horizontal → vertical
4. ✅ Unknown line ⊥ vertical → horizontal
5. ✅ General perpendicular lines (neither axis-aligned)
6. ✅ Commutative equality: `S + x = x + S`
7. ✅ Symbolic linear: `x + S = 2*S` → `x = S`
8. ✅ Symbolic linear: `2*y = S` → `y = S/2`
9. ✅ Chained perpendicularity: AB ⊥ BC, BC ⊥ CD
10. ✅ Square construction with symbolic side length
