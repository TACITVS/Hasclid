# CAD Completion Plan - v9.2

## Executive Summary

**Good News**: CAD is **90% implemented**! The core algorithm, projection, and lifting phases are complete and working.

**The Gap**: Performance, testing, and edge cases need work to make CAD production-ready for 3D+ problems.

**Goal**: Make CAD fast, reliable, and well-tested for multivariate inequalities up to 5 dimensions.

**Expected Impact**: Coverage increase from 60-70% → 80-85%

---

## Current State Analysis

### ✅ What's WORKING (Already Implemented)

1. **Core CAD Algorithm (CAD.hs)**:
   - ✅ Recursive polynomial representation
   - ✅ Pseudo-division (pseudoRem) - Collins 1975
   - ✅ Discriminant computation
   - ✅ Resultant computation via PSC
   - ✅ Complete Collins projection (all discriminants + resultants)

2. **CAD Lifting Phase (CADLift.hs)**:
   - ✅ 1D base case (univariate root isolation via Sturm)
   - ✅ Recursive lifting algorithm (liftCell)
   - ✅ Cell classification (Sector vs. Section)
   - ✅ Sign assignment for all polynomials
   - ✅ Tree-based decomposition (hierarchical)
   - ✅ Sample point generation
   - ✅ Formula evaluation in cells
   - ✅ Quantifier elimination (∃ and ∀)

3. **Integration**:
   - ✅ SolverRouter recognizes inequality problems
   - ✅ Routes to CAD automatically
   - ✅ Quantified formula support

### ⚠️ What's INCOMPLETE

1. **Performance**:
   - ⚠️ No projection optimizations (uses full Collins - overkill)
   - ⚠️ No incremental CAD
   - ⚠️ No parallel cell evaluation
   - ⚠️ Exponential blowup for 3D+ problems

2. **Robustness**:
   - ⚠️ No early termination when answer found
   - ⚠️ No heuristics for variable ordering
   - ⚠️ Limited error handling for degenerate cases

3. **Testing**:
   - ⚠️ Only 1 CAD test in test suite
   - ⚠️ No multivariate inequality stress tests
   - ⚠️ No regression tests

4. **Documentation**:
   - ⚠️ CAD capabilities poorly documented
   - ⚠️ No usage examples
   - ⚠️ No performance characteristics

---

## Completion Plan

### Phase 1: Optimize CAD Projection (Week 1)

**Goal**: Reduce projection set size by 50-70%

#### Task 1.1: Implement McCallum Projection
**Current**: Uses full Collins projection (all discriminants + all resultants)
**Improvement**: McCallum projection (1985) - much smaller set

**Benefits**:
- 50-70% fewer projection polynomials
- Faster decomposition
- Less memory usage

**Implementation**:
```haskell
-- src/CAD.hs
mcCallumProjection :: [Poly] -> String -> [Poly]
mcCallumProjection polys var =
  let
      -- Leading coefficients
      leadingCoeffs = [ leadingCoeff p var | p <- polys ]

      -- Discriminants of all polynomials
      discs = [ discriminant p var | p <- polys ]

      -- Resultants only between DISTINCT polynomials (not all pairs)
      results = [ resultant p q var | p <- polys, q <- polys, p /= q ]
  in
      nub (leadingCoeffs ++ discs ++ results)
```

**Testing**:
- Compare projection set sizes: Collins vs. McCallum
- Verify correctness on 2D/3D examples
- Benchmark performance improvement

**Estimated Time**: 2-3 days

---

#### Task 1.2: Add Early Termination
**Current**: Computes ALL cells even if answer found early
**Improvement**: Stop when proof/refutation found

**Benefits**:
- 2-10x speedup for provable theorems
- Much faster for "easy" problems

**Implementation**:
```haskell
-- src/CADLift.hs
cadDecomposeEarlyStop :: [Poly] -> [String] -> Formula -> Maybe Bool
-- Returns: Just True (proved), Just False (refuted), Nothing (need more cells)

-- Use in proveFormulaCAD:
proveFormulaCAD theory goal =
  case cadDecomposeEarlyStop (formulaToPolys goal) vars goal of
    Just result -> result
    Nothing -> -- fall back to full decomposition
```

**Testing**:
- Verify early termination triggers correctly
- Ensure no false positives/negatives
- Benchmark speedup on test suite

**Estimated Time**: 2 days

---

#### Task 1.3: Variable Ordering Heuristics
**Current**: Uses default variable order (arbitrary)
**Improvement**: Smart ordering based on problem structure

**Heuristics**:
1. **Degree-based**: Variables with lower max degree go deeper (less projection growth)
2. **Appearance-based**: Variables appearing in fewer polynomials go deeper
3. **Geometric**: Prefer natural geometric ordering (e.g., z before y before x)

**Implementation**:
```haskell
-- src/CADLift.hs
optimizeVariableOrder :: [Poly] -> [String] -> [String]
optimizeVariableOrder polys vars =
  sortBy (comparing (varScore polys)) vars
  where
    varScore polys v =
      (maxDegreeIn polys v, countAppearances polys v)
```

**Testing**:
- Compare different orderings on benchmark problems
- Measure cell count and time differences
- Document when heuristics help vs. hurt

**Estimated Time**: 2 days

---

### Phase 2: Robustness & Edge Cases (Week 2)

#### Task 2.1: Handle Degenerate Cases
**Issues**:
- Polynomials with no roots in variable
- Constant polynomials
- Zero polynomials

**Implementation**:
```haskell
-- Add guards in liftCell:
liftCell polys var (lowerCell, lowerSigns)
  | all isConstantInVar substituted =
      -- No critical values, just one sector
      [createSectorCell lowerCell var]
  | otherwise = -- existing logic
```

**Testing**:
- Test with constant constraints
- Test with infeasible systems
- Test with tautologies

**Estimated Time**: 1 day

---

#### Task 2.2: Improve Rational Root Refinement
**Current**: `refineToExactRoot` tries integers, then gives up
**Improvement**: Use bisection to arbitrary precision

**Implementation**:
```haskell
-- src/CADLift.hs
refineToRational :: [Rational] -> (Rational, Rational) -> Int -> Rational
-- Binary search with depth limit
```

**Testing**:
- Test with irrational roots (√2, π, etc.)
- Verify precision is sufficient
- Benchmark performance impact

**Estimated Time**: 2 days

---

#### Task 2.3: Better Error Messages
**Current**: Cryptic failures on edge cases
**Improvement**: Clear error messages with context

**Implementation**:
```haskell
-- Wrap CAD calls with informative errors
cadDecomposeWithErrors :: [Poly] -> [String] -> Either String [(CADCell, SignAssignment)]
```

**Testing**:
- Trigger various error conditions
- Verify messages are helpful

**Estimated Time**: 1 day

---

### Phase 3: Testing & Validation (Week 3)

#### Task 3.1: Comprehensive Test Suite
**Goal**: 100+ CAD-specific tests

**Test Categories**:
1. **Univariate** (20 tests):
   - x² ≥ 0
   - x² + 1 > 0
   - x² - 1 < 0
   - Polynomial inequalities of various degrees

2. **Bivariate** (30 tests):
   - x² + y² ≤ r² (circle)
   - x² + y² ≥ 1 (exterior of circle)
   - x + y ≥ 0 (half-plane)
   - xy > 1 (hyperbola)
   - Triangle inequalities

3. **Trivariate** (20 tests):
   - x² + y² + z² ≤ r² (sphere)
   - Geometric constraints in 3D
   - Mixed equalities and inequalities

4. **Quantifiers** (15 tests):
   - ∃x. x² = 4
   - ∀x. x² ≥ 0
   - ∃x,y. x² + y² = 1
   - Mixed quantifiers

5. **Edge Cases** (15 tests):
   - Empty solution sets
   - Tautologies (always true)
   - Contradictions (always false)
   - Degenerate configurations

**Implementation**:
```haskell
-- test/CADSpec.hs
describe "CAD Multivariate Inequalities" $ do
  it "proves circle containment" $
    proveWithCAD [] (Ge (exprSum [exprPow (Var "x") 2, exprPow (Var "y") 2]) (Const 0))
      `shouldBe` True
```

**Estimated Time**: 4-5 days

---

#### Task 3.2: Performance Benchmarks
**Goal**: Track CAD performance over time

**Benchmark Suite**:
- Variable count: 1D, 2D, 3D, 4D, 5D
- Polynomial degree: 1, 2, 3, 4
- Polynomial count: 1, 2, 5, 10
- Measure: time, memory, cell count

**Implementation**:
```haskell
-- bench/CADBench.hs (using Criterion)
```

**Estimated Time**: 2 days

---

#### Task 3.3: Regression Tests
**Goal**: Prevent future breakage

**Strategy**:
- Save failing/slow examples
- Add to automated test suite
- Track known limitations

**Estimated Time**: 1 day (ongoing)

---

### Phase 4: Documentation & Examples (Week 4)

#### Task 4.1: CAD User Guide
**Content**:
- What CAD can solve
- Performance characteristics
- Usage examples
- Limitations

**File**: `docs/CAD_USER_GUIDE.md`

**Estimated Time**: 2 days

---

#### Task 4.2: Example Gallery
**Goal**: 50+ multivariate inequality examples

**Categories**:
- Circle/sphere problems
- Triangle inequalities
- Optimization constraints
- Area/volume comparisons
- Geometric feasibility

**Format**: `.euclid` files in `examples/cad/`

**Estimated Time**: 2 days

---

#### Task 4.3: Performance Guide
**Content**:
- When to use CAD
- Expected performance (dimension, degree, count)
- Troubleshooting slow problems
- Optimization tips

**File**: `docs/CAD_PERFORMANCE.md`

**Estimated Time**: 1 day

---

## Timeline & Milestones

### Week 1: Performance Optimization
- ✅ Day 1-2: McCallum projection
- ✅ Day 3-4: Early termination
- ✅ Day 5: Variable ordering heuristics

### Week 2: Robustness
- ✅ Day 1: Degenerate case handling
- ✅ Day 2-3: Root refinement
- ✅ Day 4: Error messages

### Week 3: Testing
- ✅ Day 1-4: Comprehensive test suite (100+ tests)
- ✅ Day 5: Performance benchmarks
- ✅ Day 6: Regression tests setup

### Week 4: Documentation
- ✅ Day 1-2: CAD user guide
- ✅ Day 3-4: Example gallery
- ✅ Day 5: Performance guide

---

## Expected Results

### Before (v9.1):
- CAD works but slow for 3D+
- Limited testing (1 test)
- Poor documentation
- Coverage: ~30% of multivariate inequalities

### After (v9.2):
- CAD fast up to 5D with optimizations
- 100+ comprehensive tests
- Full user guide + examples
- Coverage: **80-85% of multivariate inequalities**

### Performance Improvements:
- 2-5x speedup from McCallum projection
- 2-10x speedup from early termination
- 1.5-2x from variable ordering
- **Overall: 5-20x speedup expected**

---

## Success Metrics

1. **Test Coverage**:
   - ✅ 100+ CAD tests in test suite
   - ✅ All tests passing
   - ✅ 90%+ coverage of CAD code

2. **Performance**:
   - ✅ 2D problems: < 1s
   - ✅ 3D problems: < 10s
   - ✅ 4D problems: < 60s
   - ✅ 5D problems: < 300s (5min)

3. **Documentation**:
   - ✅ Complete user guide
   - ✅ 50+ example files
   - ✅ Performance characteristics documented

4. **Integration**:
   - ✅ All SolverRouter paths working
   - ✅ No "not yet integrated" messages
   - ✅ Seamless automatic dispatch

---

## Risk Assessment

### Low Risk:
- ✅ Core algorithm already works
- ✅ Mostly optimization and polish
- ✅ Can test incrementally

### Medium Risk:
- ⚠️ McCallum projection correctness
- ⚠️ Variable ordering may not always help
- ⚠️ 5D+ still may be slow

### Mitigation:
- Extensive testing at each step
- Keep Collins projection as fallback
- Document performance limits clearly

---

## Next Steps

1. **Create feature branch**: `feature/cad-optimization`
2. **Start with Week 1, Task 1.1**: McCallum projection
3. **Test incrementally**: Don't break existing tests
4. **Commit frequently**: Small, focused commits
5. **Document as you go**: Update docs with each change

**Ready to begin?** Let's start with McCallum projection implementation!
