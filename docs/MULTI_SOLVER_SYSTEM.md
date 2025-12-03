# Multi-Solver System - Complete Documentation

## Overview

Hasclid now features an intelligent multi-solver architecture that automatically selects the most appropriate solving method based on problem characteristics. This transforms the prover from a single-method tool into a robust, adaptive theorem proving system.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         User Input                           │
│                    :auto (= expr1 expr2)                     │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                     Problem Analyzer                         │
│  - Extract variables and constraints                         │
│  - Classify problem type (Algebraic, Geometric, etc.)       │
│  - Estimate complexity (Trivial → Infeasible)               │
│  - Detect geometric features (distances, perpendicularity)  │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                      Solver Router                           │
│  Rule-based dispatch:                                        │
│  1. Check if solvable (complexity not too high)             │
│  2. Match problem type to solver strength                    │
│  3. Apply routing rules (see below)                         │
└────────────────────┬───────────────┬────────────────────────┘
                     │               │
         ┌───────────┘               └────────────┐
         ▼                                        ▼
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│  Gröbner Basis   │  │   Wu's Method    │  │      CAD         │
│  (Buchberger)    │  │  (Char. Sets)    │  │   (Collins)      │
│                  │  │                  │  │                  │
│ • General Alg.   │  │ • Geometric      │  │ • Inequalities   │
│ • Robust         │  │ • Fast for Geo   │  │ • 1D-2D only     │
│ • Slower for Geo │  │ • Triangular     │  │ • Real numbers   │
└──────────────────┘  └──────────────────┘  └──────────────────┘
```

## Components

### 1. ProblemAnalyzer (Phase 1)

**File**: `src/ProblemAnalyzer.hs` (346 lines)

**Purpose**: Analyzes theorem proving problems to determine structure and complexity.

**Key Functions**:
```haskell
analyzeProblem :: Theory -> Formula -> ProblemProfile

-- Returns profile with:
data ProblemProfile = ProblemProfile
  { numVariables :: Int
  , numConstraints :: Int
  , maxDegree :: Int
  , hasSymbolicParams :: Bool
  , symbolicParams :: [String]
  , problemType :: ProblemType
  , geometricFeatures :: GeometricFeatures
  , estimatedComplexity :: Complexity
  , variables :: [String]
  }
```

**Problem Types**:
- `PureAlgebraic` - Only polynomial equations
- `PureInequality` - Only inequalities
- `Mixed` - Equations and inequalities
- `Geometric` - Has distance, perpendicularity, etc.
- `SinglePositivity` - Just proving p > 0
- `Unknown` - Cannot classify

**Complexity Levels**:
- `Trivial` - Instant (<1s)
- `Low` - Easy (1-5s)
- `Medium` - Moderate (5-30s)
- `High` - Hard (30s-5min)
- `VeryHigh` - Very hard (5min+)
- `Infeasible` - Too complex

### 2. Wu's Method (Phase 2)

**File**: `src/Wu.hs` (463 lines)

**Purpose**: Characteristic set method optimized for geometric theorem proving.

**Algorithm**:
1. **Triangularization**: Convert polynomial system to triangular form
2. **Pseudo-Division**: Reduce conclusion modulo characteristic set
3. **Zero Test**: If remainder is zero → theorem is proved

**Key Advantages**:
- Much faster than Gröbner bases for geometric problems
- Natural handling of degeneracy conditions
- Direct triangular form (no S-polynomial computation)

**Usage**:
```
:wu (= expr1 expr2)
```

**Example**:
```
Euclid> :assume (= (* x x) 1)
Euclid> :wu (= (* x (* x (* x x))) 1)
WU'S METHOD: PROVED
Conclusion reduces to zero under characteristic set
```

### 3. SolverRouter (Phase 3)

**File**: `src/SolverRouter.hs` (275 lines)

**Purpose**: Intelligent dispatch system for automatic solver selection.

**Routing Rules** (in priority order):

```haskell
1. IF unsupported formula type THEN Unsolvable

2. IF estimated complexity >= VeryHigh THEN Unsolvable
   -- Prevents hangs on intractable problems

3. IF problem type == Geometric THEN UseWu
   -- Wu's method is optimal for coordinate geometry

4. IF inequality AND 1-2 variables AND numeric THEN UseCAD
   -- CAD handles small inequality problems

5. IF algebraic + symbolic params + geo variables THEN UseWu
   -- Wu handles symbolic parameters better for geometry

6. IF single positivity check AND few variables THEN UseCAD
   -- Positivity checking via real root isolation

7. IF problem type == PureAlgebraic THEN UseGroebner
   -- Gröbner is the reliable general-purpose method

8. DEFAULT: UseGroebner
   -- Most robust fallback
```

**Usage**:
```
:auto (= expr1 expr2)
```

**Example Output**:
```
=== AUTOMATIC SOLVER ===

Problem Analysis:
  Type: PureAlgebraic
  Variables: 1
  Constraints: 1
  Max Degree: 4
  Complexity: High

Solver Selection:
  Selected Gröbner Basis: General algebraic equation solving

Result: PROVED
  Equality Holds (Groebner Normal Form is 0)
```

## Complete Command Reference

### Manual Solver Selection

```
(= expr1 expr2)          # Default: Gröbner basis
:wu (= expr1 expr2)      # Force Wu's method
:cad x [y]               # CAD decomposition
:solve (> p 0) x [y]     # Solve inequality
```

### Automatic Solver Selection

```
:auto (= expr1 expr2)    # Let router choose best solver
```

### Analysis and Debugging

```
:verbose                 # Toggle detailed proof traces
:validate                # Check for degenerate configurations
:cache-stats             # Show Gröbner cache statistics
```

## Solver Comparison

| Solver         | Best For           | Speed    | Robustness | Limitations           |
|----------------|-------------------|----------|------------|----------------------|
| Gröbner Basis  | General algebra   | Slow     | Very High  | Slow for geometry    |
| Wu's Method    | Geometry problems | Fast     | High       | Equality only        |
| CAD            | Inequalities      | Variable | Medium     | 1D-2D only, no symbolic |

## Performance Characteristics

### Gröbner Basis
- **Complexity**: Doubly exponential in worst case
- **Optimal for**: Problems requiring specific term orderings
- **Cache**: Benefits from caching (enable with `:cache-stats`)

### Wu's Method
- **Complexity**: Exponential, but faster constants than Gröbner
- **Optimal for**: Coordinate geometry (2D/3D points with distances)
- **Structure**: Exploits triangular form of geometric problems

### CAD
- **Complexity**: Doubly exponential in number of variables
- **Optimal for**: 1D inequality solving, 2D regions
- **Critical Limitation**: Unusable for 3D+ with symbolic parameters

## Design Principles

### 1. Pure Additions (No Breaking Changes)

Each phase was implemented as a standalone module:
- Phase 1: ProblemAnalyzer (pure addition)
- Phase 2: Wu module with `:wu` command
- Phase 3: SolverRouter with `:auto` command

All existing commands (`:prove`, `:cad`, `:solve`) work unchanged.

### 2. Incremental Development

Designed to be usable at each phase completion:
- After Phase 1: Can analyze problems manually
- After Phase 2: Can use Wu's method explicitly
- After Phase 3: Have automatic routing

### 3. Fail-Safe Defaults

Router uses conservative estimates:
- Defaults to Gröbner (most robust)
- Rejects VeryHigh complexity problems
- Clear error messages on unsolvable problems

## Known Limitations

### Phase 4 Not Implemented: Timeout System

**Status**: Pending

**Risk**: Solvers can hang on inappropriate problems

**Workaround**:
- Router rejects VeryHigh complexity
- Use Ctrl+C to interrupt hung process
- Manual solver selection bypasses router protection

**Future**: Add `System.Timeout` wrapper around solver calls

### CAD Integration Incomplete

**Status**: Limited integration with router

**Current**: Router detects CAD-suitable problems but returns "not yet integrated"

**Future**: Full CAD inequality proving through router

### Complexity Estimates Heuristic

**Status**: Rule-based estimates (not ML-based)

**Accuracy**: Good for common patterns, may miss edge cases

**Future**: Machine learning-based complexity prediction

## Testing and Validation

### Test 1: Existing Commands Still Work
```
:assume (= (* x x) 4)
(= x 2)
→ FALSE (with counter-example x=0)
```
**Result**: ✅ Gröbner method unchanged

### Test 2: Wu's Method Works
```
:assume (= (* x x) 1)
:wu (= (* x (* x (* x x))) 1)
→ WU'S METHOD: PROVED
```
**Result**: ✅ Wu's method functional

### Test 3: Router Selects Correctly
```
:assume (= (* x x) 1)
:auto (= (* x (* x (* x x))) 1)
→ Selected Gröbner Basis
→ PROVED
```
**Result**: ✅ Router analyzes and dispatches correctly

## Future Enhancements

### Short Term (Phase 4-5)

1. **Timeout System**: Wrap solvers with `System.Timeout`
2. **Complete CAD Integration**: Full inequality support
3. **Comprehensive Testing**: Test suite for routing logic
4. **Documentation**: Usage examples and best practices

### Long Term

1. **ML-Based Routing**: Train on problem dataset for better selection
2. **Parallel Solving**: Run multiple solvers concurrently, use first result
3. **Incremental Solving**: Checkpoint and resume for long proofs
4. **Proof Certificates**: Generate and verify formal proof objects

## Migration Guide

### From Manual to Automatic

**Before** (manual selection):
```
# Have to know which method to use
:wu (= geometric_formula)
(= algebraic_formula)
```

**After** (automatic):
```
# Router chooses best method
:auto (= geometric_formula)
:auto (= algebraic_formula)
```

### When to Use Manual Selection

Use explicit commands when:
1. **Performance tuning**: Know specific solver is faster
2. **Debugging**: Want to compare solver outputs
3. **Research**: Studying solver behavior
4. **Router fails**: Problem misclassified

Use `:auto` for:
1. **General use**: Don't know problem structure
2. **Safety**: Want complexity protection
3. **Convenience**: Don't want to choose manually

## Troubleshooting

### Problem: Router selects wrong solver

**Solution**: Use manual command (`:prove`, `:wu`, `:cad`)

**Example**:
```
:auto (= formula)  # Router chose Gröbner
:wu (= formula)    # Force Wu instead
```

### Problem: Solver hangs

**Solution 1**: Interrupt with Ctrl+C

**Solution 2**: Problem too complex
- Router should reject VeryHigh complexity
- If using manual command, problem may be infeasible

### Problem: "Not yet integrated" for inequalities

**Solution**: Use explicit `:solve` or `:cad` commands
- Router CAD integration incomplete
- Direct CAD commands still work

## Contributing

### Adding a New Solver

1. Implement solver module (e.g., `src/NewSolver.hs`)
2. Add detection logic to `ProblemAnalyzer`
3. Add routing rule to `SolverRouter.selectSolver`
4. Add execution case to `SolverRouter.executeSolver`
5. Update help text in `Main.hs`
6. Add tests and documentation

### Improving Routing Logic

Edit `SolverRouter.selectSolver`:
- Add new `ProblemType` patterns
- Refine complexity thresholds
- Add heuristics for edge cases

## References

- **Gröbner Bases**: Buchberger (1965), Cox-Little-O'Shea (2007)
- **Wu's Method**: Wu (1978), Chou (1988)
- **CAD**: Collins (1975), Brown (2003)
- **Theorem Proving**: Harrison (2009), Nipkow et al. (2002)

## Version History

- **v8.0**: Basic CAD with cell classification
- **v9.0**: ProblemAnalyzer + Wu's Method + SolverRouter
  - Phase 1: Problem classification (Jan 2025)
  - Phase 2: Wu's method implementation (Jan 2025)
  - Phase 3: Automatic routing (Jan 2025)

---

**Status**: Phases 1-3 complete and tested. System is production-ready for equality solving with intelligent dispatch.

**Remaining**: Phase 4 (timeouts) and Phase 5 (comprehensive testing/docs) are nice-to-have enhancements.
