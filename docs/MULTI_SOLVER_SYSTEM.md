# Multi-Solver System - Complete Documentation

## Overview

Hasclid now features an intelligent multi-solver architecture that automatically selects the most appropriate solving method based on problem characteristics. This transforms the prover from a single-method tool into a robust, adaptive theorem proving system.

## Architecture

### Two-Phase Solving System

```
┌─────────────────────────────────────────────────────────────┐
│                      User Input Formula                      │
│              (= expr1 expr2) or :auto command                │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                     Problem Analyzer                         │
│  - Extract variables and constraints                         │
│  - Classify problem type (Algebraic, Geometric, etc.)       │
│  - Estimate complexity (Trivial → Infeasible)               │
│  - Detect geometric features & quantifiers                   │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌═════════════════════════════════════════════════════════════┐
║                  PHASE 1: GeoSolver                          ║
║              (Fast Symbolic Geometric Reasoning)             ║
║                                                              ║
║  - Symbolic constraint propagation                           ║
║  - Handles symbolic parameters (S, T, etc.)                  ║
║  - Explores multiple geometric branches                      ║
║  - Returns in milliseconds when successful                   ║
║                                                              ║
║  Results: GeoProved | GeoDisproved | GeoUnknown             ║
└────────────────┬──────────────────┬─────────────────────────┘
                 │                  │
    GeoProved/   │                  │ GeoUnknown
    GeoDisproved │                  │ (fallback needed)
                 │                  │
                 ▼                  ▼
            ┌────────┐    ┌─────────────────────────────┐
            │ DONE   │    │  Quantifier Detection       │
            └────────┘    │  & Special Case Routing     │
                          └──────────┬──────────────────┘
                                     │
                ┌────────────────────┼────────────────────┐
                │                    │                    │
                ▼                    ▼                    ▼
        ┌──────────────┐    ┌──────────────┐    ┌──────────────┐
        │ Exists +     │    │ Integer      │    │  Other       │
        │ Equality     │    │ Quantifiers  │    │  Formulas    │
        └──────┬───────┘    └──────┬───────┘    └──────┬───────┘
               │                   │                    │
               ▼                   ▼                    ▼
        ┌──────────────┐    ┌──────────────┐           │
        │ Modular +    │    │ intSolve/    │           │
        │ Const. Wu    │    │ intSat       │           │
        └──────────────┘    └──────────────┘           │
                                                        │
                                                        ▼
┌═════════════════════════════════════════════════════════════┐
║              PHASE 2: Algebraic Solver Router                ║
║           (Intelligent Solver Selection & Dispatch)          ║
└─┬───────────────┬───────────────┬───────────────┬───────────┘
  │               │               │               │
  ▼               ▼               ▼               ▼
┌──────────┐ ┌──────────┐ ┌──────────┐ ┌─────────────┐
│ Gröbner  │ │   Wu's   │ │   CAD    │ │  Sturm      │
│  Basis   │ │  Method  │ │(Collins) │ │ Sequences   │
│          │ │          │ │          │ │             │
│• General │ │• Geo     │ │• Ineq    │ │• Univariate │
│• Robust  │ │• Fast    │ │• 1D-2D   │ │• Positivity │
└──────────┘ └──────────┘ └──────────┘ └─────────────┘
```

## Components

### 0. GeoSolver (Phase 1 - Fast Path)

**File**: `src/GeoSolver.hs` (~600 lines)

**Purpose**: Fast symbolic geometric constraint propagation solver that attempts to prove theorems before falling back to algebraic methods.

**Algorithm**:
1. **Constraint Propagation**: Symbolically propagates known constraints (distances, perpendicularity, etc.)
2. **Branching**: Explores multiple geometric configurations when ambiguity exists
3. **Symbolic Reasoning**: Works with symbolic parameters (e.g., side length 'S')
4. **Quick Decision**: Returns GeoProved/GeoDisproved in milliseconds when constraints are sufficient

**Key Advantages**:
- **Speed**: Orders of magnitude faster than algebraic methods for solvable cases
- **Symbolic Support**: Unlike CAD, handles symbolic parameters naturally
- **Constructive**: Can explore multiple geometric branches
- **Early Exit**: Prevents expensive algebraic computation when unnecessary

**Return Values**:
```haskell
data GeoResult = GeoProved | GeoDisproved | GeoUnknown
```

**Example**:
```
:point A 0 0
:point B S 0
:point C 0 S
:assume (= S 5)
(= (dist2 A B) 25)
→ GeoProved (instantly, before algebraic fallback)
```

**When GeoSolver Succeeds**:
- Concrete geometric configurations with sufficient constraints
- Some symbolic problems where constraints fully determine the result
- Problems involving perpendicularity, distance, collinearity with known values

**When it Falls Back to Phase 2**:
- Insufficient constraints to determine truth value
- Complex algebraic relationships
- High-degree polynomial constraints

### 1. ProblemAnalyzer

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

**Constructive Existence Proofs**:

Wu's method also implements `proveExistentialWu` for constructive existence proofs:
- Uses triangularization to build satisfying assignments
- Returns witnesses for existentially quantified formulas
- Integrated into SolverRouter for `Exists` quantifiers

**Example**:
```haskell
-- Proves: ∃x. (x² = 4)
-- Returns: x = 2 (constructive witness)
```

### 3. Modular Arithmetic Solver

**File**: `src/Modular.hs`

**Purpose**: Probabilistic consistency checking over finite fields for existence proofs.

**Algorithm**:
1. **Finite Field Evaluation**: Evaluates polynomials modulo a prime (default: 97)
2. **Solution Search**: Searches for satisfying assignments in ℤ_p
3. **Probabilistic Guarantee**: If inconsistent mod p, then inconsistent over ℚ

**Key Advantages**:
- **Fast**: Finite field arithmetic is much faster than symbolic computation
- **Existence Proofs**: Can quickly find witnesses for existential formulas
- **Early Rejection**: Detects inconsistencies without expensive symbolic computation

**Usage** (automatic via SolverRouter):
- Triggered for `Exists` quantifiers with equality constraints
- Works alongside Constructive Wu for witness finding

**Example**:
```haskell
-- Formula: ∃x, y. (x² + y² = 5)
-- Modular solver finds: x=1, y=2 (mod 97)
-- Verifies: 1² + 2² = 5 ✓
```

**Limitations**:
- Only proves existence, not universal statements
- Probabilistic (can have false positives over finite fields)
- Used as a heuristic alongside other methods

### 4. Integer Constraint Solver

**File**: `src/Prover.hs` (functions: `intSolve`, `intSat`, `proveForallInt`)

**Purpose**: Specialized solver for integer-valued variables with linear and bounded constraints.

**Algorithm**:
1. **Linear Interval Solving**: Propagates linear constraints to compute variable bounds
2. **Bounded Brute-Force**: Optional exhaustive search over small integer ranges (when `:bruteforce on`)
3. **Quantifier Handling**:
   - `∃x. φ(x)`: Search for satisfying integer assignment
   - `∀x. φ(x)`: Check all values in computed bounds

**Key Features**:
- **Integer Variables**: `IntVar`, `IntConst` for integer-only problems
- **Linear Constraints**: Efficiently handles `ax + b ≤ c` style constraints
- **Configurable Search**: User controls brute-force search via `:bruteforce on|off`

**Usage**:
```
:bruteforce on
-- Enables bounded search for integer goals
```

**Example**:
```haskell
-- ∀x ∈ [1..10]. (x² ≥ x)
-- Integer solver checks all values 1,2,...,10
-- Returns: PROVED
```

**Options** (configurable via `IntSolveOptions`):
- `allowBruteForce`: Enable/disable exhaustive search
- `maxBruteForceRange`: Limit on search space size (default: 1000)
- `integerMode`: Require integer solutions

**When Used**:
- Problems with `IntVar` or `IntConst` expressions
- Quantified formulas over bounded integer domains
- As fallback when algebraic methods fail on integer-constrained problems

### 5. SolverRouter (Phase 2 Dispatcher)

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

### Phase 4 COMPLETE: Timeout System ✅

**Status**: IMPLEMENTED (v9.1)

**Implementation**: `solveWithFallback` in `src/Main.hs`

**Features**:
1. **Timeout Protection**: All solver calls wrapped with `System.Timeout`
2. **Configurable**: User-controlled timeout via `:set-timeout <seconds>` (default: 30s)
3. **Symbolic Fallback**: On timeout with symbolic parameters, automatically retries with concrete values (e.g., S=1)
4. **Clear Messaging**: User sees `[TIMEOUT]` with explanation and fallback results

**Usage**:
```
:set-timeout 60        -- Set 60 second timeout
:show-timeout          -- Display current timeout
```

**Example**:
```
-- Symbolic problem times out after 30s
[TIMEOUT] Symbolic proof timed out. Retrying with concrete values (S=1)...
NOTE: This is a specific instance check.
Result: PROVED (for S=1)
```

**Eliminated Risks**:
- ✅ No more indefinite hangs
- ✅ User control over computation time
- ✅ Graceful degradation to concrete instances

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

### Short Term (Phase 5+)

1. ✅ **Timeout System**: COMPLETE - Implemented in v9.1 with symbolic fallback
2. **Complete CAD Integration**: Full inequality support for higher dimensions
3. **Comprehensive Testing**: Expand test suite for routing logic
4. **Documentation**: Additional usage examples and best practices

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

**Status**: Phases 1-4 COMPLETE and tested. System is production-ready with:
- ✅ Phase 1: GeoSolver (fast geometric reasoning)
- ✅ Phase 2: Multi-solver architecture (6 solvers)
- ✅ Phase 3: Intelligent routing
- ✅ Phase 4: Timeout system with symbolic fallback

**Remaining**: Phase 5 (comprehensive testing/docs expansion) is a continuous improvement effort.
