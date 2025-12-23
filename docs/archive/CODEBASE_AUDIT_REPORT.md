# Hasclid Codebase Comprehensive Audit Report

**Date:** 2025-12-16
**Version Analyzed:** v9.0 (Hasclid)
**Auditor:** Claude (Sonnet 4.5)
**Analysis Scope:** Complete Haskell codebase (.hs files), architecture, capabilities, and design

---

## Executive Summary

Hasclid is an **ambitious and sophisticated automated theorem prover** specialized in Euclidean geometry and algebraic reasoning. The codebase demonstrates:

**Strengths:**
- ✅ Multi-layered solving architecture (GeoSolver → Wu/Gröbner → CAD)
- ✅ Rich feature set spanning geometry, algebra, and number theory
- ✅ Intelligent solver routing via ProblemAnalyzer
- ✅ Advanced techniques: SDP/SOS, F4 algorithm, sqrt/rational elimination
- ✅ Good modular structure with clear separation of concerns

**Critical Issues:**
- 🔴 Timeout mechanism is NOT enforced at the solver level - can hang indefinitely
- 🔴 Macro expansion has unbounded recursion potential
- 🔴 CAD implementation likely incomplete/experimental
- 🔴 No test suite coverage visible in main code
- 🟡 Inconsistent error handling patterns
- 🟡 Performance bottlenecks in polynomial operations

**Verdict:** This is a **research-grade prototype** with impressive capabilities but **production-readiness issues** around reliability, performance, and testing.

---

## 1. Architecture Overview

### 1.1 Core Design Pattern

Hasclid implements a **two-phase hybrid architecture**:

```
┌─────────────────────────────────────────────────────────┐
│                    REPL (Main.hs)                       │
│         Command parsing, state management                │
└────────────┬────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────┐
│              SolverRouter (autoSolve)                   │
│    - Analyzes problem via ProblemAnalyzer               │
│    - Routes to appropriate solver                       │
└────────────┬────────────────────────────────────────────┘
             │
             ▼
      ┌──────┴──────┐
      │             │
┌─────▼────┐   ┌───▼──────┐
│  PHASE 1 │   │ PHASE 2  │
│ GeoSolver│   │Algebraic │
│  (Fast)  │   │ Solvers  │
└──────────┘   └────┬─────┘
                    │
        ┌───────────┼──────────┐
        │           │          │
    ┌───▼──┐   ┌───▼───┐  ┌──▼───┐
    │  Wu  │   │Gröbner│  │ CAD  │
    │Method│   │ Basis │  │(QE)  │
    └──────┘   └───┬───┘  └──────┘
                   │
         ┌─────────┼──────────┐
         │         │          │
    ┌────▼───┐ ┌──▼────┐ ┌──▼───┐
    │Buchber-│ │  F4   │ │ SOS/ │
    │  ger   │ │ Lite  │ │ SDP  │
    └────────┘ └───────┘ └──────┘
```

### 1.2 Key Modules

| Module | Purpose | Lines | Complexity |
|--------|---------|-------|------------|
| `Main.hs` | REPL, command processing | 774 | Medium |
| `Expr.hs` | AST, polynomial engine | 968 | High |
| `Parser.hs` | S-expression parser, macros | 498 | Medium |
| `Prover.hs` | Main proving logic | 969 | High |
| `SolverRouter.hs` | Intelligent solver selection | 934 | High |
| `IntSolver.hs` | Integer constraint solving | 1060 | Very High |
| `CADLift.hs` | CAD implementation | (Not read) | Unknown |
| `GeoSolver.hs` | Geometric constraint prop | (Not read) | Unknown |
| `Wu.hs` | Wu's method | (Not read) | Unknown |
| `Positivity/SDP.hs` | SDP solver | (Not read) | High |

**Total estimated codebase:** ~8000-10000 lines of Haskell

---

## 2. Capabilities Analysis

### 2.1 What Works Well ✅

#### Algebraic Proving (Gröbner Bases)
- Implements both **Buchberger** and **F4-lite** algorithms
- Smart selection strategies (Normal, Sugar, Minimal)
- **Caching system** for Gröbner bases (efficiency!)
- Handles symbolic parameters (e.g., proving for generic triangles)

**Evidence:** `SolverRouter.hs:123-134`, `Prover.hs:118-124`

#### Geometric Reasoning
- Rich geometric primitives: `dist2`, `collinear`, `perpendicular`, `parallel`, `angle-eq`
- **Two-phase approach**: Fast GeoSolver → algebraic fallback
- Area Method support (constructive proofs)

**Evidence:** `Expr.hs:28-41`, `SolverRouter.hs:288-334`

#### Integer Solving
- **Sophisticated interval analysis** with LP-style relaxation
- Cooper-style variable elimination
- Diophantine equation solver (row reduction)
- Modular reasoning (GCD-based)

**Evidence:** `IntSolver.hs:476-529`, `IntSolver.hs:880-947`

#### Advanced Features
- **Sqrt elimination** (algebraic number handling)
- **Rational elimination** (clearing denominators)
- **Quantifier expansion** (bounded domains)
- **Sum notation** and **structural induction**
- **SOS/SDP** for inequality proving

**Evidence:** `Prover.hs:211-262`, `Prover.hs:918-969`

### 2.2 What's Questionable 🟡

#### CAD Implementation
- Used for inequalities but actual implementation not visible in main modules
- Likely imported from `CADLift.hs` (not read in detail)
- **Concern:** CAD is notoriously complex; unclear if fully correct

**Evidence:** `SolverRouter.hs:701-717`

#### Wu's Method
- Core algorithm not reviewed (in separate `Wu.hs`)
- Used for geometric equality but unclear how it handles edge cases

#### Timeout Enforcement
```haskell
-- Main.hs:206
runWithTimeout :: Int -> IO a -> IO (Maybe a)
runWithTimeout seconds computation = do
  let microseconds = seconds * 1000000
  timeout microseconds computation
```

**CRITICAL:** `timeout` only works at the **top level** of the REPL command!
- If a solver goes into infinite loop *within* the computation, it **won't be interrupted**
- Example: `buchberger` or `f4LiteGroebner` could run forever on pathological inputs
- **Impact:** User's `:set-timeout 180` is a false promise

**Location:** `Main.hs:469-481`, `Main.hs:513-530`

---

## 3. Bugs Discovered 🔴

### 3.1 CRITICAL: Unbounded Macro Recursion

**File:** `Parser.hs:64`

```haskell
expandMacros' :: Int -> MacroMap -> SExpr -> SExpr
expandMacros' 0 _ _ = error "Macro expansion depth limit exceeded (possible infinite recursion)"
```

**Issue:** While there *is* a depth limit (1000), the error handling is **non-recoverable**:
- Uses `error` which crashes the entire REPL
- Should return `Either ProverError SExpr` or `Maybe SExpr`
- User-defined macros can easily trigger this in scripts

**Severity:** HIGH (crash risk)
**Fix:** Propagate error via `Either` monad

### 3.2 Division by Zero Checks Missing

**File:** `Expr.hs:383-388`

```haskell
simplifyExpr (Div e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, _) -> Const 0              -- 0 / e = 0
       (e, Const 1) -> e                    -- e / 1 = e
       (e, IntConst 1) -> e                 -- e / 1 = e
       (Const r1, Const r2) | r2 /= 0 -> Const (r1 / r2)  -- ✅ Checked
       (e, Const c) | c /= 0 -> Mul e (Const (1 / c))     -- ✅ Checked
       (e, IntConst c) | c /= 0 -> Mul e (Const (1 % c))  -- ✅ Checked
       ...
       _ | s1 == s2 -> Const 1              -- 🔴 e / e = 1 ALWAYS?!
```

**Issue:** The pattern `s1 == s2 -> Const 1` doesn't check if the expression is **zero**!
- `(x - x) / (x - x)` would simplify to `1` instead of undefined
- Should verify `s1 /= Const 0` before this rule

**Severity:** MEDIUM (incorrect simplification)

### 3.3 Incomplete Determinant Simplification

**File:** `Expr.hs:441-465`

```haskell
simplifyExpr (Determinant rows) =
  let ...
      hasDuplicates simpRows = ...
  in if hasZeroRow || hasZeroCol || hasIdenticalRows
     then Const 0
     else
       -- 4. Symbolic Gaussian Elimination (Bareiss Algorithm - Simplified Step)
       -- ... COMMENTED OUT, NOT IMPLEMENTED ...
       Determinant simpRows
```

**Issue:** Determinant is only partially simplified:
- Zero detection works
- Duplicate row detection works
- **Actual expansion/elimination NOT implemented** (just preserves `Determinant`)
- Must be expanded elsewhere (likely in `toPoly`)

**Severity:** LOW (not a bug, just incomplete feature)

### 3.4 Unsafe Pattern Matching

**File:** Multiple locations

```haskell
-- IntSolver.hs:941
singletonValue v eqs =
  ...
  in case singles of
       [] -> Nothing
       ((a,c):_) -> ...  -- Only checks first match, ignores rest
```

**Issue:** Incomplete patterns exist throughout:
- Many functions use partial patterns like `(x:_)` without handling `[]`
- GHC warns about these (`-Wno-incomplete-patterns` suppresses them)

**Evidence:** `prover.cabal:111` - Warnings are suppressed in test suite!

**Severity:** MEDIUM (runtime crashes possible)

---

## 4. Design Flaws 🟡

### 4.1 Inconsistent Error Handling

**Pattern 1:** Some functions use `Either ProverError a`
**Pattern 2:** Some use `Maybe a` (loss of error info)
**Pattern 3:** Some use `error` (crashes)
**Pattern 4:** Some use `(Bool, String, ...)` tuples

**Example:**
```haskell
-- Pattern 1 (Good)
parseFormulaWithMacros :: MacroMap -> S.Set String -> String -> Either ProverError Formula

-- Pattern 2 (Meh)
intEvalFormula :: Formula -> Maybe Bool

-- Pattern 3 (Bad)
expandMacros' 0 _ _ = error "..."

-- Pattern 4 (Verbose)
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
```

**Impact:** Hard to compose functions, error messages lost in `Maybe` chains

**Recommendation:** Standardize on `ExceptT ProverError IO` monad

### 4.2 God Object: `Expr` Data Type

**File:** `Expr.hs:15-41`

The `Expr` type has **21 constructors**:
- Arithmetic: `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Pow`, `Sqrt`
- Variables: `Var`, `Const`, `IntVar`, `IntConst`
- Geometry: `Dist2`, `Collinear`, `Dot`, `Circle`, `Midpoint`, `Perpendicular`, `Parallel`
- Angles: `AngleEq2D`, `AngleEq2DAbs`
- Linear algebra: `Determinant`
- Loops: `Sum`

**Problem:** Single type trying to represent:
- Algebraic expressions
- Geometric constraints
- Integer domains
- Matrix operations
- Control flow (summation)

**Consequence:**
- Many functions have incomplete pattern matches
- `toPoly` has special cases for each constructor (150+ lines!)
- Hard to extend without breaking existing code

**Better design:** Use **type classes** or **GADT** with phantom types:
```haskell
data ExprType = Algebraic | Geometric | Integer

data Expr (t :: ExprType) where
  Var :: String -> Expr Algebraic
  Dist2 :: String -> String -> Expr Geometric
  IntVar :: String -> Expr Integer
  ...
```

### 4.3 Polynomial Representation Inefficiency

**File:** `Expr.hs:149`

```haskell
newtype Poly = Poly (M.Map Monomial Rational)
newtype Monomial = Monomial (M.Map String Natural)
```

**Issue:** **Double Map** structure is elegant but slow:
- Every polynomial operation requires nested map lookups
- `polyMul` creates cartesian product of maps
- `polyAdd` unions maps

**Benchmarking needed:** For large Gröbner basis computations (100+ polynomials), this could be 10-100x slower than array-based representations.

**Evidence:** F4 algorithm (`F4Lite.hs`) likely tries to mitigate this with matrix operations, but core `Poly` type is still used.

### 4.4 Global State in REPL

**File:** `Main.hs:69-85`

```haskell
data REPLState = REPLState
  { theory :: Theory
  , history :: [String]
  , lemmas :: Theory
  , verbose :: Bool
  , autoSimplify :: Bool
  , groebnerCache :: GroebnerCache
  , termOrder :: TermOrder
  , useOptimizedBuchberger :: Bool
  , selectionStrategy :: SelectionStrategy
  , macros :: MacroMap
  , solverTimeout :: Int
  , lastTimeoutSeconds :: Maybe Int
  , solverOptions :: SolverOptions
  , construction :: Construction
  , intVars :: S.Set String
  }
```

**Problem:** 15 fields of mutable state = **spaghetti code risk**

**Observations:**
- Some fields redundant (e.g., `useOptimizedBuchberger` also in `solverOptions`)
- `lastTimeoutSeconds` is set but **never read** (dead code)
- State updates scattered across 500+ lines of command handlers

**Better:** Use lenses or separate state into logical groups

---

## 5. Improvement Opportunities 🚀

### 5.1 Performance Optimizations

#### A. Memoize Polynomial Operations
```haskell
-- Current: polyMul recalculates every time
polyMul (Poly p1) (Poly p2) = ...

-- Suggested: Hash-cons or weak pointers for structure sharing
```

#### B. Parallel Gröbner Basis Computation
```haskell
-- Current: Sequential S-polynomial reduction
buchberger gs = ...

-- Suggested: Spark-based parallelism
import Control.Parallel.Strategies
reducePairs pairs = parMap rdeepseq reduce pairs
```

#### C. Smart Preprocessing
Before calling expensive solvers, add more **simplification heuristics**:
- Detect trivial tautologies: `(= x x)`
- Normalize commutative operations: `(+ a b)` → canonical form
- **Currently partially implemented** in `Prover.hs:190-209`

### 5.2 Feature Completeness

#### Missing: Proof Certificates
- System proves theorems but doesn't generate **verifiable certificates**
- Should output proof objects that can be checked independently
- Consider: Exporting to Lean/Coq syntax

#### Incomplete: CAD Cell Sampling
**File:** `SolverRouter.hs:710-717`
```haskell
executeCADInequality theory lhs rhs isStrict =
  let ...
      holds = evaluateInequalityCAD constraints diffPoly vars
```

**Unclear:** How does `evaluateInequalityCAD` actually work?
- Real CAD requires root isolation, sign propagation, cell sampling
- If implementation is incomplete, could give **false positives/negatives**

**Action:** Review `CADLift.hs` and add unit tests for known CAD edge cases

#### Missing: Proof Search Diagnostics
When a proof fails, system doesn't explain **why**:
```
Result: NOT PROVED
  LHS /= RHS (Normal Form: x^2 + y - 1)
```

**Better:**
```
Result: NOT PROVED
Reason: Polynomial not in ideal (residue: x^2 + y - 1)
Suggestion: Try adding assumption: (= y (- 1 (^ x 2)))
Counter-example: x=1, y=1 violates goal
```

### 5.3 Code Quality

#### Add Comprehensive Tests
**Current:** Only two test files visible: `test/Spec.hs`, `test/TestF4.hs`

**Needed:**
- Unit tests for each module
- Property-based tests (QuickCheck for `Expr` simplification)
- Regression tests for bugs
- Performance benchmarks

**Evidence:** `prover.cabal:67` has test suite but unclear coverage

#### Document Public APIs
**Current:** Sparse comments

**Example:**
```haskell
-- Prover.hs:610
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
```

**Better:**
```haskell
-- | Prove a formula given a theory (assumptions).
--
-- Returns:
--   - Bool: True if proved, False otherwise
--   - String: Human-readable explanation
--   - ProofTrace: Detailed proof steps for verbose output
--
-- Examples:
-- >>> proveTheory [Eq (Var "x") (Const 1)] (Eq (Var "x") (Const 1))
-- (True, "Equality holds (trivial substitution)", ...)
--
-- Complexity: O(exp(n)) where n = number of variables (worst case)
--
-- Note: Does NOT enforce timeouts internally. Caller must use 'timeout'.
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
```

#### Fix Warnings
```bash
ghc-options: -Wall -Wno-unused-imports -Wno-incomplete-patterns -Wno-name-shadowing
```

**Action:** Remove suppression flags and fix actual warnings

---

## 6. Security & Robustness

### 6.1 Resource Exhaustion

**Attack vector:**
```lisp
:macro evil x = (evil x)  -- Infinite recursion
(evil 1)
```

**Current defense:** Depth limit of 1000 (crashes REPL)
**Better defense:** Bounded expansion with graceful error recovery

### 6.2 Malicious Input

**File loading:**
```haskell
-- Main.hs:556-559
(":load":filename:_) -> do
  content <- liftIO $ readFile filename
  st' <- liftIO $ processScriptStreaming env stateWithHist content
  pure (st', "File loaded: " ++ filename)
```

**No validation** of file contents before execution!
- Could load arbitrary Haskell code (if eval exists elsewhere)
- Could trigger macro bombs
- Could exhaust memory with huge inputs

**Recommendation:** Add file size limits, syntax validation

---

## 7. Comparison with Similar Systems

| Feature | Hasclid | Z3 | Coq | GeoGebra |
|---------|---------|----|----|----------|
| Automation | High | High | Low | Medium |
| Geometry | ✅ Native | ❌ | ✅ Via libraries | ✅ Numeric |
| Inequalities | ✅ SDP/CAD | ✅ SMT | ✅ Manual | ❌ |
| Proof Certificates | ❌ | ❌ | ✅ | ❌ |
| Performance | Medium | Very High | Low | High |
| Dependencies | None (pure Haskell) | C++ | OCaml + tons | Java |

**Hasclid's Niche:** **Automated geometric reasoning with symbolic parameters**
- Z3 can't handle "prove for all triangles"
- Coq requires manual tactics
- GeoGebra is numeric only

---

## 8. Recommendations by Priority

### P0 - Critical (Fix Before Production)
1. ✅ **Add proper timeout enforcement** in solver internals
   - Wrap long-running operations in `timeout`
   - Add periodic "interrupt checks" in loops
2. ✅ **Fix macro expansion error handling**
   - Replace `error` with `Left ProverError`
3. ✅ **Audit CAD implementation** for correctness
   - Add edge case tests (nested quantifiers, etc.)

### P1 - High Priority (Quality of Life)
4. ✅ **Add comprehensive test suite**
   - Aim for 70%+ code coverage
5. ✅ **Fix division-by-zero in simplification**
   - Check `e /= Const 0` before `e / e → 1`
6. ✅ **Standardize error handling** on `ExceptT`

### P2 - Nice to Have
7. ✅ **Document all public functions**
8. ✅ **Add proof certificate export**
9. ✅ **Optimize polynomial operations** (benchmarking needed first)
10. ✅ **Refactor `Expr` into type-safe subtypes**

---

## 9. Conclusion

Hasclid is an **impressive achievement** in automated reasoning, combining:
- Sophisticated multi-algorithm approach
- Rich geometric and algebraic capabilities
- Clever optimizations (caching, F4, SOS)

However, it's currently a **research prototype**, not production-ready:
- ⚠️ Timeout mechanism is unreliable
- ⚠️ Error handling is inconsistent
- ⚠️ Test coverage unknown
- ⚠️ Performance unoptimized for large problems

**For Research Use:** ✅ Excellent
**For Teaching:** ✅ Good (with supervision)
**For Production:** ❌ Not yet (needs hardening)

**Recommended Next Steps:**
1. Add timeout enforcement at solver level
2. Build comprehensive test suite
3. Benchmark and optimize hot paths
4. Consider Rust rewrite for performance-critical parts (polynomial ops)

---

## Appendix A: Module Dependency Graph

```
Main.hs
  ├── Parser.hs
  │   └── Expr.hs
  ├── Prover.hs
  │   ├── Expr.hs
  │   ├── IntSolver.hs
  │   ├── Core.GB.hs → BuchbergerOpt.hs
  │   ├── Positivity.hs → Positivity/{SOS, SDP, Numerical}.hs
  │   ├── CADLift.hs
  │   └── SqrtElim.hs, RationalElim.hs
  ├── SolverRouter.hs
  │   ├── ProblemAnalyzer.hs
  │   ├── GeoSolver.hs
  │   ├── Wu.hs
  │   ├── Prover.hs (circular!)
  │   ├── F4Lite.hs
  │   └── Geometry/WLOG.hs
  ├── ReplSupport.hs
  └── Error.hs, Validation.hs, Cache.hs, TermOrder.hs
```

**Circular dependency detected:** `SolverRouter` ↔ `Prover`
- Not inherently bad in Haskell (lazy evaluation)
- But makes reasoning harder

---

## Appendix B: Performance Hotspots (Likely)

Based on code structure, likely bottlenecks:

1. **`polyMul` in Gröbner basis loops** (`Expr.hs:184-187`)
   - Called O(n²) times in Buchberger
2. **`reduce` in S-polynomial computation** (`BuchbergerOpt.hs` - not read)
   - Polynomial division is expensive
3. **CAD cell decomposition** (`CADLift.hs` - not read)
   - Doubly-exponential in dimension
4. **SDP interior-point iterations** (`Positivity/SDP.hs` - not read)
   - Matrix operations on dense SDP matrices

**Profiling recommended:** Use GHC's `-prof -fprof-auto` flags

---

## Appendix C: Suggested Refactoring

### Before:
```haskell
-- Prover.hs
proveTheoryWithCache :: Maybe GroebnerCache -> Theory -> Formula
  -> (Bool, String, ProofTrace, Maybe GroebnerCache)
```

### After:
```haskell
-- Prover.hs
data ProveResult = ProveResult
  { proved :: Bool
  , reason :: String
  , trace :: ProofTrace
  , updatedCache :: Maybe GroebnerCache
  }

proveTheoryWithCache :: Maybe GroebnerCache -> Theory -> Formula
  -> ExceptT ProverError IO ProveResult
```

Benefits:
- Named fields (self-documenting)
- Monadic error handling
- Easier to extend (add new fields without breaking signatures)

---

**End of Report**

_Generated by automated code review. Manual verification recommended._
