# Hasclid Theorem Prover - Comprehensive Technical Assessment

**Version Analyzed:** v7.3
**Date:** December 2, 2025
**Analysis Type:** Codebase Review, Capability Assessment, Improvement Proposals

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Real Power & Strengths](#1-real-power--strengths)
3. [Limitations & Weaknesses](#2-limitations--weaknesses)
4. [Proposed Improvements](#3-proposed-improvements)
5. [Specific Code Improvements](#4-specific-code-improvements)
6. [Comparison with Other Provers](#5-comparison-with-other-provers)
7. [Conclusion](#conclusion)

---

## Executive Summary

Hasclid is a **sophisticated automated theorem prover for Euclidean geometry** that combines three powerful algebraic techniques:

1. **Gröbner Bases** (Buchberger's Algorithm) - For equality proofs
2. **Sturm's Theorem** - For univariate inequality proofs
3. **Cylindrical Algebraic Decomposition (CAD)** - For multivariate problems (partial implementation)

**Key Innovation:** LISP-like prefix notation DSL for expressing geometric theorems with exact rational arithmetic.

**Target Users:** Mathematics researchers, educators, students, and automated reasoning enthusiasts.

**Current Maturity:** Research prototype with production-quality equality proving, experimental inequality handling.

---

## 1. REAL POWER & STRENGTHS

### 1.1 Core Mathematical Engine

#### Gröbner Bases Implementation
**Location:** `src/Prover.hs:103-163`

```haskell
buchberger :: [Poly] -> [Poly]
```

- **Theoretically Sound**: Based on Hilbert's Nullstellensatz
- **Complete for Polynomial Ideals**: Can decide any polynomial equality over algebraically closed fields
- **S-Polynomial Computation**: Correct implementation of critical pair generation
- **Normal Form Reduction**: Multivariate polynomial division algorithm

**Proven Capabilities:**
- Automatically proves classic theorems (Thales, Apollonius, Stewart)
- Handles symbolic coordinates (not just numeric)
- Reduces geometric problems to ideal membership

#### Exact Rational Arithmetic
**Location:** `src/Expr.hs:83-106`

```haskell
type Rational = Ratio Integer
newtype Poly = Poly (M.Map Monomial Rational)
```

**Advantages:**
- No floating-point errors
- Mathematical correctness guaranteed
- Can work with fractions (1/2, 3/4) exactly
- Suitable for formal verification

**Example:**
```lisp
:point M 1/2 3/4  -- Stored as exact rationals, not 0.5 and 0.75
```

#### Sturm's Theorem Implementation
**Location:** `src/Sturm.hs:63-96`

```haskell
countRealRoots :: UPoly -> Int
```

**Features:**
- Computes Sturm sequences correctly
- Counts real roots in any interval
- Isolates roots using bisection
- Checks polynomial positivity via root counting

**Significance:** This is relatively rare in geometry provers. Most rely on numerical approximation or don't handle inequalities at all.

#### Partial CAD Implementation
**Location:** `src/CAD.hs`

**Implemented:**
- Recursive polynomial representation
- Pseudo-remainder algorithm (avoids division)
- Resultant computation (subresultant PRS)
- Discriminant calculation

**Use Case:** Projects multivariate polynomials to eliminate variables.

### 1.2 Geometric Capabilities

Successfully handles these geometric primitives:

| Primitive | Formula | Implementation |
|-----------|---------|----------------|
| **Distance²** | `(x₁-x₂)² + (y₁-y₂)² + (z₁-z₂)²` | `Expr.hs:285-289` |
| **Collinear** | `det([AB, AC]) = 0` | `Expr.hs:300-306` |
| **Dot Product** | `AB · CD` | `Expr.hs:291-298` |
| **Circle** | `dist²(P,C) = r²` | `Expr.hs:308-311` |
| **Midpoint** | `2M = A + B` | `Expr.hs:316-324` |
| **Perpendicular** | `AB · CD = 0` | `Expr.hs:327` |
| **Parallel** | `AB × CD = 0` | `Expr.hs:332-345` |

**Example Success - Thales' Theorem:**
```lisp
-- Angle inscribed in semicircle is 90°
:point O 0 0
:point A -r 0
:point C r 0
:point B u v
:assume (= (circle B O r) 0)
(= (dot B A B C) 0)  -- ✓ PROVED
```

### 1.3 Software Engineering Strengths

#### Clean Module Architecture
```
Expr.hs    → Core AST & polynomial engine
Parser.hs  → S-expression parser
Prover.hs  → Buchberger's algorithm
Sturm.hs   → Real root counting
CAD.hs     → Cylindrical decomposition tools
Main.hs    → REPL & user interface
```

**Benefits:**
- Easy to understand and modify
- Clear separation of concerns
- Testable components

#### Interactive REPL
**Location:** `src/Main.hs:405-420`

**Features:**
- Command history
- Persistent state (assumptions + lemmas)
- Script loading (`.euclid` files)
- Error recovery (doesn't crash on parse errors)

#### Lemma Library System
**Location:** `src/Main.hs:268-293`

```lisp
:lemma (= (dist2 A B) 9)      -- Prove and save
:save-lemmas basics.lemmas     -- Persist to file
:load-lemmas basics.lemmas     -- Reuse in future sessions
```

**Impact:** Enables incremental proof development and theorem reuse.

#### Proof Tracing
**Location:** `src/Prover.hs:22-72`

```haskell
data ProofTrace = ProofTrace
  { steps :: [ProofStep]
  , usedAssumptions :: Theory
  , basisSize :: Int
  }
```

**Verbose Mode Output:**
```
==========================================
PROOF EXPLANATION:
==========================================

Used Assumptions (6):
  * xB = 3
  * yB = 0
  ...

Proof Steps:
  1. Used substitution: xB -> 3
  2. Used substitution: yB -> 0
  3. Computed Groebner basis (0 polynomials)
  4. Reduced to normal form: 0 (PROOF COMPLETE)
==========================================
```

#### Expression Simplification
**Location:** `src/Expr.hs:178-234`

```haskell
simplifyExpr :: Expr -> Expr
```

**Rules:**
- `0 + e = e`, `e + 0 = e`
- `0 * e = 0`, `1 * e = e`
- `e - e = 0`, `e / e = 1`
- Constant folding

**Toggle:** `:auto-simplify` command

---

## 2. LIMITATIONS & WEAKNESSES

### 2.1 Theoretical Limitations

#### A. Incomplete Inequality Handling

**Problem Location:** `src/Prover.hs:232-254`

```haskell
checkPositivity :: Poly -> Bool -> (Bool, String)
checkPositivity p allowZero =
  case toUnivariate p of
    Just (var, coeffs) ->
        -- ✓ STURM WORKS: univariate case
    Nothing ->
        -- ✗ HEURISTIC ONLY: multivariate case
        if isTrivialSOS p
        then (True, "Sum of Squares (Heuristic)")
        else (False, "Multivariate polynomial...")
```

**Limitation:** `isTrivialSOS` only recognizes trivial sum-of-squares forms like `x² + y²`.

**Cannot Prove:**
```lisp
-- Triangle inequality: |AB| + |BC| ≥ |AC|
-- Requires sqrt, which creates non-polynomial constraints
(>= (+ (dist2 A B) (dist2 B C)) (dist2 A C))  -- FAILS

-- General AM-GM inequality
(>= (+ x y) (* 2 (^ (* x y) 1/2)))  -- FAILS (fractional exponent)
```

**Missing Theory:**
- Full sum-of-squares (SOS) decomposition via semidefinite programming
- Positivstellensatz (Stengle/Schmüdgen certificates)
- Complete CAD lifting phase

#### B. CAD is Partial (Missing Lifting Phase)

**Implemented:** Projection phase only
```haskell
discriminant :: Poly -> String -> Poly  -- ✓ Eliminates one variable
```

**Missing:** Lifting phase
```haskell
-- Should exist but doesn't:
cadLift :: [Poly] -> [String] -> SampleTree
```

**Impact:** The `:solve` command for 2D is incomplete:

```lisp
:solve (> (+ (* x x) (* y y) -1) 0) x y
-- Returns "sample points" but not guaranteed to be correct
```

**What's Needed:**
1. Build sample tree bottom-up
2. Test constraints at each sample point
3. Propagate validity up the tree

#### C. No Non-degeneracy Conditions

**Problem:** No checks for degenerate geometric configurations.

**Examples that should warn but don't:**
```lisp
:point A 0 0
:point B 0 0  -- A and B coincide!

:point P 0 0
:point Q 1 0
:point R 2 0  -- P, Q, R are collinear (degenerate triangle)
```

**Consequence:** Can give false proofs for degenerate cases.

**What's Needed:**
```haskell
data GeometricConstraint =
  NonCoincident Point Point
  | NonCollinear Point Point Point
  | PositiveDistance Point Point

validateConfiguration :: [Point] -> [GeometricConstraint] -> Bool
```

#### D. Coordinate-Based Only

**Limitation:** All geometry is reduced to Cartesian coordinates.

**Cannot Express:**
- Ruler-and-compass constructions
- Projective geometry (points at infinity)
- Geometric transformations as first-class objects
- Synthetic geometry (angle bisectors, circumcenters without coordinates)

**Alternative Approaches Missing:**
- Wu's method (characteristic sets)
- Area method (Chou-Gao-Zhang)
- Full-angle method

### 2.2 Implementation Issues

#### A. Performance Problems

**Issue 1: Buchberger Has No Optimizations**

**Location:** `src/Prover.hs:153-162`

```haskell
buchberger :: [Poly] -> [Poly]
buchberger polys = go (filter (/= polyZero) polys)
  where
    go basis =
      let pairs = [ (f, g) | f <- basis, g <- basis, f /= g ]
```

**Problems:**
- Generates `O(n²)` S-polynomial pairs
- No selection strategy (processes all pairs)
- No Buchberger criteria (to skip useless pairs)
- No sugar strategy (degree-based ordering)

**Impact:** Exponential blowup on larger problems.

**Example:**
```lisp
-- Simple problem with 10 assumptions
-- Generates 45 S-polynomial pairs
-- Most are redundant!
```

**Better Algorithms Exist:**
- F4 (Faugère 1999) - Uses linear algebra
- F5 (Faugère 2002) - With signature-based criteria
- Gröbner walk - For term order conversion

**Issue 2: No Control Over Term Ordering**

**Location:** `src/Expr.hs:61`

```haskell
newtype Monomial = Monomial (M.Map String Natural)
```

Uses `Data.Map`'s default ordering (lexicographic by variable name).

**Problem:** Term ordering **critically affects** Gröbner basis size and computation time.

**Example:**
```haskell
-- Same ideal, different orderings:
-- Lex order:   basis = [x² - 1, y - x]
-- GrLex order: basis = [x² - 1, y² - 1, xy - x]
```

**What's Needed:**
```haskell
data TermOrder = Lex | GrLex | GrevLex | Custom

monomialCompare :: TermOrder -> Monomial -> Monomial -> Ordering
```

#### B. Sparse Error Handling

**Problem:** Many functions use `error` which crashes the program.

**Examples:**

```haskell
-- Expr.hs:282
toPoly (Div _ _) = error "Division not supported"

-- Parser.hs:24
parseSExpr [] = error "Unexpected end of input"

-- CAD.hs:39
polyRem f g
  | gNorm == [] = error "Division by zero polynomial"
```

**Better Approach:**
```haskell
-- Use Either for error handling
toPoly :: Expr -> Either String Poly
toPoly (Div _ _) = Left "Division is not supported in polynomial expressions"
```

#### C. No Proof Certificates

**Problem:** Verbose mode shows steps but doesn't generate **verifiable certificates**.

**Current Output:**
```
PROOF EXPLANATION:
Used Assumptions: ...
Proof Steps: ...
```

**Cannot:**
- Export to Coq/Lean/Isabelle for formal verification
- Independently verify the proof
- Store minimal proof witnesses

**What's Needed:**
```haskell
data ProofCertificate =
  GroebnerProof {
    ideal :: [Poly],
    basis :: [Poly],
    normalForm :: Poly,
    cofactors :: [Poly],  -- Bézout coefficients
    reductionTrace :: [ReductionStep]
  }

exportToCoq :: ProofCertificate -> String
verifyProof :: ProofCertificate -> Bool
```

#### D. Limited 3D Support

**Problem:** Can define 3D points but lacks 3D-specific primitives.

**Missing:**
```lisp
-- No plane equations
(plane A B C)  -- Plane through 3 points

-- No 3D cross product magnitude
(cross A B C D)  -- |AB × CD|

-- No volume calculations
(volume A B C D)  -- Tetrahedron volume
```

### 2.3 Usability Issues

#### A. LISP Syntax Barrier

**Current:**
```lisp
(= (+ (dist2 A B) (dist2 A C)) (* 2 (+ (dist2 A M) (dist2 B M))))
```

**Problem:** Intimidating for users unfamiliar with LISP/prefix notation.

**Wish:**
```
dist2(A,B) + dist2(A,C) = 2 * (dist2(A,M) + dist2(B,M))
```

#### B. No Visualization

**Problem:** Cannot see geometric configurations.

**Missing:**
- Diagram generation (SVG/TikZ output)
- Interactive geometry editor
- Animation of proof steps

#### C. Limited Documentation

**Missing:**
- Formal specification of provable theorems
- Complexity guarantees (time/space)
- Benchmark suite
- Failure case database

---

## 3. PROPOSED IMPROVEMENTS

### Priority 1: Core Proof Engine

#### A. Enhanced Gröbner Basis Algorithm

**Implementation Plan:**

```haskell
-- src/Prover.hs

data SelectionStrategy =
  Normal           -- FIFO
  | Sugar          -- Minimum total degree
  | Gebauer        -- Gebauer-Möller criteria

data BuchbergerCriteria =
  Criterion1       -- Relatively prime leading monomials
  | Criterion2     -- Product criterion

needsComputation :: Poly -> Poly -> [Poly] -> Bool
needsComputation f g basis =
  not (criterion1 f g) && not (criterion2 f g basis)

buchbergerOpt :: SelectionStrategy -> [Poly] -> [Poly]
buchbergerOpt strategy polys =
  let pairs = generatePairs polys
      filtered = filter needsComputation pairs
      sorted = sortByStrategy strategy filtered
  in processPairs sorted []
```

**Expected Impact:**
- 10-100x speedup on medium problems
- Enables tackling larger theorems

#### B. Complete CAD Implementation

**Implementation Plan:**

```haskell
-- src/CAD.hs

data SampleTree =
  Leaf Rational
  | Branch [(Interval, SampleTree)]

-- Complete 2-phase CAD
cadSolve :: [Poly] -> [String] -> SampleTree
cadSolve constraints vars =
  let projections = cadProject constraints vars
      basePhase = isolateRoots (last projections)
  in cadLift projections basePhase

cadLift :: [[Poly]] -> [Interval] -> SampleTree
-- Recursively lift samples through projection levels
```

**Expected Impact:**
- Correct 2D/3D inequality solving
- Can handle problems like "find all configurations where triangle is acute"

#### C. Quantifier Elimination

**Implementation Plan:**

```haskell
-- src/Prover.hs

data Formula =
  Eq Expr Expr
  | Ge Expr Expr
  | Gt Expr Expr
  | Exists String Formula
  | Forall String Formula
  | And Formula Formula
  | Or Formula Formula

-- Tarski-Seidenberg decision procedure
eliminateQuantifiers :: Formula -> Formula
eliminateQuantifiers (Exists x f) =
  let constraints = extractConstraints f
      shadow = project constraints x
  in formulaFromPoly shadow

-- Example usage:
-- ∃x. (x² + y = 0) → y ≤ 0
```

**Expected Impact:**
- Can solve geometric construction problems
- Handles problems like "does there exist a point P such that..."

### Priority 2: Alternative Proof Methods

#### A. Wu's Method

**Rationale:** Often faster than Gröbner bases for geometric theorems.

**Implementation Plan:**

```haskell
-- src/Wu.hs

-- Characteristic set representation
type CharacteristicSet = [Poly]

-- Wu's method for geometric proving
wuMethod :: Theory -> Formula -> Bool
wuMethod assumptions goal =
  let charSet = computeCharSet assumptions
      reduced = pseudoDivide goal charSet
  in reduced == polyZero

computeCharSet :: Theory -> CharacteristicSet
-- Uses ascending chain decomposition
```

**Expected Impact:**
- Faster proofs for many geometric theorems
- Handles some cases where Gröbner fails

#### B. Area Method (Chou-Gao-Zhang)

**Rationale:** Natural for geometry, often more efficient.

**Implementation Plan:**

```haskell
-- src/AreaMethod.hs

data AreaExpr =
  Area Point Point Point        -- Signed area of triangle
  | PythagorasDiff Point Point Point Point

-- Express geometric constraints as area expressions
toAreaExpr :: Expr -> AreaExpr

-- Prove using area elimination
proveByArea :: Theory -> Formula -> Bool
```

**Example:**
```lisp
-- Collinearity: Area(A,B,C) = 0
-- Distance: Pythagoras(A,B,C,D) = AB² - CD²
```

#### C. Non-degeneracy Checking

**Implementation Plan:**

```haskell
-- src/Geometry.hs

data GeometricConstraint =
  NonCoincident Point Point
  | NonCollinear Point Point Point
  | PositiveDistance Point Point
  | NonZeroArea Point Point Point

validateConfiguration :: [Point] -> Theory -> [GeometricConstraint]
validateConfiguration points theory =
  [ constraint | constraint <- generate points,
                 not (implied theory constraint) ]

-- Warn user about degeneracies
:point A 0 0
:point B 0 0
-- WARNING: Points A and B coincide! This may lead to trivial proofs.
```

### Priority 3: User Experience

#### A. Infix Syntax Option

**Implementation Plan:**

```haskell
-- src/Parser.hs

data SyntaxMode = Prefix | Infix

-- Parser that accepts both:
-- Prefix: (= (dist2 A B) 9)
-- Infix:  dist2(A,B) = 9

parseFormula :: SyntaxMode -> String -> Either String Formula
```

**Command:**
```lisp
:syntax infix
dist2(A,B) = 9  -- Now works!
```

#### B. Proof Certificates

**Implementation Plan:**

```haskell
-- src/Certificate.hs

data ProofCertificate =
  GroebnerProof {
    ideal :: [Poly],
    basis :: [Poly],
    goal :: Poly,
    normalForm :: Poly,
    cofactors :: [Poly],  -- Goal = Σ cofactor[i] * basis[i]
    reductionSteps :: [ReductionStep]
  }

-- Export formats
exportToCoq :: ProofCertificate -> String
exportToLean :: ProofCertificate -> String
exportToJSON :: ProofCertificate -> String

-- Verification
verifyGroebnerProof :: ProofCertificate -> Bool
```

**Usage:**
```lisp
:certificate on
(= (dist2 A B) 9)
-- Saves proof certificate to proof.json
```

#### C. Interactive Proof Mode

**Implementation Plan:**

```haskell
-- src/Main.hs

data ProofState =
  ProofState {
    goal :: Formula,
    currentTheory :: Theory,
    remainingGoals :: [Formula]
  }

-- Tactics
:tactic apply-lemma thales
:tactic substitute x = 0
:tactic reduce
:tactic split-cases
```

**Example Session:**
```lisp
:prove (= (dist2 A B) 25)
-- Current goal: dist2(A,B) = 25
:tactic expand
-- Expanded: (xB-xA)² + (yB-yA)² = 25
:tactic substitute xA=0, yA=0
-- Simplified: xB² + yB² = 25
:tactic substitute xB=3, yB=4
-- Goal: 9 + 16 = 25
:tactic arithmetic
-- ✓ PROVED
```

#### D. Visualization

**Implementation Plan:**

```haskell
-- src/Visualize.hs

-- Generate SVG diagrams
renderConfiguration :: Theory -> SVG
renderConfiguration theory =
  let points = extractPoints theory
      constraints = extractConstraints theory
  in drawPoints points <> drawConstraints constraints

-- Commands:
:visualize            -- Shows current configuration as SVG
:visualize-proof      -- Animates proof steps
:export-tikz          -- Generates LaTeX/TikZ code
```

**Output:**
```svg
<svg width="400" height="400">
  <circle cx="0" cy="0" r="100" />
  <line x1="0" y1="0" x2="100" y2="0" />
  ...
</svg>
```

### Priority 4: Advanced Features

#### A. Constraint Solving & Construction

**Implementation Plan:**

```haskell
-- src/Constructor.hs

-- Parse geometric construction problems
:construct "right triangle with legs 3,4"
-- Generates:
-- :point A 0 0
-- :point B 3 0
-- :point C 0 4
-- :assume (= (perpendicular A B A C) 0)

:construct "equilateral triangle with side 5"
-- Uses constraint solving to find coordinates
```

**Under the hood:**
```haskell
solveConstruction :: String -> Either String Theory
solveConstruction desc =
  let constraints = parseDescription desc
      solution = cadSolve constraints
  in generateTheory solution
```

#### B. Counter-example Finding

**Implementation Plan:**

```haskell
-- src/Counterexample.hs

findCounterexample :: Theory -> Formula -> Maybe (M.Map String Rational)
findCounterexample theory (Eq l r) =
  let poly = toPolySub (buildSubMap theory) (Sub l r)
      vars = extractVars poly
  in searchForZero poly vars

-- Usage:
(= (dist2 A B) 100)
-- RESULT: FALSE
:counterexample
-- Counter-example found: A=(0,0), B=(3,4)
-- Evaluates to: dist2(A,B) = 25 ≠ 100
```

#### C. Metric Geometry (Angles)

**Implementation Plan:**

```haskell
-- src/Expr.hs

data Expr =
  ...
  | Angle Point Point Point  -- Angle ABC (in radians)

-- Conversion using arctan
toAngleConstraint :: Point -> Point -> Point -> Poly
-- Requires transcendental functions → needs interval arithmetic
```

**Challenge:** Angles involve `arctan`, which is transcendental (not polynomial).

**Solution:** Use interval arithmetic or cylindrical algebraic decomposition with transcendental extensions.

#### D. Automated Proof Search

**Implementation Plan:**

```haskell
-- src/AutoProve.hs

data Strategy =
  DirectProof
  | ByContradiction
  | ByInduction
  | CaseAnalysis
  | LemmaApplication

autoProve :: Formula -> [Strategy] -> Maybe Proof
autoProve goal strategies =
  tryStrategies goal strategies

:auto-prove (= (dist2 A B) 25)
-- Trying DirectProof... ✓ SUCCESS
-- Proof found in 0.02s using DirectProof strategy

:hint
-- Suggestion: Try applying lemma 'pythagorean'
-- Suggestion: Substitute xA=0, yA=0
```

### Priority 5: Performance & Scalability

#### A. Parallel Computation

**Implementation Plan:**

```haskell
import Control.Parallel.Strategies

buchbergerParallel :: [Poly] -> [Poly]
buchbergerParallel polys =
  let pairs = generatePairs polys
      sPolys = parMap rdeepseq (uncurry sPoly) pairs
      remainders = parMap rdeepseq (`reduce` polys) sPolys
  in processRemainders remainders polys
```

**Expected Impact:**
- Near-linear speedup on multi-core systems
- Enables tackling much larger problems

#### B. Caching & Memoization

**Implementation Plan:**

```haskell
-- src/Cache.hs

type BasisCache = M.Map [Poly] [Poly]

cachingBuchberger :: IORef BasisCache -> [Poly] -> IO [Poly]
cachingBuchberger cache polys = do
  cached <- M.lookup polys <$> readIORef cache
  case cached of
    Just basis -> return basis
    Nothing -> do
      basis <- return (buchberger polys)
      modifyIORef cache (M.insert polys basis)
      return basis
```

#### C. Incremental Proving

**Implementation Plan:**

```haskell
-- Instead of recomputing entire basis when adding assumptions:

data IncrementalBasis =
  IncrementalBasis {
    currentBasis :: [Poly],
    addedPolys :: [Poly]
  }

addAssumption :: IncrementalBasis -> Poly -> IncrementalBasis
addAssumption ib newPoly =
  let newPairs = [ (newPoly, f) | f <- currentBasis ib ]
      newSPolys = map (uncurry sPoly) newPairs
      newBasis = processNewSPolys newSPolys (currentBasis ib)
  in IncrementalBasis newBasis (newPoly : addedPolys ib)
```

**Expected Impact:**
- Much faster when proving multiple related theorems
- Better REPL responsiveness

---

## 4. SPECIFIC CODE IMPROVEMENTS

### A. Better Term Ordering

**File:** `src/Expr.hs`

```haskell
-- Add term ordering types
data TermOrder = Lex | GrLex | GrevLex | Elimination [String]

-- Modify Monomial comparison
instance Ord Monomial where
  compare = monomialCompare currentOrdering  -- Global or passed as parameter

monomialCompare :: TermOrder -> Monomial -> Monomial -> Ordering
monomialCompare Lex (Monomial m1) (Monomial m2) =
  compare (M.toAscList m1) (M.toAscList m2)

monomialCompare GrLex (Monomial m1) (Monomial m2) =
  let deg1 = sum (M.elems m1)
      deg2 = sum (M.elems m2)
  in compare deg1 deg2 <> compare (M.toAscList m1) (M.toAscList m2)

monomialCompare GrevLex (Monomial m1) (Monomial m2) =
  let deg1 = sum (M.elems m1)
      deg2 = sum (M.elems m2)
  in compare deg1 deg2 <> compare (reverse $ M.toDescList m1) (reverse $ M.toDescList m2)

-- REPL command
:set-order grevlex
```

### B. Improved Positivity Checking

**File:** `src/Prover.hs`

```haskell
checkPositivity :: Poly -> Bool -> (Bool, String)
checkPositivity p allowZero =
  -- Try methods in order of sophistication:

  -- Method 1: Constant check
  if isConstant p then
    let c = getConst p
    in (c > 0 || (allowZero && c == 0), "Constant evaluation")

  -- Method 2: Sturm's theorem (univariate)
  else case toUnivariate p of
    Just (var, coeffs) ->
      let nRoots = countRealRoots coeffs
          lc = last coeffs
      in (nRoots == 0 && lc > 0, "Sturm's theorem")

    Nothing ->
      -- Method 3: Sum of Squares decomposition
      case decomposeAsSOS p of
        Just sos -> (True, "Sum-of-Squares decomposition: " ++ show sos)
        Nothing ->
          -- Method 4: Sampling + interval arithmetic
          case samplingCheck p of
            Just True -> (True, "Sampling heuristic (1000 points)")
            _ -> (False, "Could not determine positivity")

-- Enhanced SOS decomposition
decomposeAsSOS :: Poly -> Maybe [(Poly, Rational)]
decomposeAsSOS p =
  -- Try to express p as Σ cᵢ·qᵢ² where cᵢ > 0
  -- Uses semidefinite programming or Gram matrix approach
  ...

-- Sampling with interval arithmetic
samplingCheck :: Poly -> Maybe Bool
samplingCheck p =
  let samples = generateSamples (extractVars p) 1000
      values = map (evaluateAtPoint p) samples
  in if all (> 0) values
     then Just True
     else if any (<= 0) values
          then Just False
          else Nothing  -- Inconclusive
```

### C. Robust Error Handling

**File:** `src/Expr.hs`, `src/Parser.hs`

```haskell
-- Replace error with Either
toPoly :: Expr -> Either String Poly
toPoly (Var x)     = Right $ polyFromVar x
toPoly (Const r)   = Right $ polyFromConst r
toPoly (Add e1 e2) = do
  p1 <- toPoly e1
  p2 <- toPoly e2
  return $ polyAdd p1 p2
toPoly (Div _ _)   = Left "Division is not supported in polynomial expressions"

-- Parser error handling
parseFormulaPrefix :: String -> Either ParseError Formula
parseFormulaPrefix input =
  case tokenizePrefix input of
    Left err -> Left $ "Tokenization error: " ++ err
    Right tokens -> case parseSExpr tokens of
      Left err -> Left $ "Parse error: " ++ err
      Right sexpr -> case formulaFromSExpr sexpr of
        Left err -> Left $ "Formula construction error: " ++ err
        Right formula -> Right formula
```

### D. WebAssembly Frontend

**Implementation Plan:**

```haskell
-- src/WebMain.hs (already exists but enhance it)

-- Add JavaScript FFI bindings
foreign export javascript "evaluateFormula"
  evaluateFormula :: JSString -> JSString

foreign export javascript "getProofTrace"
  getProofTrace :: JSString -> JSString

-- Compile with:
-- $ ghc --make -o hasclid.js -O2 -threaded WebMain.hs
-- $ asterius --bundle --input hasclid.hs --output hasclid.wasm
```

**Web Interface:**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Hasclid Online</title>
  <script src="hasclid.js"></script>
</head>
<body>
  <canvas id="geometry"></canvas>
  <textarea id="input"></textarea>
  <button onclick="prove()">Prove</button>
  <div id="output"></div>
</body>
</html>
```

---

## 5. COMPARISON WITH OTHER PROVERS

### Feature Matrix

| Feature | **Hasclid** | GeoGebra | Coq | Lean | MetiTarski | Z3 |
|---------|-------------|----------|-----|------|------------|-----|
| **Automated geometric proofs** | ✓ | Partial | Manual | Manual | ✓ | Partial |
| **Exact arithmetic** | ✓ | ✗ (float) | ✓ | ✓ | ✗ | ✓ |
| **Gröbner bases** | ✓ | ✗ | Manual | Manual | ✗ | ✗ |
| **Sturm's theorem** | ✓ | ✗ | Manual | Manual | ✓ | ✗ |
| **CAD** | Partial | ✗ | Manual | Manual | ✓ | Partial |
| **Quantifier elimination** | ✗ | ✗ | Manual | Manual | ✓ | ✓ |
| **User-friendly** | Medium | ✓✓ | ✗ | ✗ | ✗ | Medium |
| **Proof certificates** | ✗ | ✗ | ✓ | ✓ | ✓ | ✓ |
| **3D geometry** | Limited | ✓ | ✓ | ✓ | Limited | ✓ |
| **Interactive UI** | REPL | ✓✓ (GUI) | ✗ | ✗ | REPL | REPL |
| **Scripting** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Visualization** | ✗ | ✓✓ | Plugin | Plugin | ✗ | ✗ |
| **Open source** | ✓ | ✓ | ✓ | ✓ | ✓ | ✗ |

### Detailed Comparisons

#### vs. GeoGebra
**GeoGebra Advantages:**
- Beautiful GUI with interactive geometry
- Real-time dynamic updates
- Accessible to high school students

**Hasclid Advantages:**
- Exact proofs (not approximate)
- Supports symbolic coordinates
- Programmable/scriptable
- Formal proof traces

**Use Case Split:**
- GeoGebra: Exploration, education, visualization
- Hasclid: Formal verification, research, automated proving

#### vs. Coq/Lean
**Coq/Lean Advantages:**
- Complete formal verification
- Trusted proof certificates
- Rich type systems
- Large mathematical libraries

**Hasclid Advantages:**
- Fully automated (no tactics needed)
- Faster for geometric theorems
- No proof assistant expertise required

**Potential Integration:**
- Hasclid could generate Coq/Lean proof terms
- Use Hasclid as preprocessing for formal provers

#### vs. MetiTarski
**MetiTarski Focus:**
- Special functions (sin, cos, exp, log)
- Interval arithmetic
- Automated inequality proving

**Hasclid Focus:**
- Euclidean geometry
- Polynomial constraints
- Exact symbolic computation

**Complementary Strengths:**
- MetiTarski: Better for analysis (calculus, transcendentals)
- Hasclid: Better for pure geometry (triangles, circles)

#### vs. Z3 SMT Solver
**Z3 Advantages:**
- Industrial-strength
- Multiple theories (arrays, bitvectors, etc.)
- Highly optimized

**Hasclid Advantages:**
- Specialized for geometry
- Gröbner bases (Z3 doesn't have this)
- Educational/transparent (can see proof steps)

**Z3 Limitations for Geometry:**
- Non-linear arithmetic is incomplete in Z3
- No geometric primitives

---

## 6. CONCLUSION

### Summary of Capabilities

**Hasclid is a solid automated geometry theorem prover** with:

**Strong Points:**
- ✓ Robust Gröbner basis engine for equality proofs
- ✓ Sturm's theorem for univariate inequalities
- ✓ Exact rational arithmetic (no floating-point errors)
- ✓ Clean, modular Haskell codebase
- ✓ Interactive REPL with lemma libraries
- ✓ Proof tracing for transparency

**Unique Features:**
- ✓ LISP-based geometric DSL
- ✓ Combination of Gröbner + Sturm + CAD
- ✓ Scriptable theorem proving

**Promising but Incomplete:**
- ⚠ CAD implementation (projection only)
- ⚠ Multivariate inequality handling
- ⚠ Performance optimizations

### Main Limitations

1. **Incomplete inequality handling** - Only univariate Sturm, no full CAD lifting
2. **Performance bottlenecks** - Unoptimized Buchberger algorithm
3. **No non-degeneracy checks** - Can give false proofs for degenerate cases
4. **Coordinate-only geometry** - Cannot express synthetic constructions
5. **No proof certificates** - Cannot export for formal verification
6. **Limited 3D support** - Missing planes, volumes, 3D primitives

### Development Priorities

**Immediate (Priority 1):**
1. Complete CAD lifting phase
2. Optimize Buchberger (selection strategy, criteria)
3. Add non-degeneracy validation

**Short-term (Priority 2):**
4. Implement Wu's method as alternative engine
5. Add robust error handling
6. Support multiple term orderings

**Medium-term (Priority 3):**
7. Generate proof certificates (Coq/Lean export)
8. Add visualization (SVG/TikZ output)
9. Implement quantifier elimination

**Long-term (Priority 4):**
10. Build web interface (WASM compilation)
11. Add area method (Chou-Gao-Zhang)
12. Support geometric transformations
13. Integrate with formal proof assistants

### Target Audience

**Current Best Users:**
- Mathematics researchers studying algebraic geometry
- Educators teaching automated reasoning
- Students learning Gröbner bases/CAD
- Hobbyists interested in automated theorem proving

**Potential Future Users (with improvements):**
- Formal verification engineers (with proof certificates)
- Geometry software developers (as a library)
- High school math teachers (with GUI + visualization)
- Robotics researchers (geometric constraint solving)

### Comparison to State-of-the-Art

**Academic Level:** Research prototype approaching production quality

**Strengths vs. Competition:**
- More specialized than general SMT solvers (Z3)
- More automated than proof assistants (Coq/Lean)
- More rigorous than numeric tools (GeoGebra)

**Gaps vs. State-of-the-Art:**
- Less optimized than industrial Gröbner basis tools (Singular, Macaulay2)
- Less complete than full CAD implementations (QEPCAD)
- Less user-friendly than interactive geometry software

### Final Assessment

**Rating: 7.5/10** for automated geometric theorem proving

**Breakdown:**
- Theory/Correctness: 9/10 (sound but incomplete)
- Implementation Quality: 7/10 (clean but unoptimized)
- Feature Completeness: 6/10 (good basics, missing advanced)
- Usability: 7/10 (REPL is nice, but needs GUI)
- Performance: 5/10 (works but slow on large problems)
- Documentation: 8/10 (good tutorials and references)

**Recommendation:**
With the proposed improvements (especially completing CAD and optimizing Buchberger), Hasclid could become a **top-tier automated geometry prover** suitable for both research and education. The clean codebase and solid theoretical foundation make it an excellent platform for further development.

---

## References & Further Reading

### Gröbner Bases
- Cox, Little, O'Shea: "Ideals, Varieties, and Algorithms" (textbook)
- Buchberger: "An Algorithm for Finding the Basis Elements of the Residue Class Ring of a Zero Dimensional Polynomial Ideal" (1965)
- Faugère: "A new efficient algorithm for computing Gröbner bases (F4)" (1999)

### CAD & Quantifier Elimination
- Collins: "Quantifier elimination for real closed fields by cylindrical algebraic decomposition" (1975)
- Brown: "QEPCAD B: A system for computing with semi-algebraic sets via CAD" (2003)

### Geometry Theorem Proving
- Wu Wen-tsün: "Basic principles of mechanical theorem proving in elementary geometries" (1986)
- Chou, Gao, Zhang: "Machine Proofs in Geometry" (1994)
- Kapur: "Using Gröbner Bases to Reason About Geometry Problems" (1986)

### Sturm's Theorem
- Sturm: "Mémoire sur la résolution des équations numériques" (1829)
- Basu, Pollack, Roy: "Algorithms in Real Algebraic Geometry" (2006)

### Related Tools
- **QEPCAD B**: http://www.usna.edu/CS/qepcadweb/B/QEPCAD.html
- **Singular**: https://www.singular.uni-kl.de/
- **GeoGebra**: https://www.geogebra.org/
- **Coq**: https://coq.inria.fr/

---

**Document Version:** 1.0
**Last Updated:** December 2, 2025
**Author:** AI Analysis of Hasclid v7.3 Codebase
