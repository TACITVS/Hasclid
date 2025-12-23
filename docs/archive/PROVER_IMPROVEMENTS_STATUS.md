# Prover Code Improvements - Status Report (Updated Dec 22, 2025)

## New Feature: Arbitrary Precision Numeric SDP Solver

**Goal**: Prove hard geometric inequalities (e.g., Ono's Inequality) using Sum-of-Squares (SOS) relaxation.

**Achievement**:
- Implemented a **Primal-Dual Interior Point Method (IPM)** solver in pure Haskell (`src/Positivity/SDP.hs`).
- Integrated `Data.Number.BigFloat` for arbitrary precision arithmetic (Prec50 ~ 166 bits).
- Implemented **Cholesky Decomposition** and dense matrix operations.
- Added **Constrained SOS** support (Putinar's Positivstellensatz).

**Current Status**:
- **Infrastructure**: COMPLETE & ROBUST. The solver runs without crashing and correctly integrates with the `Prover` pipeline.
- **Ono's Inequality**: **NOT PROVED**.
    - The formulation (Degree 12) requires higher-degree multipliers than the current default configuration allows.
    - Boundary conditions (equality at equilateral triangle) cause singularity in IPM, requiring sophisticated handling (or higher precision).
    - Multiple formulations (Standard, Smart, Ravi, Tangent) were tested; all hit computational limits (timeout or infeasibility).
- **Verdict**: The system correctly identifies that it cannot prove the theorem within reasonable resources, rather than hallucinating a proof or crashing.

## Improvements Made

### 1. Sqrt Elimination Enhancement (src/SqrtElim.hs)
... (Same as before)

### 2. Solver Routing Enhancement (src/SolverRouter.hs)
... (Same as before)

### 3. Numeric SDP Solver (src/Positivity/SDP.hs) **[NEW]**
- **Algorithm**: Mehrotra Predictor-Corrector Primal-Dual IPM.
- **Precision**: Configurable `BigFloat` (default Prec50).
- **Capability**: Can solving SDPs with thousands of constraints (in principle), limited by dense matrix operations ($O(m n^3)$).
- **Integration**: Accessed via `checkPositivityWithFallback` in `Prover.hs`.

## Current Status

### ✅ Working
...
- **Numeric SOS**: Simple inequalities `x^2+y^2 >= 0` are provable.

### ❌ Still Failing
- **Ono's Inequality**: Requires Degree-10+ relaxation.
- **Triangle Inequality**: Symbolic complexity.

## Recommendations for Future
1.  **Sparse Matrix Support**: The current dense matrix implementation limits the scale of SDPs. Sparse Cholesky is needed for higher-degree relaxations.
2.  **Symmetry Reduction**: Exploiting $S_3$ symmetry in Ono's inequality could reduce the problem size by factor 6.
3.  **Hybrid Symbolic-Numeric**: Use the numeric solution as a guide to find exact rational SOS certificates (Rational Recovery).