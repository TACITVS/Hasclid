# Hasclid Geometry Completion Program
**Generated:** 2025-12-28 12:00 UTC

This document outlines the roadmap to achieve "complete" coverage of Euclidean Geometry in Hasclid, addressing the remaining gaps in logical soundness, metric reasoning, and locus generation.

Prioritized by complexity (Ascending).

---

## 1. Genericity Checker (Low Complexity)
**Goal:** Ensure geometric constructions are valid (e.g., verifying lines intersect before defining an intersection point). Algebraic solvers often prove theorems true "generically" even if the specific configuration is impossible (e.g., parallel lines).

- [x] **1.1 Denominator Verification**: Enhance `AreaMethod.hs` to collect denominators generated during elimination. Use `Modular.probSolve` to check if they can be zero.
- [x] **1.2 `:check-construction` Command**: Add a REPL command that validates the current geometric construction for degeneracy.
- [x] **1.3 Integration**: Automatically run genericity checks before attempting algebraic proofs in `:prove`.

## 2. Metric Space Solver (Medium Complexity)
**Goal:** Efficiently prove triangle inequalities ($AB + BC \ge AC$) without expensive algebraic expansion.

- [x] **2.1 Metric Graph**: Implement a `src/Solvers/Metric.hs` module that builds a graph where nodes are points and edges are distance expressions.
- [x] **2.2 Shortest Path Logic**: Implement a solver that checks if a target distance inequality follows from triangle inequality axioms (shortest path algorithms).
- [x] **2.3 Integration**: Wire into `SolverRouter` to handle `Ge (Add (Dist A B) (Dist B C)) (Dist A C)` patterns instantly.

## 3. Locus Generation (High Complexity)
**Goal:** Derive the algebraic equation describing a geometric locus (e.g., "The set of points equidistant from A and B").

- [x] **3.1 Elimination Engine**: Create `src/Locus.hs` wrapping Gröbner Basis functionality to perform variable elimination orderings ($Parameters > LocusPoint > ConstructedVars$).
- [x] **3.2 `:locus` Command**: Add `:locus P <formula>` to the REPL, which outputting the polynomial equation satisfied by point $P$.
- [x] **3.3 Output Formatting**: Pretty-print the resulting polynomial (e.g., "Result: x^2 + y^2 - 1 = 0").

## 4. Angle Canonizer (Very High Complexity)
**Goal:** Robustly handle general angle sums and differences ($\angle ABC + \angle DEF = 90^\circ$), which require transcendental reasoning beyond simple trig polynomialization.

- [x] **4.1 Complex Number Mapping**: Implement translation of points to complex numbers ($z = x + iy$) and angles to unit complex numbers ($e^{i\theta}$).
- [x] **4.2 Rewrite Engine**: Implement "Geometry of Directions" rewrite rules to linearize angle expressions.
- [x] **4.3 Integration**: Map complex number equations back to real polynomials for the algebraic solvers.

---

**Next Step:** All Phases Complete.
