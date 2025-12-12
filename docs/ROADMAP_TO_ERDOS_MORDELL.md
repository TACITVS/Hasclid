# Roadmap to Proving Erdős-Mordell in < 30s

This plan outlines the specific engineering steps required to upgrade HASCLID from its current state (capable of algebraic equalities via F4) to a system capable of proving complex geometric inequalities like the General Erdős-Mordell theorem efficiently.

## 1. Automated Symmetry Breaking (WLOG)
**Problem:** The general case involves 6-8 variables (3 vertices + point P). This is too high for algebraic solvers.
**Solution:** Implement "Without Loss of Generality" (WLOG) coordinate fixing.

- [ ] **Implement `Geometry.WLOG` module**
    - [ ] Detect if problem is translation-invariant (depends only on differences).
    - [ ] Detect if problem is rotation-invariant (depends only on dot products/distances).
    - [ ] Detect if problem is scale-invariant (homogeneous inequalities).
- [ ] **Implement `CoordinateGauge` strategy**
    - [ ] If invariant: Fix $A = (0,0)$.
    - [ ] If rotation-invariant: Fix $B = (x_B, 0)$ (align with x-axis).
    - [ ] If scale-invariant: Fix $x_B = 1$ (or $c=1$).
- [ ] **Integrate into `ProblemAnalyzer`**
    - [ ] Apply WLOG transformations *before* passing constraints to the algebraic solver.
    - [ ] **Expected Impact:** Reduces variable count from ~8 to ~3-4 (e.g., $x_C, y_C, x_P, y_P$).

## 2. Hybrid F4-Inequality Reduction
**Problem:** Currently, F4 is used for equalities, and CAD is used for inequalities. They don't talk to each other.
**Solution:** Use F4 to simplify the inequality expression modulo the geometric constraints.

- [ ] **Implement `F4.reducePoly`**
    - [ ] Expose a function that takes a `Theory` (equalities) and a `Poly` (from the inequality LHS - RHS).
    - [ ] Compute the Groebner Basis $G$ of the Theory using F4.
    - [ ] Compute the Normal Form $P_{reduced} = \text{reduce}(P_{ineq}, G)$.
- [ ] **Benefit:**
    - [ ] $P_{reduced}$ will be free of dependent variables (e.g., side lengths expressible via coordinates).
    - [ ] This "unmasks" the true polynomial that needs to be non-negative.

## 3. Fast Sum-of-Squares (SOS) Solver
**Problem:** Proving $P_{reduced} \ge 0$ via CAD is slow (exponential).
**Solution:** Implement a dedicated SOS checker. Most geometric inequalities are SOS.

- [ ] **Implement `Positivity.SOS` module**
    - [ ] **Gram Matrix Method:** Represent $P(x)$ as $v(x)^T Q v(x)$ where $v(x)$ is a vector of monomials.
    - [ ] **SDP-Lite:** Implement a lightweight solver (e.g., LDL decomposition or simple semidefinite check) to find if a positive semidefinite $Q$ exists.
    - [ ] **Fallback:** If generic SOS fails, check for "Weighted SOS" (sum of products of squares).
- [ ] **Integrate with F4:**
    - [ ] Verify $P_{reduced} = \sum f_i^2$ purely algebraically.
    - [ ] **Target Speed:** < 100ms for degree 4-6 polynomials.

## 4. Pipeline Integration (The "Fast Proof" Path)
**Problem:** `SolverRouter` currently picks either Wu, Groebner, or CAD.
**Solution:** Create a new pipeline for "Geometric Inequalities".

- [ ] **Update `SolverRouter.hs`**
    - [ ] Detect `ProblemType == Geometric` AND `Goal == Inequality`.
    - [ ] Route to new function `proveGeometricInequality`:
        1.  Apply **WLOG** (Step 1).
        2.  **Reduce** inequality polynomial using F4 (Step 2).
        3.  Check **SOS** on the reduced polynomial (Step 3).
        4.  Fallback to CAD only if SOS fails.

## 5. Benchmarking & Tuning
- [ ] **Create `bench/erdos_mordell_general_wlog.euclid`**
    - [ ] Verify the pipeline handles the symbolic coordinates automatically.
- [ ] **Stress Test**
    - [ ] Test with other hard inequalities (e.g., Euler's, Weitzenböck's).
    - [ ] Tuning F4 batch size and SOS heuristics.

---

**Estimated Effort:** ~3-5 days of coding.
**Expected Result:** Erdős-Mordell proof in < 5 seconds.
