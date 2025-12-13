# Roadmap to Proving Erdős-Mordell (Optimized)

**Goal**: Prove the Erdős-Mordell Theorem (or its component lemmas) in < 5 minutes.
**Current Status**: F4/Groebner + SOS heuristics implemented but timing out (> 60s).

## 1. Current Architecture & Bottlenecks

### Architecture
- **Router**: `UseGeoSolver` (Area Method) -> `UseWu` -> `UseGroebner` -> `UseCAD`.
- **Inequalities**: `proveGeometricInequality` uses a pipeline:
    1.  **WLOG**: Canonicalizes coordinates (already done manually in current examples).
    2.  **Smart Squaring**: Converts $A \ge B$ to $A^2 \ge B^2$ (implemented).
    3.  **F4 Reduction**: Reduces target polynomial modulo equality constraints (implemented).
    4.  **SOS Check**: Greedy Rational Cholesky decomposition (implemented).

### Bottlenecks
- **SOS Strength**: The greedy Cholesky algorithm (`checkSumOfSquares`) is incomplete. It fails if the polynomial is SOS but the "pivot" terms are not chosen perfectly or if terms need to be split.
- **Polynomial Size**: Reducing high-degree geometric polynomials (especially with distance variables $R_b$) creates massive expressions.
- **Term Ordering**: The default `GrevLex` might not be optimal for the specific variable hierarchy of Erdős-Mordell ($x, y$ vs parameters).

## 2. Optimization Plan (The "5-Minute" Path)

### A. Enhance Sum-of-Squares (Priority: High)
**Problem**: Greedy Cholesky is fragile.
**Solution**: Implement a stronger SOS heuristic.
1.  **Cross-Term Search**: Instead of just dividing by the leading term's root, explicitly search for $2ab$ terms that match available squares.
2.  **Numerical Guidance**: Use `Double` precision Cholesky to *find* the decomposition coefficients first, then reconstruct exact Rational coefficients (Rational Reconstruction). This avoids massive intermediate expression swell during the search.
3.  **Semidefinite Programming (SDP) Lite**: Implement a specialized solver for small Gram matrices (up to 6x6) which covers most geometry problems.

### B. Optimize Term Ordering (Priority: Medium)
**Problem**: Elimination order affects reduction size.
**Solution**:
1.  **Block Ordering**: Ensure variables $\{x, y\}$ are always greater than parameters $\{s, R, \dots\}$.
2.  **Dynamic Selection**: Try both `Lex` (for triangularization) and `GrevLex` (for size) during the reduction phase.

### C. Algebraic Simplifications (Priority: Medium)
**Problem**: $R_b = \sqrt{\dots}$ involves radicals.
**Solution**:
1.  **Radical Ideal Injection**: Explicitly add $R_b^2 - (\dots) = 0$ to the ideal (already done in input).
2.  **Weighted Sums**: The theorem is $\sum R \ge 2 \sum d$. Proving the sum might be easier due to symmetry than proving individual components $R_b \ge d_a + d_c$.
    - *Action*: Create `erdos_mordell_full_sum.euclid` to test if symmetry cancels out complex terms.

### D. Area Method Expansion (Priority: Low)
**Problem**: Area method is equality-only.
**Solution**:
1.  **Inequality Extensions**: Some literature exists on extending Area Method to inequalities (Yang et al.).
2.  **Bridge**: Use Area Method to prove intermediate equalities, simplifying the job for F4/SOS.

## 3. Concrete TODO List

1.  [ ] **Run with Multithreading**: Ensure `bench_f4.exe` is run with `+RTS -N`.
2.  [ ] **Test Full Sum**: Create a benchmark for the full symmetric inequality.
3.  [ ] **Numerical SOS Prototype**: Implement a quick-fail check using Floats to see if SOS is even *possible* for the generated polynomial (sanity check).
4.  [ ] **Term Order Tuning**: Expose term order selection in CLI options.

## 4. Benchmark Targets

| Problem | Goal Time | Current |
|---------|-----------|---------|
| `lemma_Rb` (Component) | < 60s | Timeout |
| `full_sum` (Symmetric) | < 300s | N/A |

---
**Strategy**: If symbolic SOS continues to fail, the "Numerical Guidance" approach (float Cholesky -> rational reconstruction) is the highest-reward path to solving this within 5 minutes.
