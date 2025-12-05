# Stress Plan Progress - 2025-12-03

We will address the remaining “NOT PROVED” cases in ascending order of complexity, updating this file after each milestone.

## Milestones (ordered)
1. **Regression tests for equality cases (expected to prove with GeoSolver/Groebner):**
   - Collinear symmetry with numeric points (A,B,C collinear ⇒ B,C,A collinear).
   - Perpendicular with numeric coordinates (OX ⟂ OY where X=(1,0), Y=(0,1)).
   - Parallel axis-aligned rectangle (A=(0,0), B=(1,0), C=(0,1), D=(1,1): AB ∥ CD).
   - Perpendicular completion in rectangle (AB ⟂ BC, BC ⟂ CD ⇒ CD ⟂ DA with rectangle coords).

2. **Routing tweak for geometric equalities:** Prefer GeoSolver or Groebner when hypotheses are empty or coordinates are numeric; avoid Wu for those simple cases.

3. **Simple inequality support:** Use `checkPositivityEnhanced`/Sturm for univariate inequalities; use CAD for geometric inequalities and low-degree bivariate algebraic ones; allow non-strict roots when checking ≥.

4. **Broader CAD integration:** Replace “not yet fully integrated” with best-effort handling and clear limits for mixed/higher-degree inequalities.

## Status
- 2025-12-03: Plan created.
- 2025-12-03: Milestone 1 tests added and passing with Groebner (router updated); removed :q from scripts to avoid parse warnings on :load.
- 2025-12-03: Milestone 2 routing tweak applied (small geometric equalities now prefer Groebner, avoiding Wu); milestone marked complete.
- 2025-12-03: Milestone 3 complete: router now sends geometric inequalities to CAD; univariate handled via enhanced positivity (Sturm, allowing zero roots for ≥); 2D bivariate uses CAD. Regression stress suite now proves the earlier failing inequalities (y=x^2+1>0, x^2+y^2>=0, x^2>20, x^2>=0); multivariate/high-degree inequalities remain explicitly unsupported (deferred to milestone 4).
- 2025-12-04: Added 2D angle equality support (oriented `angle=` and reflection-tolerant `angle=abs`); numeric fast-paths substitute coordinates to avoid heavy Groebner when points are concrete. Script loading streams output per line; timeouts auto-retry once with bumped limit. `stress_angles.euclid` first case proves; second case (abs-angle) now short-circuits when numeric coords are present. Cache/lemma soft-reset added for reuse.
