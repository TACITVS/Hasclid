# Quantifier/CAD Progress — 2025-12-04

## Current State
- **Parsing**: Quantifier binders support bounded forms `(name lo hi)`, `(int name lo hi)`, typed `(real name [lo hi])`, and unbounded real binders `(name)` / `(real name)`.
- **Integer quantifiers**: Pure ∀/∃ integers routed to the integer solver; negation-refutation for ∀; satisfiability for ∃. Mixed domains still unsupported.
- **Real universals**:
  - Bounded (all vars have bounds): fast linear/univariate path, otherwise CAD box check.
  - Unbounded/pure real polynomial goals: CAD refutation of negation (∀ proved if negation is unsat via CAD).
  - Quantifiers in assumptions are still rejected.
- **Real existentials**:
  - Pure real polynomial goals (bounded or unbounded): CAD satisfiability (finds a satisfying cell).
  - Mixed domains/non-polynomial (sqrt/div/int) → unsupported.
- **Routing**:
  - Real ∀/∃ now routed to the prover/CAD path (bounded and unbounded) instead of “quantifiers unsupported”.
  - Bounded real ∀ still uses the earlier “linear interval/CAD box” path first.
- **Examples**:
  - `examples/quantifier_stress.euclid` (ints) still OK.
  - `examples/quantifier_reals_stress.euclid` (bounded reals) OK.
  - `examples/quantifier_reals_unbounded.euclid` added for unbounded reals; CAD refutes/accepts as expected.
- **Known gaps**:
  - No full QE for alternation (∀∃ over reals) or nested quantifiers; only single-quantifier class goals are handled.
  - Quantified assumptions still rejected.
  - Geometry goals with alternation (e.g., “∀ non-collinear triple ∃ circle…”) not yet supported.

## Recent Fixes
- Added `negateRealGoal` for CAD refutation.
- Parser accepts unbounded real binders `(x)`.
- Router dispatches real ∀/∃ to CAD/prover instead of “unsupported”.
- Multivariate bounded real ∀ handled via CAD box; real ∃ via CAD satisfiability.
- Crash on non-exhaustive pattern (bounded reals) fixed; layout issues resolved.

## Next Steps (not implemented)
1) Full real QE for mixed ∀/∃ (projection/CAD) to handle alternation.
2) Allow quantified assumptions (pure real) by running QE over theory+goal or instantiation policy.
3) Geometry theorem encoding (circle through three non-collinear points) once alternation/QE is in place.
4) Broaden real support to non-polynomial (sqrt/div) if needed, via elimination or fallback.
