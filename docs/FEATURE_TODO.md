# Hasclid Feature TODO (Verified Against Current Codebase)

This list reflects what is currently missing or only partially implemented, after checking the code.

## Missing
- Proof certificates/export: No facility to emit proofs (Groebner/CAD/Wu) to Coq/Lean/Isabelle or similar.
- Infix/friendlier syntax: Parser remains prefix-only; no infix grammar or dual-mode input.
- Advanced geometry/UX: No geometric transformations, area method, rich metric-angle support beyond current angle equality helpers, visualization, or WebAssembly/frontend.
- Proof search/tactics: No tactic engine or interactive proof assistant mode; only REPL commands.
- Parallelism and incremental Groebner/CAD: No parallel execution paths or incremental basis updates for added assumptions.
- Constraint solver/constructive geometry commands: No CLI commands to synthesize configurations or constructive solutions.

## Partial
- CAD completeness: CAD with lifting/sign assignment exists, but not a full Collins-level, higher-dimensional QE with guarantees.
- Positivity/SOS: Enhanced in v9.4 with robust nth-root handling, coefficient tracking, and smart inequality squaring. SDP solver available for complex cases. Room for improvement with interval arithmetic.
- Non-degeneracy checks: Coincident/zero-length checks are active; collinearity and broader degeneracy checks remain disabled to avoid false positives.

## Notes
- Term ordering control is already present (Lex/GrLex/GrevLex with REPL support), so no action required there aside from potential integration audits.

## Completed (Recent)
- **Trigonometric Support**: Full support for `sin`, `cos`, `tan`, `asin`, `acos`, `atan` in the expression language, with implicit `sqrt` recognition and algebraic simplification.
- **Variable Centralization**: Refactored `varsInExpr` and `varsInFormula` to `Core/Types.hs` for cleaner architecture.

## Prioritized TODO (2025-12-05)
- **Constraint solving / constructive mode** (easy, high value): broaden witness/counterexample search (wider ranges, simple linear solving, better heuristics).
- **Non-degeneracy filters** (easy–moderate, high safety): richer degeneracy checks (collinear/coplanar/rank conditions) with low false positives.
- **Positivity/SOS stack** (moderate, high coverage): add interval arithmetic + SOS/SDP fallback and clearer heuristic ordering.
- **Parallel/incremental solvers** (moderate–high, performance): parallel S-polynomials/CAD tasks; incremental Groebner updates for added assumptions.
- **Optimized Groebner/CAD heuristics** (moderate–high, core speed): better Buchberger criteria/F4-style improvements; smarter CAD projection/lifting heuristics.
- **Advanced geometry methods** (high, medium impact): area method, richer angle/metric primitives, optional geometric transforms.
- **Proof certificates/export** (high, niche but important): emit verifiable certificates (Coq/Lean/Isabelle) for Groebner/CAD/Wu results.
- **Visualization/UX polish** (high, medium UX): diagrams/plots, optional frontend hooks; keep pure backend unaffected.
