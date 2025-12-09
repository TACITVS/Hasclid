# Idiomatic Refactor Plan

We’re on branch `refactor-webmain-cleanup` (not pushed). This plan is for eliminating remaining incomplete patterns and warning suppressions across modules. One PR per milestone.

## Targets
1) Remove latent incomplete patterns by making matches total in:
   - `GeoSolver`
   - `Positivity`
   - `CADLift`
   - `Validation`
   - `Prover` (remaining partial matches)
   - `SolverRouter` (if any leftovers)
2) Audit numeric defaulting/shadowing and add minimal type annotations where defaults arise.
3) Keep side effects isolated (WebMain already removed); ensure no `unsafePerformIO` remains.

## Milestone 1 (this branch)
- Make the above modules total; run `cabal build` and `cabal test`.
- Update `IDIOMATIC_REFACTOR_TODO.md` with completed items.
- Stage/commit to a dedicated branch for this milestone (when allowed).

## Notes
- Current working branch contains:
  - `SqrtElim` totalized.
  - `ProblemAnalyzer` totalized.
  - `WebMain` removed.
