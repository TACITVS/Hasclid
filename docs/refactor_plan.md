# Refactor Plan (Idiomatic Haskell + Maintainability)

Goal: make the codebase more idiomatic, testable, and modular without changing behavior. This plan is staged so future agents can resume work safely.

## Ground Rules
- Do not change user-facing commands or behavior while refactoring.
- Keep core solvers pure; push IO/printing/state to the edges.
- Use git locally for each logical change (small commits). Do not amend existing commits unless necessary.
- Add tests/benchmarks before risky refactors.

## Phase 0: Baseline & Safety
- [ ] Add minimal property tests (QuickCheck/hedgehog) and a tiny regression suite:
  - GB reduction: normal form of f mod G is 0 for constraints that imply f=0.
  - toPoly vs evaluation agreement on simple expressions.
  - Positivity check consistency on known positive/negative polys.
  - Integer solver sanity on small linear systems.
- [ ] Add criterion or simple timing harness for representative problems (GB, CAD 1D/2D, integer, Wu) to detect regressions.
- [ ] Wire a simple `make test`/`cabal test` target if absent.

## Phase 1: Effect Structure & Configuration
- [x] Introduce a `REPLEnv` and wrap REPL command processing in `ReaderT Env (ExceptT ProverError IO)` (placeholder env now has concrete fields; still need full threading).
- [ ] Fill `REPLEnv` with real config (term order, strategy, flags) in use (threaded through commands), and keep solvers pure; only REPL/script loading uses IO.  
      (Progress: `initialState` now derives its defaults from `REPLEnv`; REPL command processing pulls env via `ReaderT` and seeds resets/auto-solve from it.)
- [ ] Split mutable bits (state) cleanly from env.
- [ ] Remove ad-hoc error strings: use `ProverError`/`ParseErrorType` consistently in the command layer.
- [ ] Unify solver options: single immutable `SolverOptions` threaded through router/REPL; cache/state held separately.

## Phase 2: Module Boundaries
- [ ] Split `Prover.hs` into focused modules (keeping public API stable):
  - `Core.GB` (Buchberger ops/reduction/SPoly),
  - `Core.Positivity`,
  - `Core.Integer` (interval/branch/diophantine),
  - `Core.CAD` (proof/quantifier entry points),
  - `Core.Wu`,
  - `Core.Trace` (ProofTrace formatting),
  - `Core.Quantifier` (helpers for bounded/negation strategies),
  - `Core.Subst` (buildSubMap/evaluate).
- [ ] Make exports explicit; keep `Main`/`WebMain` imports narrow.
  - TODO (next): carve out `Core.GB` from the reduction/SPoly code in `Prover` and `BuchbergerOpt`, keep API shim in `Prover` to avoid breaking callers.

## Phase 3: Types & Totality
- [ ] Introduce newtypes for identifiers (e.g., `Var`, `Point`, `MacroName`) to reduce stringly-typed errors.
- [ ] Add smart constructors for goals and quantified formulas to avoid partial matches.
- [ ] Remove partial functions/patterns; return `Either`/`Maybe` as needed. Handle Wu/CAD “Eq-only” constraints via types.  
      (Progress: Router promotion helper now covers And/Or/Not/default; added missing solver-choice case.)
      Next: continue eliminating redundant catch-alls that cause overlapping-pattern warnings.
      Note: Replaced `head` fallback in constructive existence path with safe pattern match in Prover.
  - TODO (next): audit `Prover`/`CADLift` integer/interval branches for partial pattern matches and convert to total or explicit error paths.
- [ ] Replace scattered booleans with sum types where clearer (e.g., solver mode, term order selection).

## Phase 4: Parsing & Macros
- [ ] Wrap the S-expression parser in parser-combinator style for clearer errors, or at least centralize arity/syntax checks.
- [ ] Keep macro expansion pure; add depth-limit error handling instead of `error`.

## Phase 5: Tracing & Logging
- [ ] Normalize tracing to structured data across Wu/GB/CAD/Positivity/Integer; formatting only at the boundary (REPL/Web).
- [ ] Add lightweight logging hooks (pure traces or Writer) for profiling without IO in core code.

## Phase 6: Benchmarks & Performance Validation
- [ ] Add benchmarks for: (a) representative GB instances, (b) CAD 1D/2D inequalities, (c) Wu geometric proofs, (d) integer solver branch cases.
- [ ] Compare “optimized” vs “baseline” Buchberger strategies to justify defaults; keep flags but pick sane defaults after data.

## Phase 7: Cleanup & Docs
- [ ] Apply hlint/stylish-haskell on touched files.
- [ ] Document invariants and edge cases in haddocks (esp. solver entry points).
- [ ] Update `README`/`docs` with configuration flags and recommended workflows.

## Notes on Git Workflow
- Work on a feature branch (e.g., `refactor/idiomatic-core`).
- Small commits per logical step (tests, type changes, module splits).
- If a refactor is risky, land tests first, then the refactor, then a follow-up fix if needed.
- Keep CI/test instructions simple (`cabal test`, `cabal run prover` smoke).

## Quick Start for Next Agent
- Start with Phase 0 tests to lock behavior.
- Then do Phase 1 effect layering around REPL/router without touching solver internals.
- Proceed to Phase 2 module splits once tests pass.
