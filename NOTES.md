# Onboarding Notes

## Architecture Overview
- REPL entry in `src/Main.hs` orchestrates theory state, lemma handling, solver configuration, and command parsing. Automatic routing is handled by `src/SolverRouter.hs`, which stages `GeoSolver` → Wu/Groebner/CAD/Int as needed.
- Core algebra lives in `src/Expr.hs` (AST, polynomial ops, simplification, pretty-printing) plus `src/BuchbergerOpt.hs`/`src/Core/GB.hs` (Groebner), `src/CAD*.hs` (projection and lifting), `src/Sturm.hs` (univariate root analysis), and `src/Positivity.hs`.
- Rational and sqrt preprocessing (`src/RationalElim.hs`, `src/SqrtElim.hs`) feed CAD when divisions or radicals appear. Integer reasoning is embedded in `src/Prover.hs` with interval and diophantine heuristics; probabilistic modular checks live in `src/Modular.hs`.
- Geometry fast-paths: `src/GeoSolver.hs` propagates symbolic constraints; `src/Wu.hs` provides characteristic-set proofs. `src/ProblemAnalyzer.hs` supplies profiling data to the router.
- Minimal tests in `test/Spec.hs` cover polynomial conversion, reduction, positivity detection, and small integer solving. Docs are under `docs/` with tutorial/reference/grammar; numerous analysis/result logs live at repo root.

## Potential Issues / Risks
- REPL macro plumbing is unfinished: `REPLState` carries a `macros` map, but there is no command to define macros, and `:solve`/`:load` paths call `parseFormulaPrefix` (no macros) instead of the macro-aware parsers. Script execution therefore cannot expand macros, and interactive macro definition is impossible.
- Compiled artifacts (`*.o`, `*.hi`, `nul`) are committed under `src/`; they bloat the repo and risk stale code being mistaken for sources.
- Version/branding drift: README advertises v9.1, while `Main` banner still prints v9.0.
- Encoding artifacts remain in user-facing strings (“Gr”bner”, odd angle symbols) across README and some printers; this can leak into logs and JSON/FFI consumers.
- CAD/Wu/GB routing handles integer goals via `intSolve`, but the integer solver is still heuristic (interval propagation + small search) and surfaces “incomplete” states frequently; no higher-level guidance or timeouts are exposed to users.
- GeoSolver distance/perpendicular propagation only fires when exactly one coordinate is unknown; multi-unknown branches fall back silently, so users may assume more inference than is actually done. No degeneracy warnings are raised when distance equations are unsolvable.
- `:solve` and script processing ignore verbose/trace toggles when a timeout bump occurs; users only see the final rerun message, not partial progress.

## Improvement Ideas
1) Complete macro support: add REPL commands to define/list/clear macros and switch `:solve`/`:load`/script parsing to the macro-aware functions so batch runs behave like interactive use. Include tests for macro expansion in scripts.
2) Clean repository hygiene: drop generated `.o/.hi`/`nul` files, enforce `.gitignore`, and add a CI check to prevent compiled artifacts from reappearing.
3) Align versioning and messaging: drive banner/version strings from a single constant and normalize wording/encoding (“Gröbner”, angle symbols) in user-visible output.
4) Harden integer solver UX: surface whether fallback brute-force was considered, allow per-call bounds/timeouts, and emit clearer guidance when results are “incomplete”. Add regression tests that cover linear/mixed integer cases.
5) Expand GeoSolver diagnostics: emit branch/degeneracy warnings when distance/perpendicular propagation cannot solve (or produces zero/negative radicands), and document that only single-unknown distance equations are handled.
6) Broaden CAD preprocessing coverage: unify rational/sqrt elimination paths in router to avoid duplicated logic and ensure inequality goals with radicals/divisions always get the same transformations. Add focused tests for CAD flows with sqrt + division combos.
7) Testing depth: grow QuickCheck coverage for parser round-trips (including macros and quantifiers), polynomial pretty-print stability, and router selection logic under different profiles.

