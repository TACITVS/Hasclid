# Idiomatic Refactor TODO

Working log for the idiomatic Haskell refactor. Keep this file updated so the next pass picks up smoothly.

## Completed
- Rewritten `fallThrough` in `src/Prover.hs` to use guarded equations and share the Groebner/CAD flow without dangling `else` blocks.
- Ensured `Prover`’s Groebner path reuses cached bases and builds traces consistently with other solver paths.
- Removed the partial `head` fallback in `src/Wu.hs`; now safely handles empty branch lists.
- Replaced `head` on point coordinates in `src/WebMain.hs` with total pattern matching.
- Deduplicated the Groebner fallback logic into `groebnerFallback` to keep cache/custom Buchberger paths in sync.

## Next Steps
- Run `cabal build` and `cabal test` after each change to surface any new `-Wall` issues.
- Continue replacing partial patterns (`head`/incomplete matches) in remaining modules; lift routing logic into shared helpers where possible.
- Consider deduplicating the common Groebner/CAD fallback logic between `proveTheoryWithCache` and `proveTheoryWithOptions` to reduce drift.
- Rerun example scripts to ensure behavior remains unchanged after refactors.
