# Idiomatic Refactor TODO

Working log for the idiomatic Haskell refactor. Keep this file updated so the next pass picks up smoothly.

## Completed
- Rewritten `fallThrough` in `src/Prover.hs` to use guarded equations and share the Groebner/CAD flow without dangling `else` blocks.
- Ensured `Prover`’s Groebner path reuses cached bases and builds traces consistently with other solver paths.

## Next Steps
- Run `cabal build` and `cabal test` to confirm the new `fallThrough` compiles cleanly and to surface any remaining `-Wall` warnings (notably in `Prover.hs` for incomplete patterns or defaulting).
- If warnings remain, address them directly (prefer filling patterns and adding explicit types over suppressing warnings).
- Consider deduplicating the common Groebner/CAD fallback logic between `proveTheoryWithCache` and `proveTheoryWithOptions` to reduce drift.
- Once warning-free, rerun the example scripts to ensure behavior is unchanged.

