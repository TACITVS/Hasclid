# Quantifier & Geometry Proof TODOs (ascending difficulty)

1. **Univariate real ∀ (non-linear)**  
   - Add bounded real universal handler for single-variable polynomials using Sturm root isolation and sign checks (>= and >).  
   - Keep clean fallback: return “unsupported” on failure so router can escalate.

2. **Real ∀ with CAD fallback**  
   - Route unsupported real universals to a 1-D/low-D CAD pass before giving up.  
   - Preserve informative messages when CAD is skipped or inconclusive.

3. **Real ∃ (multi-var) enablement**  
   - Support existential reals in the router and prover, reusing CAD for satisfiability.  
   - Allow mixed algebraic constraints (no quantifiers in assumptions yet).

4. **Quantified assumptions in theory**  
   - Allow quantified lemmas/assumptions; define safe elimination or instantiation policy to avoid unsound reuse.

5. **Geometry-to-algebra bridge for circles**  
   - Encode “three non-collinear points define a circle” as polynomial constraints with ∃ center/radius; ensure solver dispatches to CAD/quantifier engine.

6. **General multivariate real QE**  
   - Implement/plug a full quantifier elimination path (e.g., projection CAD) for ∀/∃ mixes over reals; integrate with routing heuristics and timeouts.

7. **Mixed-domain quantifiers**  
   - Handle combined integer/real quantifiers (e.g., integer parameters in real constraints) with a principled strategy (Cooper + CAD or layering).
