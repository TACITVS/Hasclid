# Roadmap Toward Proving Lagrange’s Four-Square Theorem

Goal: Evolve Hasclid from a real-geometry/prover into a system capable of proving number-theoretic results like “every integer is a sum of four squares.”

Principles:
- Build incrementally with smallest viable steps.
- Reuse external solvers where possible; don’t re-implement mature number-theory engines.
- Keep geometry stack intact; add number-theory/arith extensions alongside.

Ranked Steps (easiest → hardest)

1) Add Integer Sort & Basic Parsing
   - Introduce distinct `Int`/`Nat` domains in the AST.
   - Extend parser/syntax to declare integer variables and literal forms.
   - Track domains through the router (separate from reals).

2) Integrate SMT for Quantified Integer Arithmetic (QF_NIA / NIA)
   - Add a backend to call an SMT solver (e.g., Z3/CVC5) for integer formulas.
   - Pipe integer-only goals/constraints to SMT; keep existing solvers for real algebra/geometry.
   - Return proof/unsat cores or at least SAT/UNSAT with models for counterexamples.

3) Integer-Specific Rewrites & Normalization
   - Add rewrites for divisibility, modulo, gcd/lcm when present.
   - Normalize polynomial equalities over ℤ separately from ℚ/ℝ pipelines.
   - Basic diophantine simplifications (e.g., x^2 ≡ 0 ⇒ x = 0 over ℤ).

4) Quantifier Support in Frontend & Routing
   - Parse ∀/∃ (or equivalent) and scope them to integer domains.
   - Route quantifier-bearing integer problems to SMT or higher-order backend.
   - Add skolemization/instantiation heuristics for simple patterns when SMT is avoided.

5) Integer Induction/Tactics Layer
   - Implement or integrate an induction tactic for ℕ/ℤ (structural or well-founded).
   - Provide a small tactic DSL to combine induction with rewriting/SMT checks.
   - Cache lemmas proved by induction for reuse.

6) Number-Theory Lemma Library (Incremental)
   - Seed with standard facts: parity, divisibility, gcd properties, sums of squares algebra.
   - Add closure under multiplication for sums of two/four squares (Brahmagupta-Fibonacci identity, quaternions).
   - Provide modular constraints (e.g., quadratic residues) to prune search.

7) External Proof Assistant Bridge (Optional but Powerful)
   - Add an export/import path to Lean/Coq/Isabelle for heavy theorems.
   - Allow offloading goals plus hypotheses; ingest a checkable certificate or result.
   - Use as a fallback for goals that SMT/tactics cannot discharge.

8) Higher-Order Algebraic Structures
   - Model rings/fields/groups with typeclasses or predicates to generalize lemmas.
   - Useful if extending beyond ℤ to ℤ[i], quaternions for four-square composition.

9) End-to-End Four-Square Strategy
   - Encode the classical proof: reduction to sums of two squares, use of quaternions or Gaussian integers, descent arguments.
   - Implement a composed tactic: normalize + SMT for ground arithmetic + induction for descent + library lemmas for sum-of-squares closure.

10) Certification & Proof Objects
   - Emit proof terms or certificates for integer theorems (from SMT or tactic runs).
   - Optional: checker inside Hasclid to validate returned proofs.

Milestones to Track Progress
- M1: Integer sort parsing + SMT backend wired for quantifier-free integer goals.
- M2: Quantifiers parsed and routed; basic induction tactic available.
- M3: Number-theory lemma library (parity/divisibility/gcd) in place; simple diophantine goals prove.
- M4: Sum-of-two-squares closure lemmas automated (Brahmagupta-Fibonacci).
- M5: Successful automated proof of a bounded four-square claim (e.g., for n ≤ N).
- M6: Full Lagrange four-square proof via composed tactics or external PA bridge.

Open Questions / Design Choices
- SMT vs. in-house induction: how much to rely on external solvers?
+- Proof objects: do we require certificates for trust, or accept solver answers?
+- API: how to expose integer domains and quantifiers without complicating the existing REPL UX?

Notes
- Keep geometry stack untouched; add routing that selects integer backends when domains are integral.
- Prefer modularization: new modules for integer logic, SMT bridge, and tactics, keeping Expr lean if possible.
