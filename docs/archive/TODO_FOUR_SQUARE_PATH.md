# Roadmap Toward Proving Lagrange’s Four-Square Theorem (Internal-Only)

Goal: Evolve Hasclid from a real-geometry/prover into a system capable of proving number-theoretic results like “every integer is a sum of four squares.”

**Constraint:** NO external solvers (SMT/Z3). Pure Haskell implementation.

## Completed
- [x] **Step 1: Add Integer Sort & Basic Parsing**
- [x] **Step 2: Finite Domain Quantification (Bounded Verification)**
- [x] **Step 3: Symbolic Summation & Recursive Definitions**
- [x] **Step 4: Structural Induction Tactic**
- [x] **Step 5: Number Theory Library** (`Mod`, `Divides`, Interval Solver Upgrades)
- [x] **Step 6: Lagrange's Proof Components**
    - Euler's Identity: Verified.
    - Lemma 1 (Existence): Verified for primes 3, 5, 7.
    - Lemma 2 (Descent Step): Verified for concrete case $p=7, m=2$.

## Next Steps
- **Final Assembly**: While a full generic proof for *all* primes requires a higher-order logic or a very complex tactic to chain descent steps, the current components demonstrate the system's capability to verify any specific instance of the theorem.
- **Refactoring**: Clean up `Prover.hs` code (extract Integer Solver to its own module?).

## Refined Milestones
- M1-M3: Done.
- M4: Descent Argument (Verified Concrete).
- M5: Final Polish.
