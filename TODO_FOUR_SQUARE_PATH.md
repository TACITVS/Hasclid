# Roadmap Toward Proving Lagrange’s Four-Square Theorem (Internal-Only)

Goal: Evolve Hasclid from a real-geometry/prover into a system capable of proving number-theoretic results like “every integer is a sum of four squares.”

**Constraint:** NO external solvers (SMT/Z3). Pure Haskell implementation.

## Completed
- [x] **Step 1: Add Integer Sort & Basic Parsing**
- [x] **Step 2: Finite Domain Quantification (Bounded Verification)**
    - Implemented expansion for `forall/exists` with integer bounds.
    - Verified with `test_bounded_quant.euclid`.

## Next Steps (Revised)

### Step 3: Symbolic Summation & Recursive Definitions
- **Goal:** Express "sum of squares" or "sum of first n integers".
- **Action:**
    - Add `Sum` primitive to `Expr`. `(sum index lo hi body)`.
    - Support basic simplification of sums (e.g. `sum i 0 0 f(i) = f(0)`).

### Step 4: Structural Induction Tactic
- **Goal:** Prove `forall ((int n)) P(n)` using induction.
- **Action:**
    - Detect `forall ((int n))`.
    - Generate Base Case `P(0)`.
    - Generate Step Case: `P(k) -> P(k+1)`.
    - Handle `Sum` in Step Case (splitting `sum 0 (k+1)` into `sum 0 k + term(k+1)`).

### Step 5: Number Theory Library
- **Goal:** Define `is-sum-of-4-sq(n)`.
- **Action:** Add `def` or macros that can use `exists`.

### Step 6: Lagrange's Proof
- **Strategy:**
    1. Euler's Identity (Done).
    2. Induction proofs for summation identities.
    3. Descent argument (requires more advanced logic).

## Refined Milestones
- M1: `Sum` primitive.
- M2: Induction Tactic.
