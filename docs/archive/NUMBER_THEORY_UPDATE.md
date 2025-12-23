# Number Theory Library Update

## Summary
The Number Theory Library core has been implemented to support the proof of Lagrange's Four-Square Theorem. This update adds support for modular arithmetic and divisibility reasoning within the integer solver.

## New Features

### 1. New Primitives
- **Modulo (`mod a b`)**: Represented as `Mod Expr Expr`. Simplifies to constants where possible.
- **Divides (`divides a b`)**: Represented as `Divides Expr Expr`. Meaning $a | b$ (i.e., $b \equiv 0 \pmod a$).

### 2. Solver Enhancements
- **Modular Constraint Propagation**: The integer interval solver (`intSolve`) now propagates `Divides` constraints.
    - `(divides 5 n)` combined with $n \in [10, 20]$ tightens the domain to $\{10, 15, 20\}$.
    - This allows solving Diophantine-style problems by tightening bounds using modular arithmetic.
- **Improved Integer Division**: Fixed bugs in `floorDiv` logic for strict inequalities with negative coefficients.
- **Simplification**: `simplifyExpr` now handles `Div` by constants by converting them to `Mul` (e.g., `x / 2` -> `x * 0.5`), fixing issues with polynomial conversion for induction proofs involving fractions.

### 3. Verification
- Verified with `test_number_theory.euclid`.
- Verified Induction and Summation with `test_onboarding.euclid`.

## Usage Example

```lisp
:declare-int n
:assume (>= (int n) (int-const 10))
:assume (<= (int n) (int-const 20))
:assume (divides (int-const 5) (int n))
-- n is now {10, 15, 20}
:assume (> (int n) (int-const 12))
:assume (< (int n) (int-const 18))
-- n is now {15}
:prove (= (int n) (int-const 15))
```

## Next Steps
- Implement the "Descent Argument" using these new capabilities.
