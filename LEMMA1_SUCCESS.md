# Lemma 1 Verification (Number Theory)

## Objective
Verify "Lemma 1" of Lagrange's Four-Square Theorem:
For any prime $p$, there exist integers $x, y$ such that $x^2 + y^2 + 1 \equiv 0 \pmod p$.

## Methodology
Used the newly implemented Number Theory Library features in Hasclid:
1.  **Divisibility**: `(divides p expr)`
2.  **Bounded Quantification**: `(exists ((int x 0 p) (int y 0 p)) ...)`
3.  **Brute Force Solver**: `:bruteforce on` combined with the integer interval solver.

## Results
The solver successfully found witnessing integers for:
- **p=3**: $x=1, y=1 \implies 1^2+1^2+1 = 3 \equiv 0 \pmod 3$ (Found)
- **p=5**: $x=2, y=0 \implies 2^2+0^2+1 = 5 \equiv 0 \pmod 5$ (Found)
- **p=7**: $x=2, y=3 \implies 2^2+3^2+1 = 14 \equiv 0 \pmod 7$ (Found)

## Key Solver Upgrades
- **Non-Linear Integer Support**: `intSat` now attempts `intIntervalSolve` even for non-linear formulas.
- **Evaluation Fallback**: `decideWithIntervals` uses `evaluateIfSingleton` to verify non-linear constraints when variables are fully assigned (singleton intervals).
- **Recursive Branching**: `branchSmall` now implements a backtracking search to handle multiple variables simultaneously.
- **Bounded Quantifier Extraction**: `autoSolve` and `proveTheory` now extract bounds from quantifiers like `(int x 0 5)` and add them to the constraint store.

## Conclusion
The Number Theory Library is functional and capable of solving bounded Diophantine problems essential for the Four-Square Theorem proof steps.
