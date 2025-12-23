# Semidefinite Programming (SDP) Solver

**Hasclid v9.3** includes a native, arbitrary-precision SDP solver written in pure Haskell. This component enables the verification of polynomial inequalities via Sum-of-Squares (SOS) decompositions.

## Overview

The solver implements a **Primal-Dual Interior Point Method** (specifically, a variant of the Mehrotra Predictor-Corrector algorithm). It minimizes the duality gap between the Primal and Dual SDP formulations.

### Mathematical Formulation

**Primal (P):**
$$
\begin{aligned}
\text{minimize} \quad & \text{Tr}(C X) \\
\text{subject to} \quad & \text{Tr}(A_i X) = b_i, \quad i=1,\dots,m \\
& X \succeq 0
\end{aligned}
$$

**Dual (D):**
$$
\begin{aligned}
\text{maximize} \quad & b^T y \\
\text{subject to} \quad & \sum_{i=1}^m y_i A_i + S = C \\
& S \succeq 0
\end{aligned}
$$

### Implementation Details

*   **Location**: `src/Positivity/SDP.hs`
*   **Precision**: Uses `Data.Number.BigFloat` (from `numbers` package) with configurable precision (default `Prec50` $\approx$ 166 bits).
*   **Matrix Library**: Custom implementation using `Data.Array.IO` (mutable boxed arrays) for performance.
*   **Decomposition**: Uses dense Cholesky decomposition ($LL^T$) to solve the Newton system.

## Usage

The solver is automatically invoked by the `Prover` when attempting to prove inequalities (`>=`, `>`) if simple symbolic methods fail.

### Integration with Prover

1.  **Extract Constraints**: The prover extracts inequality constraints (e.g., `:assume (> x 0)`) from the theory.
2.  **Construct SDP**: The polynomial positivity problem is converted into an SDP using **Putinar's Positivstellensatz**.
    *   $P(x) = S_0(x) + \sum S_i(x) g_i(x)$
    *   $S_i(x)$ are Sum-of-Squares polynomials represented by PSD matrices $Q_i$.
3.  **Solve**: The SDP solver attempts to find feasible $Q_i$.
4.  **Result**:
    *   `True`: A numerical certificate exists (high confidence).
    *   `False`: Infeasible, unbounded, or iteration limit reached.

## Limitations

1.  **Performance**: The solver uses dense matrices. The complexity is roughly $O(m^2 n^2 + m^3)$ per iteration, where $m$ is the number of monomial constraints and $n$ is the basis size. For high-degree polynomials (deg > 8), this becomes computationally expensive.
2.  **Boundary Conditions**: Problems where the minimum is exactly 0 (e.g., Ono's inequality) are numerically difficult due to the loss of strict complementarity at the boundary (singular Jacobian).
3.  **Degree**: The default relaxation degree is minimal. Some theorems require higher-degree multipliers to be provable, which increases matrix size exponentially.

## Future Work

*   **Sparse Linear Algebra**: Implementing sparse Cholesky would significantly improve performance for high-degree problems.
*   **Rational Recovery**: Using the high-precision numeric solution to snap to exact rational coefficients for a purely symbolic proof.
