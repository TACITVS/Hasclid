# Hasclid Development Roadmap

## Milestone 1: Constructive Synthesis (Witness Finding)
The goal is to move beyond "True/False" verdicts for existential queries and actually return the geometric object or value that satisfies the condition.

- [x] **Expose CAD Witnesses**: Modify `src/CADLift.hs` to return the `samplePoint` of satisfying cells instead of just a Boolean.
- [x] **Implement `:synthesize` Command**: Add a new REPL command that finds values for free variables satisfying a formula.
- [x] **Integrate with SolverRouter**: Add a `synthesize` function to the router that invokes the CAD witness finder.

## Milestone 2: Transcendental / Interval Solver
Implement rigorous checking for non-algebraic inequalities (e.g., `sin(x) < x`) using interval arithmetic.

- [ ] **Create Interval Arithmetic Module**: `src/Interval.hs` for computing bounds of `Expr`.
- [ ] **Implement Interval Solver**: `src/Solvers/Interval.hs` that attempts to separate `lhs` and `rhs` intervals.
- [ ] **Wire into Router**: Use this as a fallback or parallel strategy for non-polynomial goals.

## Milestone 3: Parallelism
Execute solvers concurrently to get results faster.

- [ ] **Async Execution**: Use Haskell's `async` library in `src/SolverRouter.hs`.
- [ ] **Race Logic**: Run fast heuristics (Modular), Geometric, and Algebraic solvers in parallel, taking the first definite result.
