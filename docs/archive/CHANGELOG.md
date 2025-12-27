# Changelog

All notable changes to Hasclid will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [9.4.0] - 2025-12-27

### Added
- **NthRoot AST Constructor**: New `NthRoot Natural Expr` constructor for general nth-roots (cube roots, fourth roots, etc.)
- **Coefficient Pattern Matching**: `matchRootPattern` function to extract coefficient/index/radicand from expressions like `2*sqrt(ab)`
- **Coefficient*Sqrt Formula Patterns**: Direct pattern matching in `elimFormula` for `Ge`, `Gt`, `Le`, `Lt`, `Eq` with coefficient patterns
- **ElimConfig Type**: Configurable root elimination with `maxSquaringDepth` and `enableInequiSquaring` options
- **Extended AlgebraicConstraint**: Now tracks `acVar`, `acIndex`, `acRadicand`, `acCoefficient`, and `acSign`

### Changed
- **Smart Inequality Squaring**: Re-enabled with depth limit (default: 3) to prevent infinite loops
- **SqrtElim Module**: Complete overhaul for robust coefficient handling
  - `b >= c*sqrt(a)` now directly transforms to `b² >= c²*a`
  - Coefficient absorption: `c*sqrt(a)` → `sqrt(c²*a)` in expressions
- **algebraicReducer**: Generalized to `substituteNthPower` for nth-power variable substitution
- **Pattern Matching Order**: Specific coefficient patterns now match before general catch-all patterns

### Fixed
- **AM-GM with Coefficient**: `a+b >= 2*sqrt(ab)` now PROVES correctly (previously only `a+b >= sqrt(4ab)` worked)
- **Equality with Coefficients**: `2*sqrt(ab) = sqrt(4ab)` now proves correctly
- **Root Handling Consistency**: Both coefficient and non-coefficient forms produce identical polynomial goals

### Performance
- **Test Suite**: All 111 unit tests pass
- **AM-GM Variants**: All standard AM-GM formulations now prove automatically

## [9.3.0] - 2025-12-17

### Added
- **Decimal Number Support**: Full support for decimal coordinates and constants (e.g., `1.5` → exact rational `5/4`)
- **Geometric Predicate Expansion**: Automatic expansion of `Midpoint` and `Parallel` to separate coordinate equations
- **Point Substitution Engine**: Preprocessing now properly applies point coordinate substitutions in all proof commands
- **Dist2 Expansion**: `Dist2` predicate expanded during preprocessing for correct substitution behavior
- **Comprehensive Status Report**: PREPROCESSING_FIXES_COMPLETE.md documenting all improvements

### Changed
- **Encoding Strategy**: `Midpoint` and `Parallel` now use coordinate-wise equality instead of sum-of-squares
- **Preprocessing Flow**: All proof commands (`:prove`, `:wu`, `:auto`) now apply preprocessing with point substitutions
- **Parser Enhancement**: Both `parseCoord` and `exprFromSExpr` support decimal notation
- **Circle Predicate**: Now expands via `Dist2` during preprocessing

### Fixed
- **Critical Bug**: Point coordinate substitutions now correctly applied in `:prove` and `:wu` handlers
- **Encoding Bug**: Geometric predicates no longer use sum-of-squares (incompatible with Gröbner basis ideal membership)
- **Substitution Bug**: `Dist2` expansion now happens before substitution, not during polynomial conversion
- **Parse Bug**: Decimal numbers like `1.25` no longer treated as variable names

### Performance
- **Stress Suite**: Improved from 1/10 (10%) to 7/10 (70%) pass rate
- **New Proofs**: Apollonius, Ptolemy, Euler, Weitzenbock, Orthocenter now prove automatically
- **Concrete Computation**: Tests with concrete coordinates (e.g., 08_Euler_d2) now instant

## [9.1.0] - 2025-12-08

### Added
- **Timeout System**: Configurable solver timeout with :set-timeout and :show-timeout commands
- **Symbolic Fallback**: Automatic retry with concrete values (e.g., S=1) when symbolic proof times out
- **GeoSolver (Phase 1)**: Fast symbolic geometric constraint propagation solver
- **Modular Arithmetic Solver**: Probabilistic existence proofs over finite fields
- **Integer Constraint Solver**: Linear interval solving with bounded brute-force search
- **Constructive Wu Method**: Witness construction for existentially quantified formulas
- **New REPL Commands**:
  - :counterexample / :ce - Search for counterexamples
  - :construct - Find satisfying assignments
  - :bruteforce on|off - Enable/disable integer brute-force search
  - :optimize on|off - Toggle Buchberger optimization
  - :set-strategy - Configure S-polynomial selection (normal|sugar|minimal)
  - :soft-reset - Reset theory while preserving cache
  - :list-lemmas - Show stored lemmas
  - :solve <file> - Batch formula processing

### Changed
- **Two-Phase Architecture**: GeoSolver (Phase 1) → Algebraic Solvers (Phase 2)
- **Default Behavior**: All formulas now use automatic solver selection via router
- **Improved Routing**: Intelligent dispatch to 6 specialized solvers
- **Better Error Handling**: No more indefinite hangs on complex problems
- **Documentation**: Complete rewrite of architecture and command reference

### Fixed
- **Timeout Protection**: Solver no longer hangs on intractable problems
- **Symbolic Parameter Handling**: Graceful degradation to concrete instances
- **Positivity Checking**: Fixed tryUnivariate bug affecting x² ≥ 0 queries
- **Test Coverage**: Complete Hspec test suite with QuickCheck properties

## [9.0.0] - 2025-01-XX

### Added
- **Multi-Solver Architecture**: Intelligent routing between Wu, Gröbner, and CAD
- **Wu's Method**: Characteristic set method for geometric theorem proving
- **SolverRouter**: Automatic solver selection based on problem analysis
- **ProblemAnalyzer**: Problem classification and complexity estimation
- **Quantifier Support**: Basic handling for Exists and Forall
- **CAD Lifting**: Cylindrical Algebraic Decomposition for inequalities
- **Sturm Sequences**: Real root isolation for univariate polynomials

### Changed
- **Solver Selection**: Automatic dispatch instead of always using Gröbner
- **Performance**: 10-100x speedup for geometric problems via Wu's method
- **Robustness**: Better handling of different problem types

## [8.0.0] - 2024-XX-XX

### Added
- **CAD Baseline**: Basic Cylindrical Algebraic Decomposition
- **Recursive Polynomials**: Support for multivariate CAD projection
- **Cell Classification**: Sign determination for CAD cells

### Changed
- **Inequality Solver**: Initial CAD-based approach (limited to 1D-2D)

## [7.3.0] - 2024-XX-XX

### Added
- **Gröbner Basis**: Buchberger's algorithm implementation
- **Term Orderings**: Support for grevlex, lex, gradedlex
- **Geometric Primitives**: dist2, collinear, dot, circle
- **REPL**: Interactive theorem proving environment
- **S-Expression Parser**: Prefix notation for formulas

### Features
- Polynomial equality proving
- Concrete geometric theorem solving
- Basic scripting with .euclid files
