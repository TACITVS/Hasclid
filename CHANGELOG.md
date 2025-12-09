# Changelog

All notable changes to Hasclid will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
