# Programming Style and Rules

This project aims for idiomatic, maintainable Haskell. Use these guidelines for all contributions.

## Core Principles
- Favor pure functions; make effects explicit (`IO`, `State`, `Reader`, etc.).
- Prefer total functions and exhaustive pattern matches; avoid partials (`head`, `fromJust`, incomplete cases).
- Model domain with algebraic data types; use strong types over raw strings/ints/bools.
- Keep data immutable; use stateful abstractions only when required and scoped.
- Make side effects visible in types; avoid hidden mutation.

## Types and Signatures
- Give top-level functions explicit type signatures.
- Use newtypes for distinct concepts; avoid magic constants.
- Add minimal annotations to prevent defaulting warnings.

## Pattern Matching and Errors
- Match exhaustively on ADTs; add an `_ -> ...` only when it is a deliberate total catch-all.
- Avoid `error`/`undefined`; prefer `Either`, `Maybe`, or domain-specific result types.
- Use guards/case for clarity; keep branches small and well named.

## Imports and Modules
- Keep imports explicit or qualified; avoid unused imports.
- Group related functions into modules with clear responsibility; avoid cyclic dependencies.
- Reuse shared helpers instead of duplicating logic across modules.

## Naming and Structure
- Use descriptive names; avoid single letters except for small local scopes.
- Follow lowerCamelCase for functions/values, UpperCamelCase for types/constructors.
- Keep functions short and composable; extract helpers rather than nesting large case/if trees.

## Effects and State
- Use pure helpers for computation; isolate IO at the boundary.
- When state is needed, prefer `State`/`Reader` or `IORef`/`STRef` with clear ownership.
- Make error handling explicit; prefer total evaluators over runtime exceptions.

## Numeric and Algebraic Code
- Avoid implicit defaulting; annotate literals or add helper types when necessary.
- Prefer total polynomial/solver utilities; avoid partial pattern matches on algebraic structures.

## Tests and Examples
- Add tests alongside behavior changes; keep fixtures minimal and deterministic.
- Keep example `.euclid` files under `examples/`; avoid ad-hoc test files in the root.

## Formatting and Linting
- Stick to standard indentation; avoid tabs.
- Remove unused bindings/imports and redundant patterns; keep warning-free (`-Wall` friendly).
- Keep comments succinct and purposeful; explain *why* when the code isn’t obvious.

## Filesystem Hygiene
- Do not commit generated artifacts or binaries; keep `.gitignore` up to date.
- Place docs in `docs/`, examples in `examples/`, and development scratch files outside the repo or in ignored paths.

## Examples of Contribution Style
- Add pure helpers first, then a thin `IO`/REPL wrapper (e.g., `:counterexample` uses a pure counterexample engine with minimal parsing/printing in `Main`).
- When adding commands, update help text, keep parsing isolated, and reuse existing solver/context plumbing.
