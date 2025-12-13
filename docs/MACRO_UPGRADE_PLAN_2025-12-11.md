# Macro System Upgrade Plan (2025-12-11)

Goal: move toward Lisp-like macros safely, without destabilizing the prover or violating “no Greenspun #10”. Implement in phased, test-driven steps.

## Phase 1: Diagnostics & Safety Nets
- Add explicit expansion errors: arity mismatch, unknown macro, recursion/cycle with depth/trace.
- Add optional “:show-expanded” (or REPL flag) to display fully expanded form of the last parsed expression/command.
- Enforce expansion depth limit with a clear message; no `error` calls.
- Tests: nested macros, arity mismatch failure, unknown macro failure, expansion depth exceeded.

## Phase 2: Hygiene & Scoped Macro Environments
- Introduce `gensym` support in the expander; ensure generated names cannot collide with user variables.
- Add local macro scopes so definitions don’t leak globally; consider block-level scopes for script parsing.
- Tests: macro introducing bindings that shadow user variables, nested scopes, and verifying capture is prevented by hygiene.

## Phase 3: Templates (Quasiquote/Unquote) and Pattern-Based Macros
- Add quasiquote/unquote so macros can build code templates succinctly.
- Add a minimal pattern-based macro form (e.g., simple destructuring on `List` with `_` wildcards).
- Tests: template generation, unquote splicing, pattern match success/failure cases, and interaction with gensym.

## Phase 4: Command-Level Macros (Optional, if needed)
- Allow macros to rewrite full commands (`:assume`, `:prove`, etc.) under strict scoping to avoid unsafe rewrites.
- Add diagnostics to preview rewritten commands before execution.
- Tests: command rewrite success, rejection of unsafe rewrites, interaction with multi-line input.

## Phase 5: Coverage & Regression
- Expand property/unit tests to cover all new features plus regression cases for existing macro behavior.
- Add benchmarks on representative scripts to ensure expansion remains fast and does not explode size.

## Constraints / Non-Goals
- Keep expansions pure and predictable; no runtime side effects during expansion.
- Maintain current simple S-expression syntax; no full Lisp reader or eval.
- Keep error messages user-friendly and short in REPL context.
