# Phase 4 Complete: Timeout System & Symbolic Fallback

## Status: COMPLETE ✅

**Date**: 2025-12-08
**Feature**: Robust Timeout Handling & Symbolic-to-Concrete Fallback

---

## The Problem
Symbolic geometric problems (e.g., involving a parameter `S`) often cause algebraic solvers (Wu's Method, Groebner Basis) to hang due to exponential complexity explosion. Previously, users had to wait indefinitely or restart the prover.

## The Solution

We implemented a **smart fallback system** that activates when a timeout occurs.

### 1. Unified Timeout Logic (`solveWithFallback`)
All solver entry points (`:auto`, scripts, REPL formulas) now route through `solveWithFallback` in `src/Main.hs`.

### 2. Strategy
1.  **Attempt 1**: Try to prove the theorem as stated (symbolic) within the user-defined timeout (default: 30s).
2.  **On Timeout**:
    *   Check if the problem contains symbolic parameters (e.g., `S`).
    *   **If Yes**: Automatically substitute all symbolic parameters with `1` (Concrete Instance).
    *   **Attempt 2**: Retry solving with the concrete values.
    *   **Report**: If successful, report the result with a clear warning:
        > "[TIMEOUT] Symbolic proof timed out. Retrying with concrete values (S=1)... NOTE: This is a specific instance check."

### 3. User Control
*   `:set-timeout <seconds>`: Control the initial attempt duration.
*   The fallback happens automatically; no user intervention required.

## Implementation Details

*   **Modified**: `src/Main.hs`
*   **New Logic**:
    *   `solveWithFallback`: Wrapper around `autoSolve`.
    *   `substituteFormula`: Recursively applies `Const 1` substitution.
    *   Integration into `processLine` (REPL) and `processScriptStreaming` (File loading).

## Example Scenario

```lisp
:set-timeout 2
:point A 0 0
:point B S 0  ; Symbolic definition
... complex theorem ...
```

**Before**:
- Solver hangs forever. User must kill process.

**After**:
- Solver runs for 2 seconds.
- Prints: `[TIMEOUT] Symbolic proof timed out. Retrying with concrete values (S=1)...`
- Returns: `RESULT: NOT PROVED` (or PROVED) for the concrete case.

This ensures the system remains responsive and provides useful feedback even for computationally intractable symbolic problems.
