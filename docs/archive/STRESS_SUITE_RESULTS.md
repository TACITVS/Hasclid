# Stress Suite Test Results

**Date**: 2025-12-16
**Prover Version**: Hasclid v9.0
**Total Tests**: 10

## Summary

| Result | Count |
|--------|-------|
| ✅ SUCCESS | 1 |
| ❌ MEMORY FAILURE | 4 |
| ⏱️ TIMEOUT | 2 |
| ❓ PROOF FAILED | 2 |
| 🐛 PARSE ERROR | 1 |

## Detailed Results

### ✅ SUCCESS (1/10)

**05_CauchySchwarz.euclid**
- Status: PROVED
- Solver: CAD (universal quantifier refuted negation)
- Time: < 1s
- Note: Proves Cauchy-Schwarz inequality using CAD

### ❌ MEMORY FAILURES (4/10)

**02_Varignon.euclid**
- Error: `getMBlocks: VirtualAlloc MEM_COMMIT failed: The paging file is too small`
- Issue: Memory allocation failure when processing midpoint constraints
- Theorem: Varignon's theorem (midpoint quadrilateral forms parallelogram)

**03_Orthocenter.euclid**
- Error: `getMBlocks: VirtualAlloc MEM_COMMIT failed`
- Issue: Memory allocation failure
- Theorem: Orthocenter properties

**04_NinePointCircle.euclid**
- Status: Did not complete within 180s
- Suspected: Memory or extreme computational complexity

**07_Ptolemy.euclid**
- Error: `getMBlocks: VirtualAlloc MEM_COMMIT failed`
- Issue: Memory allocation failure
- Theorem: Ptolemy's theorem

### ⏱️ TIMEOUT (2/10)

**06_TriangleInequality.euclid**
- Error: `[TIMEOUT] exceeded 30s` (default timeout)
- Formula: Triangle inequality with sqrt
- Issue: Timeout with default 30s limit (needs higher timeout or optimization)

**04_NinePointCircle.euclid** (also listed above)
- Timeout at test level (>180s)

### ❓ PROOF FAILED (2/10)

**08_Euler_d2.euclid**
- Status: NOT PROVED
- Error: `LHS /= RHS (Normal Form: zO^2 - 2zI*zO + zI^2 + yO^2 - 2yI*yO + yI^2 + xI^2 - 21.5*xI + 1.5^2 - 1.25)`
- Issue: Prover correctly determined the formula is false (not provable)
- Note: Might be a test case error or intentional negative test

**10_ErdosMordell_Rb.euclid**
- Status: NOT PROVED
- Message: `[HEURISTIC] Heuristic sampling disabled; use a sound solver (CAD/GB)`
- Issue: Solver unable to prove Erdos-Mordell inequality component
- Note: May require different solver strategy or additional assumptions

### 🐛 PARSE ERROR (1/10)

**01_Apollonius.euclid**
- Error: `Parse Error: Missing closing parenthesis ')'`
- Issue: Multi-line :wu command not being parsed correctly
- Parentheses: Manually verified as balanced (22 open, 22 close)
- Root Cause: REPL may not support multi-line commands in piped input mode
- Workaround Needed: Reformat to single line or fix parser

## Critical Issues Identified

### 1. Memory Management **[CRITICAL]**
- **Impact**: 40% of stress tests fail with memory allocation errors
- **Root Cause**: Likely exponential memory growth in:
  - Groebner basis computation for complex geometries
  - CAD decomposition for multi-variable systems
  - Polynomial expansion without optimization
- **Recommendation**:
  - Implement timeout-aware memory monitoring
  - Add polynomial degree limits
  - Optimize CAD cell decomposition
  - Consider incremental garbage collection

### 2. Timeout Handling **[HIGH]**
- **Impact**: Default 30s timeout too aggressive for hard problems
- **Root Cause**: Complex formulas with sqrt need more time
- **Recommendation**:
  - Increase default timeout to 120s for stress tests
  - Add progressive timeout warnings
  - Implement partial result caching

### 3. Multi-line Command Parsing **[MEDIUM]**
- **Impact**: Cannot use formatted multi-line formulas in piped input
- **Root Cause**: REPL expects complete commands per line in non-interactive mode
- **Recommendation**:
  - Support multi-line buffering in piped input mode
  - Or require single-line format for script files

### 4. Proof Completeness **[NEEDS INVESTIGATION]**
- **Impact**: 20% of tests report "NOT PROVED"
- **Questions**:
  - Are these test case errors?
  - Are they intentionally unprovable?
  - Or does the prover need strategy improvements?
- **Recommendation**: Review each NOT PROVED case individually

## Success Rate Analysis

| Metric | Value |
|--------|-------|
| Completed Tests | 9/10 (90%) |
| Successful Proofs | 1/9 (11%) |
| Memory Failures | 4/9 (44%) |
| Timeouts | 2/9 (22%) |
| Proof Failures | 2/9 (22%) |
| Parse Errors | 1/10 (10%) |

## Comparison to User Expectation

**User Prediction**: "many have serious problems being proved or refuted"
**Reality**: **CONFIRMED** - Only 1 out of 10 stress tests succeeded

The stress suite exposed severe issues:
- Memory allocation failures are the #1 blocker
- Only simple problems (Cauchy-Schwarz with universal quantifier) succeed
- Geometric theorems with multiple points cause memory exhaustion

## Recommended Action Items

1. **IMMEDIATE**: Fix memory allocation in Groebner basis computation
2. **HIGH**: Increase stress test timeouts and add :set-timeout commands to files
3. **MEDIUM**: Fix multi-line command parsing for 01_Apollonius
4. **MEDIUM**: Investigate proof failures in 08_Euler and 10_ErdosMordell
5. **LONG-TERM**: Implement polynomial operation optimizations (Phase 3.3)

## Conclusion

The stress suite has successfully identified critical weaknesses in the prover:
- ✅ Basic functionality works (simple tests pass)
- ❌ **Memory management fails on realistic geometric problems**
- ❌ Default timeouts inadequate for hard problems
- ❌ Proof success rate on stress tests: **10%**

This validates the need for Phase 3 optimizations and additional memory management work beyond the original audit plan.
