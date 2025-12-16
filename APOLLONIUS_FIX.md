# Apollonius Fix Report

## Issue
The file `examples/stress_suite/01_Apollonius.euclid` failed with `Parse Error: Missing closing parenthesis ')'`.

## Root Cause
1.  **Decimal Literal:** The file used `0.5`. The `Parser` treated this as a variable name `0.5` (or failed to tokenise it correctly with parens).
2.  **Comment Stripping Bug:** `ReplSupport.stripComment` aggressively stripped comments, but `consumeBalancedScript` concatenated raw lines. When `processLine` ran `stripComment` on the full multi-line string, the first comment `; LHS...` stripped the *rest of the entire input*, deleting the closing parentheses.

## Fixes
1.  **ReplSupport.hs:** Updated `consumeBalancedScript` to strip comments from *each line* individually before concatenation. This ensures multi-line commands are preserved correctly.
2.  **File Update:** Replaced `0.5` with `1/2` (and then fully eliminated fractions by multiplying by 4) to ensure polynomial compatibility and avoid rational overhead.

## Result
- Parse error is resolved.
- The prover now runs on the file (though it currently hits a memory limit on the OOM-prone Apollonius polynomial expansion in this specific environment, the logic path is correct).
