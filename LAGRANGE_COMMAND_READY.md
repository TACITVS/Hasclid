# Lagrange Command Ready

## Summary
The `:lagrange` command is fully operational and integrated.

## Usage
Start the prover:
```bash
cabal run prover-int
```

Inside the REPL:
```lisp
Euclid> :lagrange 7
Lagrange sum of 4 squares for 7: [2,-1,-1,-1]

Euclid> :lagrange 30
Lagrange sum of 4 squares for 30: [-5,-1,2,0]
```

## Implementation
- **Logic**: `src/Lagrange.hs` implements the randomized descent algorithm (deterministic for small primes via brute force initial step).
- **Interface**: `src/Main.hs` hooks the command.
- **Dependencies**: `prover.cabal` links `Lagrange` module.

## Status
- Verified via `examples/test_lagrange_cmd.euclid`.
- Partial function `head` usage removed for safety.
