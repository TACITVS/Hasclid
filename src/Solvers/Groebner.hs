module Solvers.Groebner where

import Core.Solver
import Core.Problem
import Core.Types
import qualified Prover
import qualified BuchbergerOpt
import ProofMode (ProofMode(..), defaultProofMode)

-- | Groebner Solver with configuration
data GroebnerSolver = GroebnerSolver
  { groebnerOptions :: Prover.IntSolveOptions
  , proofMode :: ProofMode
  -- We can add more options here from SolverRouter.SolverOptions later
  }

-- | Default instance
mkGroebnerSolver :: GroebnerSolver
mkGroebnerSolver = GroebnerSolver Prover.defaultIntSolveOptions defaultProofMode

instance Solver GroebnerSolver where
  name _ = "Groebner Basis"
  solve (GroebnerSolver _opts mode) problem = do
    let assums = assumptions problem
    let g = goal problem
    
    -- Use proveTheoryWithCache (pure function)
    -- In a full refactor, we would pass 'opts' to proveTheoryWithOptions
    -- For now, using default proveTheoryWithCache which is the standard entry point
    let (isProved, reason, trace, _) = Prover.proveTheoryWithCache mode Nothing assums g
    
    return $ ProofResult
      { resultStatus = if isProved 
                       then Proved 
                       else Failed reason 
      , resultTrace = [Prover.formatProofTrace trace]
      , resultTime = 0.0 
      }
