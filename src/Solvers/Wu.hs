module Solvers.Wu where

import Core.Solver
import Core.Problem
import Core.Types
import qualified Wu as LegacyWu

data WuSolver = WuSolver

instance Solver WuSolver where
  name _ = "Wu's Method"
  solve _ problem = do
    let theory = assumptions problem
    let g = goal problem
    -- LegacyWu.wuProveWithTrace returns Either ProverError WuTrace
    result <- return $ LegacyWu.wuProveWithTrace theory g
    case result of
      Left err -> return $ ProofResult (Failed (show err)) [] 0.0
      Right trace -> return $ ProofResult 
        { resultStatus = if LegacyWu.isProved trace then Proved else Disproved (LegacyWu.proofReason trace)
        , resultTrace = [LegacyWu.formatWuTrace trace]
        , resultTime = 0.0 -- Todo: Measure time
        }
