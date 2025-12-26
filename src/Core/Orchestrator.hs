{-# LANGUAGE ExistentialQuantification #-}
module Core.Orchestrator where

import Core.Problem
import Core.Solver
import Core.Types
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Existential wrapper for any Solver
data AnySolver = forall s. Solver s => AnySolver s

instance Solver AnySolver where
  solve (AnySolver s) = solve s
  name (AnySolver s) = name s

data ProofOutcome = ProofOutcome
  { outcomeSolver :: String
  , outcomeResult :: ProofResult
  } deriving (Show, Eq)

-- | Run a single solver on a problem with timing
runSolver :: (Solver s) => s -> Problem -> IO ProofResult
runSolver solver problem = do
  start <- getCurrentTime
  result <- solve solver problem
  end <- getCurrentTime
  let duration = realToFrac (diffUTCTime end start)
  return $ result { resultTime = duration }

-- | Strategy: Try solvers in order until one yields a definitive result (Proved/Disproved)
proveSequential :: [AnySolver] -> Problem -> IO ProofOutcome
proveSequential [] _ =
  return $ ProofOutcome
    { outcomeSolver = "None"
    , outcomeResult = ProofResult (Failed "No solvers available or all failed") [] 0.0
    }
proveSequential (s:ss) problem = do
  res <- runSolver s problem
  case resultStatus res of
    Proved -> return $ ProofOutcome (name s) res
    Disproved _ -> return $ ProofOutcome (name s) res
    Failed _ -> proveSequential ss problem
