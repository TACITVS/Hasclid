{-# LANGUAGE ExistentialQuantification #-}
module Core.Orchestrator where

import Core.Problem
import Core.Solver
import Core.Types
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent (forkIO, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar, MVar)
import Control.Monad (forM_, void)

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

-- | Strategy: Try solvers in order until one yields a definitive result
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

-- | Strategy: Run solvers in parallel and take the first definitive result
proveParallel :: [AnySolver] -> Problem -> IO ProofOutcome
proveParallel [] _ = proveSequential [] undefined -- Fallback for empty
proveParallel solvers problem = do
  resultVar <- newEmptyMVar
  doneVar <- newMVar (0 :: Int)
  let n = length solvers
  
  forM_ solvers $ \s -> forkIO $ do
    res <- runSolver s problem
    case resultStatus res of
      Proved -> void $ tryPutMVar resultVar (ProofOutcome (name s) res)
      Disproved _ -> void $ tryPutMVar resultVar (ProofOutcome (name s) res)
      Failed _ -> do
        count <- takeMVar doneVar
        let newCount = count + 1
        if newCount == n
          then putMVar resultVar (ProofOutcome "None" res) -- Return last failure
          else putMVar doneVar newCount

  takeMVar resultVar
