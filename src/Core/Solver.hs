module Core.Solver where

import Core.Types
import Core.Problem

-- | The outcome of a proof attempt
data ProofStatus
  = Proved              -- ^ The goal follows from assumptions
  | Disproved String    -- ^ The goal is false (optional counter-example description)
  | Failed String       -- ^ The solver could not decide (reason given)
  deriving (Show, Eq)

-- | Detailed result from a solver
data ProofResult = ProofResult
  { resultStatus :: ProofStatus
  , resultTrace :: [String]   -- ^ Steps taken (for human consumption)
  , resultTime :: Double      -- ^ Time taken in seconds
  } deriving (Show, Eq)

-- | The abstract interface for any solver strategy
class Solver s where
  solve :: s -> Problem -> IO ProofResult
  name :: s -> String
