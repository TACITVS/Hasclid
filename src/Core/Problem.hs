module Core.Problem where

import Core.Types
import qualified Data.Map.Strict as M

-- | Represents a theorem proving problem
data Problem = Problem
  { assumptions :: [Formula]       -- ^ List of constraints/axioms
  , goal :: Formula                -- ^ The statement to prove
  , symbols :: M.Map String Expr   -- ^ Known definitions or symbol table
  , metadata :: M.Map String String -- ^ Extra info (e.g., "name" -> "Ono", "difficulty" -> "Hard")
  } deriving (Show, Eq)

-- | Create a basic problem from assumptions and a goal
mkProblem :: [Formula] -> Formula -> Problem
mkProblem assums g = Problem
  { assumptions = assums
  , goal = g
  , symbols = M.empty
  , metadata = M.empty
  }
