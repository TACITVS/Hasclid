module Main where

import Core.Types
import Core.Problem
import Core.Solver
import Core.Orchestrator
import Solvers.Wu
import Solvers.Groebner
import Preprocessing (preprocess, PreprocessingResult(..))
import qualified Data.Map.Strict as M

-- | Helper to run and print results
runDemo :: String -> [AnySolver] -> Problem -> IO ()
runDemo name solvers problem = do
  putStrLn $ "\n" ++ replicate 60 '=' 
  putStrLn $ "DEMO: " ++ name
  putStrLn $ replicate 60 '-' 
  putStrLn $ "Goal: " ++ prettyFormula (goal problem)
  putStrLn $ "Assumptions (" ++ show (length (assumptions problem)) ++ "):"
  mapM_ (\a -> putStrLn $ "  " ++ prettyFormula a) (assumptions problem)
  putStrLn $ replicate 60 '-' 
  
  result <- proveSequential solvers problem
  
  putStrLn $ "Status: " ++ show (resultStatus result)
  putStrLn $ "Time:   " ++ show (resultTime result) ++ "s"
  putStrLn "Trace Summary:"
  mapM_ (\step -> putStrLn $ "  " ++ take 100 step ++ "...") (resultTrace result)

-- =============================================================================
-- Example 1: Pure Algebraic Transitivity
-- Problem: a = b, b = c |- a = c
-- Solvers: Groebner (Robust for general algebra), Wu (Specialized)
-- =============================================================================
demoAlgebraic :: IO ()
demoAlgebraic = do
  let a = Var "a"
  let b = Var "b"
  let c = Var "c"
  
  -- Define Problem
  let prob = mkProblem 
               [Eq a b, Eq b c]  -- Assumptions
               (Eq a c)          -- Goal
  
  -- Strategy: Try Groebner first (General Algebra), then Wu
  let strategy = [AnySolver GroebnerSolver, AnySolver WuSolver]
  
  runDemo "Algebraic Transitivity" strategy prob

-- =============================================================================
-- Example 2: Geometric Midpoint Theorem
-- Problem: M is midpoint of AC. Prove AM = MC (using distance squared)
-- Pipeline: Geometry -> Expansion -> Preprocessing -> Algebra -> Solver
-- =============================================================================
demoGeometric :: IO ()
demoGeometric = do
  -- 1. Define Geometric Problem
  let pA = "A"; pC = "C"; pM = "M"
  
  -- Assumption: M is midpoint of AC
  -- High-level geometric predicate
  let h1 = Eq (Midpoint pA pC pM) (Const 0)
  
  -- Goal: dist(A, M)^2 = dist(M, C)^2
  let g = Eq (Dist2 pA pM) (Dist2 pM pC)
  
  putStrLn "\n[Pipeline] 1. Defined Geometric Problem"
  putStrLn $ "  Assumption: " ++ prettyFormula h1
  putStrLn $ "  Goal:       " ++ prettyFormula g
  
  -- 2. Geometric Expansion (Simulating the compiler's work)
  --    Expand 'Midpoint A C M' into coordinate equations:
  --    2*xM = xA + xC, etc.
  putStrLn "[Pipeline] 2. Expanding Geometric Predicates..."
  let midpointEqs = expandMidpoint pA pC pM
  mapM_ (\eq -> putStrLn $ "  Expanded: " ++ prettyFormula eq) midpointEqs
  
  -- 3. Preprocessing
  --    - Assigns coordinates, handles variables
  putStrLn "[Pipeline] 3. Running Preprocessor..."
  let prepResult = preprocess M.empty midpointEqs g
  
  let algTheory = preprocessedTheory prepResult
  let algGoal = preprocessedGoal prepResult
  
  putStrLn $ "  Preprocessed Theory Size: " ++ show (length algTheory)
  
  let prob = mkProblem algTheory algGoal
  
  -- 4. Solve
  putStrLn "[Pipeline] 4. Dispatching to Orchestrator..."
  -- Note: We use Groebner here as it handles the coordinate system well
  -- Wu works best when points are introduced constructively (A->B->M)
  let strategy = [AnySolver GroebnerSolver] 
  
  runDemo "Geometric Midpoint (Preprocessed)" strategy prob

-- | Helper to manually expand Midpoint into coordinate equations
expandMidpoint :: String -> String -> String -> [Formula]
expandMidpoint a b m = 
  let axes = ["x", "y", "z"]
      -- 2*m - a - b = 0  =>  2*m = a + b
      mkEq axis = Eq 
         (Sub (Mul (Const 2) (Var (axis++m))) (Add (Var (axis++a)) (Var (axis++b))))
         (Const 0)
  in map mkEq axes

-- =============================================================================
-- Main Entry
-- =============================================================================
main :: IO ()
main = do
  putStrLn "Hasclid Unified API Demonstration"
  demoAlgebraic
  demoGeometric
