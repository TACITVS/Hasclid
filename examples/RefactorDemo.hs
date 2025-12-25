module Main where

import Core.Types
import Core.Problem
import Core.Solver
import Core.Orchestrator
import Solvers.Wu
import Solvers.Groebner

main :: IO ()
main = do
  let a = Var "a"
  let b = Var "b"
  let c = Var "c"
  let h1 = Eq a b
  let h2 = Eq b c
  let g = Eq a c
  
  let problem = mkProblem [h1, h2] g
  
  let solvers = [AnySolver GroebnerSolver, AnySolver WuSolver]
  
  putStrLn "Attempting proof with Sequential Strategy (Groebner -> Wu)..."
  result <- proveSequential solvers problem
  print result