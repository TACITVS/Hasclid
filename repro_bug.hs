module Main where

import Expr
import SolverRouter (convertAndGoalRouter)

main :: IO ()
main = do
  let goal = And (Eq (Var "MainGoal") (Const 0)) (Eq (Var "SideCondition") (Const 0))
  let theory = []
  let (th', g') = convertAndGoalRouter theory goal
  putStrLn $ "Original Goal: " ++ show goal
  putStrLn $ "New Theory: " ++ show th'
  putStrLn $ "New Goal: " ++ show g'
  if not (null th') 
    then putStrLn "BUG CONFIRMED: MainGoal moved to theory!" 
    else putStrLn "Behavior is correct."
