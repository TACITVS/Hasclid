module IntervalSolverSpec (spec) where

import Test.Hspec
import Solvers.Interval
import Expr
import Data.Map.Strict as M

spec :: Spec
spec = describe "Interval Solver" $ do
  
  it "proves trivial algebraic inequality x > 0 given x >= 1" $ do
    let theory = [Ge (Var "x") (Const 1)]
    let goal = Gt (Var "x") (Const 0)
    let (proved, _) = solveInterval theory goal
    proved `shouldBe` True

  it "proves sin(x) <= 1" $ do
    let theory = []
    let goal = Le (Sin (Var "x")) (Const 1)
    let (proved, _) = solveInterval theory goal
    proved `shouldBe` True

  it "proves sin(x) >= -1" $ do
    let theory = []
    let goal = Ge (Sin (Var "x")) (Const (-1))
    let (proved, _) = solveInterval theory goal
    proved `shouldBe` True

  it "fails to prove sin(x) > 2 (correctly)" $ do
    let theory = []
    let goal = Gt (Sin (Var "x")) (Const 2)
    let (proved, _) = solveInterval theory goal
    proved `shouldBe` False

  it "proves x + y > 0 given x > 1, y > -0.5" $ do
    let theory = [Gt (Var "x") (Const 1), Gt (Var "y") (Const (-0.5))]
    let goal = Gt (Add (Var "x") (Var "y")) (Const 0)
    let (proved, _) = solveInterval theory goal
    proved `shouldBe` True
