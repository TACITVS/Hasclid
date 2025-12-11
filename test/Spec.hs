module Main where

import Expr
import BuchbergerOpt (reduce)
import Positivity (checkPositivityEnhanced, isPositive)
import Prover (intSolve, intResult, defaultIntSolveOptions)
import Test.QuickCheck
import qualified Data.Map.Strict as M

-- Property 1: toPoly distributes over addition of constants
prop_toPolyConstAdd :: Integer -> Integer -> Bool
prop_toPolyConstAdd a b =
  let e = Add (Const (fromInteger a)) (Const (fromInteger b))
  in toPoly e == polyAdd (polyFromConst (fromInteger a)) (polyFromConst (fromInteger b))

-- Property 2: reduce p [p] is zero (self-reduction)
prop_reduceSelfZero :: Bool
prop_reduceSelfZero =
  let p = toPoly (Add (Var "x") (Const 1))
  in reduce compare p [p] == polyZero  -- Use Lex order

-- Property 3: simple positive polynomial is detected as positive
prop_pos_isPositive :: Bool
prop_pos_isPositive =
  let p = polyAdd (polyPow (polyFromVar "x") 2) (polyFromConst 1)
  in isPositive (checkPositivityEnhanced p True)

-- Property 4: integer solver proves trivial integer equality
prop_intSolveConst :: Bool
prop_intSolveConst =
  let goal = Eq (IntConst 1) (IntConst 1)
      outcome = intSolve defaultIntSolveOptions [] goal
  in intResult outcome == Just True

-- Property 5: integer substitution from theory propagates
prop_intSolveSubstitution :: Bool
prop_intSolveSubstitution =
  let th = [Eq (IntVar "x") (IntConst 2)]
      goal = Eq (Add (IntVar "x") (IntConst 1)) (IntConst 3)
      outcome = intSolve defaultIntSolveOptions th goal
  in intResult outcome == Just True

main :: IO ()
main = do
  quickCheck prop_toPolyConstAdd
  quickCheck prop_reduceSelfZero
  quickCheck prop_pos_isPositive
  quickCheck prop_intSolveConst
  quickCheck prop_intSolveSubstitution
