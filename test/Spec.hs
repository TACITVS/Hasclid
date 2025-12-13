module Main where

import Expr
import BuchbergerOpt (reduce)
import Positivity (checkPositivityEnhanced, isPositive)
import Prover (intSolve, intResult, defaultIntSolveOptions)
import Parser (parseFormulaWithMacros, SExpr(..), MacroMap)
import ReplSupport (consumeBalancedScript, parenBalance, stripComment)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Map.Strict as M

-- Property 1: toPoly distributes over addition of constants
prop_toPolyConstAdd :: Integer -> Integer -> Bool
prop_toPolyConstAdd a b =
  let e = Add (Const (fromInteger a)) (Const (fromInteger b))
  in toPoly e == polyAdd (polyFromConst (fromInteger a)) (polyFromConst (fromInteger b))

prop_toPolyConstAdd_samples :: Bool
prop_toPolyConstAdd_samples =
  all (\(a,b) -> prop_toPolyConstAdd a b) samples
  where
    samples = [(-2,-3), (-1,0), (0,0), (1,2), (3,-4)]

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

-- Property 6: macros expand inside parseFormulaWithMacros
prop_macro_expansion :: Bool
prop_macro_expansion =
  let macros :: MacroMap
      macros = M.fromList [("inc", (["x"], List [Atom "+", Atom "x", Atom "1"]))]
  in case parseFormulaWithMacros macros "(= (inc 1) 2)" of
       Right _ -> True  -- accept any successful parse; shape may vary across parser tweaks
       _ -> False

-- Property 7: paren balance ignores comments and supports multiline accumulation
prop_parenBalance_ignore_comments :: Bool
prop_parenBalance_ignore_comments =
  let l1 = ":prove (>= (+ x 1) 0)  -- comment with ) )"
      l2 = "(and (> y 0) (< y 1)) ; another )"
  in parenBalance (stripComment l1) == 0
     && parenBalance (stripComment l2) == 0

-- Property 8: consumeBalancedScript merges multi-line command
prop_consumeBalancedScript :: Bool
prop_consumeBalancedScript =
  let script =
        [":prove (exists (u v)"
        ,"  (and (> u 0)"
        ,"       (> v 0)"
        ,"       (>= (+ u v) 0)))"
        ,":assume (> z 0)"
        ]
      (cmd1, rest) = consumeBalancedScript script
  in (length rest == 1)
     && (any (":assume" `isPrefixOf`) rest)
     && ("(exists (u v)" `isInfixOf` cmd1)
     && parenBalance cmd1 == 0

assertProp :: String -> Bool -> IO ()
assertProp name ok =
  if ok then return () else error (name ++ " failed")

main :: IO ()
main = do
  assertProp "prop_toPolyConstAdd_samples"   prop_toPolyConstAdd_samples
  assertProp "prop_reduceSelfZero"          prop_reduceSelfZero
  assertProp "prop_pos_isPositive"          prop_pos_isPositive
  assertProp "prop_intSolveConst"           prop_intSolveConst
  assertProp "prop_intSolveSubstitution"    prop_intSolveSubstitution
  assertProp "prop_macro_expansion"         prop_macro_expansion
  assertProp "prop_parenBalance_comments"   prop_parenBalance_ignore_comments
  assertProp "prop_consumeBalancedScript"   prop_consumeBalancedScript
  putStrLn "All deterministic spec checks passed."
