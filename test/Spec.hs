module Main where

import Test.Hspec
import Test.QuickCheck
import Expr
import BuchbergerOpt (reduce)
import Positivity (checkPositivityEnhanced, isPositive)
import Prover (intSolve, intResult, defaultIntSolveOptions)
import Parser (parseFormulaWithMacros, SExpr(..), MacroMap)
import ReplSupport (consumeBalancedScript, parenBalance, stripComment)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Polynomial Arithmetic" $ do
    it "toPoly distributes over constant addition (samples)" $ do
      let prop a b = toPoly (Add (Const (fromInteger a)) (Const (fromInteger b))) ==
                     polyAdd (polyFromConst (fromInteger a)) (polyFromConst (fromInteger b))
          samples = [(-2,-3), (-1,0), (0,0), (1,2), (3,-4)]
      all (\(a,b) -> prop a b) samples `shouldBe` True

    it "reduce self-reduces to zero" $ do
      let p = toPoly (Add (Var "x") (Const 1))
      reduce compare p [p] `shouldBe` polyZero

  describe "Positivity Checker" $ do
    it "detects simple positive polynomial x^2 + 1" $ do
      let p = polyAdd (polyPow (polyFromVar "x") 2) (polyFromConst 1)
      isPositive (checkPositivityEnhanced p True) `shouldBe` True

  describe "Integer Solver" $ do
    it "proves trivial integer equality 1 = 1" $ do
      let goal = Eq (IntConst 1) (IntConst 1)
          outcome = intSolve defaultIntSolveOptions [] goal
      intResult outcome `shouldBe` Just True

    it "propagates integer substitution from theory" $ do
      let th = [Eq (IntVar "x") (IntConst 2)]
          goal = Eq (Add (IntVar "x") (IntConst 1)) (IntConst 3)
          outcome = intSolve defaultIntSolveOptions th goal
      intResult outcome `shouldBe` Just True

  describe "Parser" $ do
    it "expands macros inside parseFormulaWithMacros" $ do
      let macros :: MacroMap
          macros = M.fromList [("inc", (["x"], List [Atom "+", Atom "x", Atom "1"]))]
      parseFormulaWithMacros macros S.empty "(= (inc 1) 2)" `shouldSatisfy` isRight

    context "when handling comments and multi-line input" $ do
      it "ignores comments in paren balance" $ do
        let l1 = ":prove (>= (+ x 1) 0)  -- comment with ) )"
            l2 = "(and (> y 0) (< y 1)) ; another )"
        parenBalance (stripComment l1) `shouldBe` 0
        parenBalance (stripComment l2) `shouldBe` 0

      it "consumes balanced multi-line commands" $ do
        let script =
              [":prove (exists (u v)"
              ,"  (and (> u 0)"
              ,"       (> v 0)"
              ,"       (>= (+ u v) 0)))"
              ,":assume (> z 0)"
              ]
            (cmd1, rest) = consumeBalancedScript script
        length rest `shouldBe` 1
        any (":assume" `isPrefixOf`) rest `shouldBe` True
        ("(exists (u v)" `isInfixOf` cmd1) `shouldBe` True
        parenBalance cmd1 `shouldBe` 0

-- Helper function for Either checks
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
