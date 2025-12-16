module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Concurrent (threadDelay)
import Expr
import BuchbergerOpt (reduce)
import Positivity (checkPositivityEnhanced, isPositive)
import Prover (intSolve, intResult, defaultIntSolveOptions)
import Parser (parseFormulaWithMacros, SExpr(..), MacroMap)
import ReplSupport (consumeBalancedScript, parenBalance, stripComment)
import Timeout
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

  describe "Expression Simplification" $ do
    describe "division-by-zero handling" $ do
      it "detects division by literal zero (Const)" $ do
        let expr = Div (Const 1) (Const 0)
        evaluate (simplifyExpr expr) `shouldThrow` anyException

      it "detects division by literal zero (IntConst)" $ do
        let expr = Div (IntConst 5) (IntConst 0)
        evaluate (simplifyExpr expr) `shouldThrow` anyException

      it "detects modulo by literal zero (Const)" $ do
        let expr = Mod (Const 5) (Const 0)
        evaluate (simplifyExpr expr) `shouldThrow` anyException

      it "detects modulo by literal zero (IntConst)" $ do
        let expr = Mod (IntConst 10) (IntConst 0)
        evaluate (simplifyExpr expr) `shouldThrow` anyException

      it "allows symbolic division" $ do
        let expr = Div (Const 1) (Var "x")
        simplifyExpr expr `shouldBe` Div (Const 1) (Var "x")

      it "simplifies division by non-zero constant" $ do
        simplifyExpr (Div (Const 6) (Const 2)) `shouldBe` Const 3

      it "simplifies nested division correctly" $ do
        simplifyExpr (Div (Div (Const 12) (Const 3)) (Const 2))
          `shouldBe` Const 2

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

    it "detects infinite macro recursion" $ do
      let macros :: MacroMap
          macros = M.fromList [("inf", ([], List [Atom "inf"]))]
      parseFormulaWithMacros macros S.empty "(= (inf) 0)" `shouldSatisfy` isLeft

    it "allows deep but finite macro expansion" $ do
      -- Create a chain of macros that expands deeply but finitely
      let macros :: MacroMap
          macros = M.fromList [("nest", (["x"], List [Atom "+", Atom "x", Atom "1"]))]
          -- (nest (nest (nest ... (nest 0)...))) with many levels
          deepExpr = foldl (\acc _ -> "(nest " ++ acc ++ ")") "0" [1..100::Int]
      parseFormulaWithMacros macros S.empty ("(= " ++ deepExpr ++ " 100)") `shouldSatisfy` isRight

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

  describe "Timeout Infrastructure" $ do
    it "creates timeout context with deadline" $ do
      ctx <- withTimeout 1.0
      deadline ctx `shouldSatisfy` isJust

    it "creates no-timeout context" $ do
      ctx <- noTimeout
      deadline ctx `shouldBe` Nothing

    it "detects timeout expiration" $ do
      ctx <- withTimeout 0.1  -- 100ms timeout
      threadDelay 150000      -- Sleep 150ms
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` True

    it "does not timeout before deadline" $ do
      ctx <- withTimeout 1.0  -- 1 second timeout
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` False

    it "no-timeout never times out" $ do
      ctx <- noTimeout
      threadDelay 10000       -- Sleep 10ms
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` False

-- Helper functions for Either and Maybe checks
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
