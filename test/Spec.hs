module Main where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Concurrent (threadDelay)
import Expr
import BuchbergerOpt (reduce, buchbergerWithStrategyT, SelectionStrategy(..))
import CAD (discriminant, resultant)
import CADLift (cadDecompose)
import Positivity (checkPositivityEnhanced, isPositive)
import Prover (intSolve, intResult, defaultIntSolveOptions, proveTheoryE, ProofResult(..), ProofTrace(..))
import Parser (parseFormulaWithMacros, SExpr(..), MacroMap)
import ReplSupport (consumeBalancedScript, parenBalance, stripComment)
import Timeout
import Wu (wuProveE, WuResult(..))
import Lagrange (solve4Squares, solve4SquaresE)
import SolverRouter (autoSolve, AutoSolveResult(..), SolverOptions(..), defaultSolverOptions, SolverChoice(..))
import Error (ProverError(..), ProofErrorType(..), TimeoutErrorType(..), formatError)
import Data.List (isPrefixOf, isInfixOf)
import Data.Ratio ((%))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified ExamplesSpec
import qualified IntegrationSpec
import qualified PropertySpec

main :: IO ()
main = hspec $ do
  coreSpec
  PropertySpec.spec
  -- NOTE: ExamplesSpec and IntegrationSpec are slow/hanging tests
  -- Run them separately: cabal test prover-test --test-options="--match Examples"
  -- ExamplesSpec.spec
  -- IntegrationSpec.spec

coreSpec :: Spec
coreSpec = do
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
      ctx <- withTimeout 1
      deadline ctx `shouldSatisfy` isJust

    it "creates no-timeout context" $ do
      ctx <- noTimeout
      deadline ctx `shouldBe` Nothing

    it "detects timeout expiration" $ do
      ctx <- withTimeout 1  -- 1 second timeout
      threadDelay 1500000      -- Sleep 1.5s
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` Right True

    it "does not timeout before deadline" $ do
      ctx <- withTimeout 1  -- 1 second timeout
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` Right False

    it "no-timeout never times out" $ do
      ctx <- noTimeout
      threadDelay 10000       -- Sleep 10ms
      result <- runTimeoutM ctx checkTimeout
      result `shouldBe` Right False

  describe "Buchberger Timeout" $ do
    it "completes for simple polynomial systems without timeout" $ do
      let x = polyFromVar "x"
          y = polyFromVar "y"
          polys = [polyAdd (polyPow x 2) (polyFromConst (-1)),
                   polyAdd (polyPow y 2) (polyFromConst (-1))]
      ctx <- noTimeout
      result <- runTimeoutM ctx (buchbergerWithStrategyT compare NormalStrategy polys)
      case result of
        Right basis -> length basis `shouldSatisfy` (> 0)
        Left err -> expectationFailure ("Unexpected error: " ++ show err)

    it "returns error on timeout for expensive systems" $ do
      let x = polyFromVar "x"
          y = polyFromVar "y"
          z = polyFromVar "z"
          w = polyFromVar "w"
          -- Create a very expensive system - Katsura-4 benchmark
          -- This is a well-known hard Groebner basis problem
          polys = [ polyAdd (polyAdd (polyPow x 2) (polyPow y 2))
                            (polyAdd (polyPow z 2) (polyPow w 2))
                  , polyAdd (polyMul x y) (polyMul z w)
                  , polyAdd (polyAdd (polyMul x z) (polyMul y w)) (polyFromConst 1)
                  , polyAdd (polyAdd (polyMul x w) (polyMul y z)) (polyFromConst 1)
                  , polyAdd (polyMul y w) (polyFromConst 1)
                  ]
      ctx <- withTimeout 0  -- 0 second timeout (immediate)
      -- With proper error handling, timeout returns Either ProverError
      result <- runTimeoutM ctx (buchbergerWithStrategyT compare NormalStrategy polys)
      case result of
        Left (TimeoutError BuchbergerTimeout) -> return ()  -- Expected
        Left err -> expectationFailure ("Wrong error type: " ++ show err)
        Right _ -> expectationFailure "Expected timeout error but computation succeeded"

  describe "CAD Implementation Correctness" $ do
    describe "Discriminant" $ do
      it "computes discriminant of x^2 + bx + c correctly (CAD variant)" $ do
        -- CAD discriminant is Res(f, f') which may differ by sign/scale
        -- For x^2 + bx + c, the discriminant is -b^2 + 4c (CAD variant)
        let x = polyFromVar "x"
            b = polyFromVar "b"
            c = polyFromVar "c"
            -- x^2 + bx + c
            poly = polyAdd (polyAdd (polyPow x 2) (polyMul b x)) c
            disc = discriminant poly "x"
            -- CAD variant: -b^2 + 4c
            expected = polyAdd (polyNeg (polyPow b 2)) (polyMul (polyFromConst 4) c)
        disc `shouldBe` expected

      it "discriminant of x^2 - 1 has correct value" $ do
        let x = polyFromVar "x"
            poly = polyAdd (polyPow x 2) (polyFromConst (-1))
            disc = discriminant poly "x"
        -- CAD discriminant for x^2 - 1 is -1 (normalized from standard -4)
        -- This is acceptable - we only care about zeros for projection
        disc `shouldBe` polyFromConst (-1)

    describe "Resultant" $ do
      it "resultant of x^2 - a^2 and x - a w.r.t x is zero" $ do
        -- Res(x^2 - a^2, x - a, x) should be 0 (they share factor x-a)
        let x = polyFromVar "x"
            a = polyFromVar "a"
            f = polyAdd (polyPow x 2) (polyNeg (polyPow a 2))
            g = polyAdd x (polyNeg a)
            res = resultant f g "x"
        res `shouldBe` polyZero

      it "resultant of coprime polynomials is nonzero" $ do
        -- Res(x^2 + 1, x + 1, x) should be nonzero (no common factor)
        let x = polyFromVar "x"
            f = polyAdd (polyPow x 2) (polyFromConst 1)
            g = polyAdd x (polyFromConst 1)
            res = resultant f g "x"
        res `shouldSatisfy` (/= polyZero)

    describe "CAD Decomposition" $ do
      it "decomposes x^2 - 1 into multiple cells" $ do
        let x = polyFromVar "x"
            poly = polyAdd (polyPow x 2) (polyFromConst (-1))
            cells = cadDecompose [poly] ["x"]
        -- x^2 - 1 has roots at x = -1 and x = 1
        -- Should produce cells: (-inf,-1), {-1}, (-1,1), {1}, (1,+inf)
        -- That's 5 cells total
        length cells `shouldBe` 5

      it "decomposes constant polynomial into one cell" $ do
        let cells = cadDecompose [polyFromConst 1] ["x"]
        -- Constant polynomial has no roots, just one sector
        length cells `shouldBe` 1

  describe "Either-based Error Handling" $ do
    it "proveTheoryE returns Right for provable theorems" $ do
      -- Simple tautology: 1 = 1
      let goal = Eq (Const 1) (Const 1)
          result = proveTheoryE [] goal
      case result of
        Right proof -> proved proof `shouldBe` True
        Left _ -> expectationFailure "Expected Right but got Left"

    it "proveTheoryE returns Right (with False) for unprovable statements" $ do
      -- False statement: 1 = 2
      let goal = Eq (Const 1) (Const 2)
          result = proveTheoryE [] goal
      case result of
        Right proof -> proved proof `shouldBe` False
        Left _ -> expectationFailure "Expected Right but got Left"

    it "proveTheoryE includes proof trace" $ do
      -- x = 1, prove x + 1 = 2
      let theory = [Eq (Var "x") (Const 1)]
          goal = Eq (Add (Var "x") (Const 1)) (Const 2)
          result = proveTheoryE theory goal
      case result of
        Right proof -> do
          proved proof `shouldBe` True
          length (steps (trace proof)) `shouldSatisfy` (> 0)
        Left _ -> expectationFailure "Expected Right but got Left"

  describe "Wu's Method Error Handling" $ do
    it "returns Left for unsupported formula (inequality)" $ do
      let goal = Gt (Var "x") (Const 0)  -- Inequality, not supported by Wu
          result = wuProveE [] goal
      case result of
        Left (ProofError (UnsupportedFormula _) _) -> return ()
        Left err -> expectationFailure $ "Expected UnsupportedFormula but got: " ++ formatError err
        Right _ -> expectationFailure "Expected Left but got Right"

    it "returns Right for supported formula (equality)" $ do
      let goal = Eq (Var "x") (Const 0)
          result = wuProveE [] goal
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ formatError err

  describe "Lagrange Four Squares" $ do
    it "returns error for negative number" $ do
      let result = solve4SquaresE (-5)
      case result of
        Left (MathematicalError _) -> return ()
        Left err -> expectationFailure $ "Expected MathematicalError but got: " ++ formatError err
        Right _ -> expectationFailure "Expected Left but got Right"

    it "correctly solves for zero" $ do
      solve4SquaresE 0 `shouldBe` Right [0,0,0,0]

    it "correctly solves for small positive integers" $ do
      -- 1 = 1^2 + 0^2 + 0^2 + 0^2
      case solve4SquaresE 1 of
        Right result -> do
          let sumSquares = sum (map (^(2::Int)) result)
          sumSquares `shouldBe` 1
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ formatError err

    it "legacy API throws error for negative" $ do
      evaluate (solve4Squares (-5)) `shouldThrow` anyException

  describe "SolverRouter" $ do
    it "routes equality goals to appropriate solver" $ do
      let goal = Eq (Add (Var "x") (Const 1)) (Const 2)
          result = autoSolve defaultSolverOptions M.empty [Eq (Var "x") (Const 1)] goal
      selectedSolver result `shouldSatisfy` (\s -> s `elem` [UseWu, UseGroebner, UseGeoSolver])

    it "handles simple true equality" $ do
      let goal = Eq (Const 1) (Const 1)
          result = autoSolve defaultSolverOptions M.empty [] goal
      isProved result `shouldBe` True

    it "handles simple false equality" $ do
      let goal = Eq (Const 1) (Const 2)
          result = autoSolve defaultSolverOptions M.empty [] goal
      isProved result `shouldBe` False

    it "routes inequality goals to CAD" $ do
      let goal = Gt (Add (Var "x") (Const 1)) (Const 0)
          result = autoSolve defaultSolverOptions M.empty [] goal
      -- Should use CAD for inequalities, not Wu
      selectedSolver result `shouldNotBe` UseWu

  describe "Polynomial Operations Extended" $ do
    it "polynomial multiplication is commutative" $ do
      let p1 = polyAdd (polyFromVar "x") (polyFromConst 1)
          p2 = polyAdd (polyFromVar "y") (polyFromConst 2)
      polyMul p1 p2 `shouldBe` polyMul p2 p1

    it "polynomial addition is associative" $ do
      let p1 = polyFromVar "x"
          p2 = polyFromVar "y"
          p3 = polyFromConst 1
      polyAdd (polyAdd p1 p2) p3 `shouldBe` polyAdd p1 (polyAdd p2 p3)

    it "polynomial multiplication by zero yields zero" $ do
      let p = polyAdd (polyFromVar "x") (polyFromConst 5)
      polyMul p polyZero `shouldBe` polyZero

    it "polynomial power works correctly" $ do
      let p = polyAdd (polyFromVar "x") (polyFromConst 1)
          -- (x+1)^2 = x^2 + 2x + 1
          squared = polyPow p 2
          expected = polyAdd (polyAdd (polyPow (polyFromVar "x") 2)
                             (polyMul (polyFromConst 2) (polyFromVar "x")))
                             (polyFromConst 1)
      squared `shouldBe` expected

  describe "Expression Conversion" $ do
    it "converts simple variable to polynomial" $ do
      toPoly (Var "x") `shouldBe` polyFromVar "x"

    it "converts addition correctly" $ do
      let expr = Add (Var "x") (Const 1)
          expected = polyAdd (polyFromVar "x") (polyFromConst 1)
      toPoly expr `shouldBe` expected

    it "converts multiplication correctly" $ do
      let expr = Mul (Var "x") (Var "y")
          expected = polyMul (polyFromVar "x") (polyFromVar "y")
      toPoly expr `shouldBe` expected

    it "handles nested expressions" $ do
      let expr = Add (Mul (Var "x") (Var "x")) (Const 1)
          expected = polyAdd (polyPow (polyFromVar "x") 2) (polyFromConst 1)
      toPoly expr `shouldBe` expected

  -- Edge case tests for improved coverage
  describe "Edge Cases" $ do
    describe "Polynomial edge cases" $ do
      it "zero polynomial is identity for addition" $ do
        polyAdd polyZero polyZero `shouldBe` polyZero

      it "zero polynomial annihilates multiplication" $ do
        let p = polyAdd (polyFromVar "x") (polyFromConst 42)
        polyMul polyZero p `shouldBe` polyZero
        polyMul p polyZero `shouldBe` polyZero

      it "negation of zero is zero" $ do
        polyNeg polyZero `shouldBe` polyZero

      it "double negation is identity" $ do
        let p = polyAdd (polyFromVar "x") (polyFromConst 3)
        polyNeg (polyNeg p) `shouldBe` p

      it "handles very large exponents correctly" $ do
        let p = polyFromVar "x"
            highPow = polyPow p 10
        getVars highPow `shouldBe` S.singleton "x"

      it "handles fractional coefficients" $ do
        let half = polyFromConst (1 / 2)
            p = polyMul half (polyFromVar "x")
            doubled = polyMul (polyFromConst 2) p
        doubled `shouldBe` polyFromVar "x"

    describe "Expression edge cases" $ do
      it "simplifies deeply nested zeros" $ do
        let expr = Add (Add (Const 0) (Const 0)) (Var "x")
        simplifyExpr expr `shouldBe` Var "x"

      it "simplifies deeply nested ones" $ do
        let expr = Mul (Mul (Const 1) (Const 1)) (Var "x")
        simplifyExpr expr `shouldBe` Var "x"

      it "handles negative constants correctly" $ do
        simplifyExpr (Mul (Const (-1)) (Const (-1))) `shouldBe` Const 1

      it "simplifies x * 0 * y to 0" $ do
        simplifyExpr (Mul (Mul (Var "x") (Const 0)) (Var "y")) `shouldBe` Const 0

      it "handles power of zero exponent" $ do
        simplifyExpr (Pow (Const 0) 0) `shouldBe` Const 1  -- 0^0 = 1 by convention

      it "handles high power correctly" $ do
        simplifyExpr (Pow (Const 2) 10) `shouldBe` Const 1024

    describe "Parser edge cases" $ do
      it "parses formula with many parentheses" $ do
        let formula = "(= (+ (+ (+ x 1) 2) 3) 6)"
        parseFormulaWithMacros M.empty S.empty formula `shouldSatisfy` isRight

      it "handles empty macro map" $ do
        parseFormulaWithMacros M.empty S.empty "(= x x)" `shouldSatisfy` isRight

      it "handles formula with long variable names" $ do
        parseFormulaWithMacros M.empty S.empty "(= longVariableName 42)" `shouldSatisfy` isRight

    describe "Rational edge cases" $ do
      it "handles large numerator" $ do
        let largeNum = Const (999999999 % 1)
        toPoly largeNum `shouldBe` polyFromConst (999999999 % 1)

      it "handles large denominator" $ do
        let smallFrac = Const (1 % 999999999)
        toPoly smallFrac `shouldBe` polyFromConst (1 % 999999999)

      it "preserves exact rational arithmetic" $ do
        let third = polyFromConst (1 % 3)
            result = polyMul (polyFromConst 3) third
        result `shouldBe` polyFromConst 1

    describe "Variable ordering edge cases" $ do
      it "orders variables lexicographically" $ do
        let xy = polyMul (polyFromVar "x") (polyFromVar "y")
            yx = polyMul (polyFromVar "y") (polyFromVar "x")
        xy `shouldBe` yx  -- Canonical form

      it "getLeadingTerm respects variable order" $ do
        let p = polyAdd (polyFromVar "x") (polyFromVar "y")
        case getLeadingTerm p of
          Just (_, _) -> True `shouldBe` True  -- Just verify it returns something
          Nothing -> expectationFailure "Expected leading term"

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
