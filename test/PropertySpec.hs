{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : PropertySpec
-- Description : Property-based tests using QuickCheck
--
-- This module contains property-based tests that verify algebraic
-- invariants of the polynomial and expression types.

module PropertySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Test.QuickCheck
import Expr
import qualified Data.Map.Strict as M
import Data.Ratio ((%))
import Numeric.Natural (Natural)

-- =============================================
-- Generators for small, fast tests
-- =============================================

-- | Generate small rationals to keep tests fast
genSmallRational :: Gen Rational
genSmallRational = do
  num <- choose (-5, 5) :: Gen Integer
  den <- choose (1, 3) :: Gen Integer
  return (num % den)

-- | Generate variable names
genVarName :: Gen String
genVarName = elements ["x", "y", "z"]

-- | Generate very simple polynomials (constant or single variable)
genSimplePoly :: Gen Poly
genSimplePoly = oneof
  [ polyFromConst <$> genSmallRational
  , polyFromVar <$> genVarName
  , do a <- genSmallRational
       v <- genVarName
       return $ polyAdd (polyFromConst a) (polyFromVar v)
  ]

instance Arbitrary Poly where
  arbitrary = genSimplePoly

-- =============================================
-- Polynomial Properties (fast subset)
-- =============================================

spec :: Spec
spec = modifyMaxSuccess (const 20) $ do  -- Only 20 tests per property
  describe "Polynomial Algebraic Properties" $ do

    describe "Addition" $ do
      prop "is commutative: p + q == q + p" $ \(p :: Poly) (q :: Poly) ->
        polyAdd p q == polyAdd q p

      prop "has zero as identity: p + 0 == p" $ \(p :: Poly) ->
        polyAdd p polyZero == p

      prop "negation is additive inverse: p + (-p) == 0" $ \(p :: Poly) ->
        polyAdd p (polyNeg p) == polyZero

    describe "Multiplication" $ do
      prop "is commutative: p * q == q * p" $ \(p :: Poly) (q :: Poly) ->
        polyMul p q == polyMul q p

      prop "has one as identity: p * 1 == p" $ \(p :: Poly) ->
        polyMul p (polyFromConst 1) == p

      prop "zero annihilates: p * 0 == 0" $ \(p :: Poly) ->
        polyMul p polyZero == polyZero

    describe "Subtraction" $ do
      prop "p - p == 0" $ \(p :: Poly) ->
        polySub p p == polyZero

    describe "Power" $ do
      it "p^0 == 1 for x" $ do
        polyPow (polyFromVar "x") 0 `shouldBe` polyFromConst 1

      it "p^1 == p for x" $ do
        polyPow (polyFromVar "x") 1 `shouldBe` polyFromVar "x"

      it "p^2 == p * p for x" $ do
        let p = polyFromVar "x"
        polyPow p 2 `shouldBe` polyMul p p

  describe "Expression to Polynomial Conversion" $ do

    it "constants convert correctly" $ do
      toPoly (Const (3 % 2)) `shouldBe` polyFromConst (3 % 2)

    it "variables convert correctly" $ do
      toPoly (Var "x") `shouldBe` polyFromVar "x"

    it "addition is homomorphic" $ do
      let e1 = Var "x"
          e2 = Const 2
      toPoly (Add e1 e2) `shouldBe` polyAdd (toPoly e1) (toPoly e2)

    it "multiplication is homomorphic" $ do
      let e1 = Var "x"
          e2 = Var "y"
      toPoly (Mul e1 e2) `shouldBe` polyMul (toPoly e1) (toPoly e2)

  describe "Expression Simplification" $ do

    it "is idempotent for simple expressions" $ do
      let e = Add (Var "x") (Const 0)
      simplifyExpr (simplifyExpr e) `shouldBe` simplifyExpr e

    it "preserves polynomial meaning" $ do
      let e = Add (Var "x") (Mul (Const 2) (Var "y"))
      toPoly (simplifyExpr e) `shouldBe` toPoly e

    it "simplifies 0 + x to x" $ do
      simplifyExpr (Add (Const 0) (Var "x")) `shouldBe` Var "x"

    it "simplifies x + 0 to x" $ do
      simplifyExpr (Add (Var "x") (Const 0)) `shouldBe` Var "x"

    it "simplifies 0 * x to 0" $ do
      simplifyExpr (Mul (Const 0) (Var "x")) `shouldBe` Const 0

    it "simplifies 1 * x to x" $ do
      simplifyExpr (Mul (Const 1) (Var "x")) `shouldBe` Var "x"

    it "simplifies x - x to 0" $ do
      simplifyExpr (Sub (Var "x") (Var "x")) `shouldBe` Const 0

    it "simplifies x^0 to 1" $ do
      simplifyExpr (Pow (Var "x") 0) `shouldBe` Const 1

    it "simplifies x^1 to x" $ do
      simplifyExpr (Pow (Var "x") 1) `shouldBe` Var "x"

  describe "Polynomial Queries" $ do

    it "getVars on variable polynomial returns that variable" $ do
      getVars (polyFromVar "x") `shouldBe` M.keysSet (M.singleton "x" (1 :: Natural))

    it "getVars on constant is empty" $ do
      getVars (polyFromConst 5) `shouldBe` M.keysSet (M.empty :: M.Map String Natural)

    it "isConstPoly correctly identifies constants" $ do
      isConstPoly (polyFromConst 42) `shouldBe` True

    it "isConstPoly correctly identifies non-constants" $ do
      isConstPoly (polyFromVar "x") `shouldBe` False

  describe "Univariate Conversion" $ do

    it "univariate round-trip for x + 2x^2" $ do
      let poly = polyAdd (polyFromVar "x") (polyMul (polyFromConst 2) (polyPow (polyFromVar "x") 2))
      case toUnivariate poly of
        Just ("x", coeffs) -> coeffs `shouldBe` [0, 1, 2]
        _ -> expectationFailure "Expected univariate polynomial in x"

    it "constant polynomial is univariate" $ do
      case toUnivariate (polyFromConst 5) of
        Just (_, [c]) -> c `shouldBe` 5
        _ -> expectationFailure "Expected univariate constant"

    it "multivariate polynomial is not univariate" $ do
      let poly = polyMul (polyFromVar "x") (polyFromVar "y")
      toUnivariate poly `shouldBe` Nothing

  describe "Polynomial Content and Primitive Part" $ do

    it "primitive part of 2x + 4 has content 1" $ do
      let p = polyAdd (polyMul (polyFromConst 2) (polyFromVar "x")) (polyFromConst 4)
          pp = polyPrimitive p
      polyContent pp `shouldBe` 1

    it "content of 6x^2 + 3x is 3" $ do
      let p = polyAdd (polyMul (polyFromConst 6) (polyPow (polyFromVar "x") 2))
                      (polyMul (polyFromConst 3) (polyFromVar "x"))
      polyContent p `shouldBe` 3
