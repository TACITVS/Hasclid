module Main where

import F5
import Expr
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec
import TermOrder (TermOrder(..), compareMonomials)
import Data.Maybe (fromMaybe)

-- Helper: Create Monomial from variable list [("x", 1), ("y", 2)] -> x*y^2
mkMono :: [(String, Int)] -> Monomial
mkMono vars = Monomial (M.fromList [ (v, fromIntegral p) | (v, p) <- vars ])

-- Helper: Create Poly
mkPoly :: [(String, Int)] -> Rational -> Poly
mkPoly vars c = Poly (M.singleton (mkMono vars) c)

main :: IO ()
main = hspec $ do
  let ord = compareMonomials GrevLex

  describe "F5 Data Structures" $ do
    it "Compares signatures correctly (POT)" $ do
      let s1 = Signature 2 (mkMono [])
          s2 = Signature 1 (mkMono [])
      -- 1 < 2 => LT => s2 < s1
      compareSignatures ord s1 s2 `shouldBe` GT
      compareSignatures ord s2 s1 `shouldBe` LT

    it "Compares signatures by term if index equal" $ do
      let sx = Signature 1 (mkMono [("x", 1)])
          sy = Signature 1 (mkMono [("y", 1)])
      let termCmp = ord (mkMono [("x", 1)]) (mkMono [("y", 1)])
      compareSignatures ord sx sy `shouldBe` termCmp

  describe "F5 Criteria" $ do
    it "Syzygy Criterion: Identifies divisible signatures" $ do
      let rules = M.singleton 1 [mkMono [("x", 1)]]
      let sTest = Signature 1 (mkMono [("x", 1), ("y", 1)])
      isSyzygy ord rules sTest `shouldBe` True

      let sTest2 = Signature 1 (mkMono [("y", 1)])
      isSyzygy ord rules sTest2 `shouldBe` False

    it "Rewritable Criterion: Skips if rewritable by previous basis" $ do
      -- Basis has g1 from f1 (index 1) with LT = x
      let g1 = LabeledPoly 
                 { lpPoly = mkPoly [("x", 1)] 1
                 , lpSig = Signature 1 (mkMono [])
                 , lpLead = Just (mkMono [("x", 1)], 1)
                 , lpNum = 1
                 , lpIsHypothesis = True
                 }
      let basis = [g1]
      
      -- We check signature u*e_2
      -- If u = x*y, it is divisible by LT(g1)=x, so it is rewritable.
      -- (Since index(g1) = 1 < 2)
      
      let sigRewritable = Signature 2 (mkMono [("x", 1), ("y", 1)])
      isRewritable ord basis sigRewritable `shouldBe` True
      
      -- If u = y, it is not divisible by x
      let sigOk = Signature 2 (mkMono [("y", 1)])
      isRewritable ord basis sigOk `shouldBe` False
      
      -- If u = x, but index is 1?
      -- Rewritable checks against basis elements with index < idx.
      -- So for Signature 1 ..., there are no "previous" basis elements with index < 1.
      let sigSelf = Signature 1 (mkMono [("x", 1)])
      isRewritable ord basis sigSelf `shouldBe` False


  describe "End-to-End F5" $ do
    it "Solves <x, y> -> [x, y]" $ do
      let x = mkPoly [("x", 1)] 1
          y = mkPoly [("y", 1)] 1
          gb = f5Solve ord [x, y]
      length gb `shouldBe` 2
      
    it "Solves trivial reduction <x, x> -> [x]" $ do
      let x1 = mkPoly [("x", 1)] 1
          x2 = mkPoly [("x", 1)] 1
          gb = f5Solve ord [x1, x2]
      -- With inter-reduction, should be 1
      length gb `shouldBe` 1

    it "Solves Cyclic-3 (sanity check)" $ do
      let x = mkPoly [("x", 1)] 1
          y = mkPoly [("y", 1)] 1
          z = mkPoly [("z", 1)] 1
          f1 = polyAdd (polyAdd x y) z
          xy = polyMul x y
          yz = polyMul y z
          zx = polyMul z x
          f2 = polyAdd (polyAdd xy yz) zx
          xyz = polyMul xy z
          f3 = polyAdd xyz (mkPoly [] (-1))
          
          gb = f5Solve ord [f1, f2, f3]
          
      length gb `shouldSatisfy` (> 0)
