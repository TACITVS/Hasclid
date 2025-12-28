module IntervalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Interval
import Data.Ratio

instance Arbitrary Interval where
  arbitrary = do
    x <- arbitrary :: Gen Double
    y <- arbitrary :: Gen Double
    let l = min x y
    let u = max x y
    -- Avoid NaN/Infinity for basic tests
    if isNaN l || isInfinite l || isNaN u || isInfinite u
      then return fullRange
      else return $ hull (fromRationalI (toRational l)) (fromRationalI (toRational u))

-- | Check if a value is contained in an interval
inInterval :: Double -> Interval -> Bool
inInterval x (I l u) = x >= l && x <= u

spec :: Spec
spec = describe "Interval Arithmetic" $ do
  
  describe "Basic Properties" $ do
    it "constructs valid intervals from rationals" $ property $ 
      \r -> let I l u = fromRationalI r 
                d = fromRational r :: Double
            in d >= l && d <= u

    it "hull contains both points" $ property $ 
      \x y -> let h = hull (fromRationalI (toRational x)) (fromRationalI (toRational y))
                  dx = fromRational (toRational x) :: Double
                  dy = fromRational (toRational y) :: Double
              in inInterval dx h && inInterval dy h

  describe "Arithmetic Correctness (Inclusion Property)" $ do
    it "Addition preserves inclusion" $ property $ 
      \x y -> let ix = fromRationalI x
                  iy = fromRationalI y
                  res = addI ix iy
                  val = fromRational (x + y) :: Double
              in inInterval val res

    it "Subtraction preserves inclusion" $ property $ 
      \x y -> let ix = fromRationalI x
                  iy = fromRationalI y
                  res = subI ix iy
                  val = fromRational (x - y) :: Double
              in inInterval val res

    it "Multiplication preserves inclusion" $ property $ 
      \x y -> let ix = fromRationalI x
                  iy = fromRationalI y
                  res = mulI ix iy
                  val = fromRational (x * y) :: Double
              in inInterval val res

  describe "Transcendental Bounds" $ do
    it "Sin range is within [-1, 1]" $ property $ 
      \x -> let ix = fromRationalI x
                res = sinI ix
            in case res of 
                 I l u -> l >= -1.000000001 && u <= 1.000000001

    it "Cos range is within [-1, 1]" $ property $ 
      \x -> let ix = fromRationalI x
                res = cosI ix
            in case res of 
                 I l u -> l >= -1.000000001 && u <= 1.000000001
