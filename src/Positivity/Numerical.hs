module Positivity.Numerical
  ( checkSOSNumeric
  , PolyD(..)
  , fromPoly
  , reconstructPoly
  , safeFromRational
  ) where

import Expr
import qualified Data.Map.Strict as M

data PolyD = PolyD (M.Map Monomial Double) deriving (Show, Eq)

checkSOSNumeric :: M.Map String Double -> Poly -> Maybe [PolyD]
checkSOSNumeric _ _ = Nothing

fromPoly :: M.Map String Double -> Poly -> PolyD
fromPoly _ _ = PolyD M.empty

reconstructPoly :: String -> Double -> PolyD -> Poly
reconstructPoly _ _ _ = polyZero

safeFromRational :: Rational -> Double
safeFromRational _ = 0.0
