{-|
Module: Positivity.Numerical
Description: Numerical (Double-precision) polynomial operations for fast SOS checking

This module provides numerical approximations for polynomial operations,
useful for fast feasibility checks before expensive symbolic computations.

Note: Results from numerical checks should always be verified symbolically
before being reported as proofs, due to floating-point precision issues.
-}
module Positivity.Numerical
  ( -- * Numerical Polynomial Type
    PolyD(..)
    -- * Conversion Functions
  , fromPoly
  , reconstructPoly
  , safeFromRational
    -- * Numerical SOS Checking
  , checkSOSNumeric
  , evaluatePolyD
  , isNumericallyPositive
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.Ratio (numerator, denominator, (%))
import Data.List (foldl')

-- | Double-precision polynomial representation for fast numerical operations.
-- Uses the same Monomial type as exact polynomials for compatibility.
newtype PolyD = PolyD (M.Map Monomial Double)
  deriving (Show, Eq)

-- | Numerical tolerance for comparisons
epsilon :: Double
epsilon = 1e-10

-- =============================================
-- Conversion Functions
-- =============================================

-- | Convert an exact rational polynomial to double-precision.
-- The variable substitution map allows evaluating some variables to constants.
fromPoly :: M.Map String Double -> Poly -> PolyD
fromPoly varSubst (Poly m) = PolyD $ M.fromListWith (+)
  [ (partialMono, evalCoeff * partialVal)
  | (mono, coeff) <- M.toList m
  , let evalCoeff = safeFromRational coeff
  , let (partialMono, partialVal) = partialEvalMonomial varSubst mono
  , abs evalCoeff > epsilon -- Filter near-zero terms
  ]

-- | Partially evaluate a monomial with given variable values.
-- Returns (remaining monomial, product of evaluated variables)
partialEvalMonomial :: M.Map String Double -> Monomial -> (Monomial, Double)
partialEvalMonomial varSubst (Monomial vars) =
  let (evalPairs, keepPairs) = M.partitionWithKey (\k _ -> M.member k varSubst) vars
      evalVal = M.foldlWithKey (\acc v e -> acc * (varSubst M.! v) ** fromIntegral e) 1.0 evalPairs
  in (Monomial keepPairs, evalVal)

-- | Reconstruct an exact polynomial from a numerical one.
-- Attempts to rationalize coefficients using continued fraction approximation.
--
-- The `prefix` argument is used to rename variables (e.g., for auxiliary variables).
-- The `scale` argument allows scaling all coefficients.
reconstructPoly :: String -> Double -> PolyD -> Poly
reconstructPoly prefix scale (PolyD m) =
  Poly $ M.fromList
    [ (renameMono prefix mono, rationalizeDouble (c * scale))
    | (mono, c) <- M.toList m
    , abs c > epsilon  -- Skip near-zero terms
    ]

-- | Rename variables in a monomial by adding a prefix.
renameMono :: String -> Monomial -> Monomial
renameMono "" mono = mono
renameMono prefix (Monomial vars) = Monomial $ M.mapKeys (prefix ++) vars

-- | Convert Rational to Double safely, handling very large/small rationals.
safeFromRational :: Rational -> Double
safeFromRational r =
  let n = numerator r
      d = denominator r
  in if d == 0
     then 0.0  -- Division by zero - should not happen with valid rationals
     else if abs n > 10^(300 :: Int) || abs d > 10^(300 :: Int)
          then fromRational (n `quot` (10^(100 :: Int)) % (d `quot` (10^(100 :: Int))))  -- Scale down
          else fromRational r

-- | Approximate a Double as a Rational using continued fraction expansion.
-- Handles the common case where the double represents an exact rational.
rationalizeDouble :: Double -> Rational
rationalizeDouble x
  | isNaN x || isInfinite x = 0
  | abs x < epsilon = 0
  | otherwise = continuedFractionApprox x 20

-- | Continued fraction approximation with limited iterations.
continuedFractionApprox :: Double -> Int -> Rational
continuedFractionApprox x maxIter = go x maxIter (1, 0) (0, 1)
  where
    go _ 0 (h1, _) (h2, _) = h1 % h2
    go y n (h1, h2) (k1, k2)
      | abs y < epsilon = h1 % k1
      | otherwise =
          let a = floor y
              newH = fromIntegral a * h1 + h2
              newK = fromIntegral a * k1 + k2
              remainder = y - fromIntegral a
          in if abs remainder < epsilon || newK == 0
             then newH % newK
             else go (1 / remainder) (n - 1) (newH, h1) (newK, k1)

-- =============================================
-- Numerical SOS Checking
-- =============================================

-- | Check if a polynomial is numerically sum-of-squares.
-- This is a fast heuristic check using sampling - does NOT provide a certificate.
--
-- Returns Just [squares] if the polynomial appears to be SOS,
-- Nothing if it might be negative somewhere.
--
-- WARNING: This is only a heuristic. A "Just" result should be verified symbolically.
checkSOSNumeric :: M.Map String Double -> Poly -> Maybe [PolyD]
checkSOSNumeric varSubst poly =
  let polyD = fromPoly varSubst poly
  in if isNumericallyPositive polyD
     then Just [polyD]  -- Return the polynomial itself as the "certificate"
     else Nothing

-- | Evaluate a numerical polynomial at given variable values.
evaluatePolyD :: M.Map String Double -> PolyD -> Double
evaluatePolyD varVals (PolyD m) = sum
  [ coeff * evalMonomial varVals mono
  | (mono, coeff) <- M.toList m
  ]

-- | Evaluate a monomial at given variable values.
evalMonomial :: M.Map String Double -> Monomial -> Double
evalMonomial varVals (Monomial vars) = M.foldlWithKey evalVar 1.0 vars
  where
    evalVar acc var exp' =
      let val = M.findWithDefault 1.0 var varVals
      in acc * (val ** fromIntegral exp')

-- | Check if a polynomial is numerically positive using sampling.
-- Samples at various points in the domain and checks for negativity.
isNumericallyPositive :: PolyD -> Bool
isNumericallyPositive (PolyD m)
  | M.null m = True  -- Zero polynomial is non-negative
  | otherwise =
      let vars = extractVarsD (PolyD m)
      in if null vars
         then  -- Constant polynomial
           case M.toList m of
             ((Monomial _, c):_) -> c >= -epsilon
             [] -> True  -- Empty map, treat as zero
         else
           -- Sample at various points
           all (>= -epsilon) $ map (evaluatePolyD' m) (samplePoints vars)

-- | Internal evaluation function for sampling
evaluatePolyD' :: M.Map Monomial Double -> M.Map String Double -> Double
evaluatePolyD' m varVals = sum
  [ coeff * evalMonomial varVals mono
  | (mono, coeff) <- M.toList m
  ]

-- | Extract variables from a numerical polynomial.
extractVarsD :: PolyD -> [String]
extractVarsD (PolyD m) =
  M.keys $ M.unions [vars | (Monomial vars, _) <- M.toList m]

-- | Generate sample points for positivity checking.
-- Uses a grid of positive values (since we typically deal with non-negative variables).
samplePoints :: [String] -> [M.Map String Double]
samplePoints vars =
  let values = [0.1, 0.5, 1.0, 2.0, 5.0]  -- Sample values
      n = length vars
  in if n <= 3
     then  -- Full grid for small dimensions
       [ M.fromList (zip vars vals)
       | vals <- replicateM n values
       ]
     else  -- Limited sampling for higher dimensions
       [ M.fromList (zip vars vals)
       | vals <- take 500 $ replicateM n values  -- Limit total samples
       ]

-- | Monadic replicateM for lists (generate all combinations)
replicateM :: Int -> [a] -> [[a]]
replicateM 0 _ = [[]]
replicateM n xs = [y:ys | y <- xs, ys <- replicateM (n-1) xs]

-- =============================================
-- Polynomial Arithmetic for PolyD
-- =============================================

-- | Add two numerical polynomials.
addPolyD :: PolyD -> PolyD -> PolyD
addPolyD (PolyD m1) (PolyD m2) = PolyD $ M.filter (\c -> abs c > epsilon) $
  M.unionWith (+) m1 m2

-- | Multiply two numerical polynomials.
mulPolyD :: PolyD -> PolyD -> PolyD
mulPolyD (PolyD m1) (PolyD m2) = PolyD $ M.filter (\c -> abs c > epsilon) $
  M.fromListWith (+)
    [ (mulMono mon1 mon2, c1 * c2)
    | (mon1, c1) <- M.toList m1
    , (mon2, c2) <- M.toList m2
    ]
  where
    mulMono (Monomial v1) (Monomial v2) = Monomial (M.unionWith (+) v1 v2)

-- | Scale a numerical polynomial by a constant.
scalePolyD :: Double -> PolyD -> PolyD
scalePolyD s (PolyD m) = PolyD $ M.filter (\c -> abs c > epsilon) $ M.map (* s) m
