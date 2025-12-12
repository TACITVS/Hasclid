module Positivity.SOS
  ( checkSOS
  , isPerfectSquare
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.Ratio (numerator, denominator)

-- | Check if polynomial is a Sum of Squares (Basic)
checkSOS :: Poly -> Bool
checkSOS p = isWeightedSOS p || isPerfectSquare p

-- | Check for weighted sum of even powers (trivial SOS)
-- P = c1*m1^2 + c2*m2^2 + ... (all ci > 0)
isWeightedSOS :: Poly -> Bool
isWeightedSOS (Poly m) =
  not (M.null m) &&
  all (\(Monomial vars, c) -> c > 0 && all even (M.elems vars)) (M.toList m)

-- | Check if polynomial is a perfect square (P = Q^2)
isPerfectSquare :: Poly -> Bool
isPerfectSquare p =
  case polynomialSqrt p of
    Just _ -> True
    Nothing -> False

-- | Compute exact square root of polynomial if it exists
polynomialSqrt :: Poly -> Maybe Poly
polynomialSqrt p
  | p == polyZero = Just polyZero
  | otherwise =
      case getLeadingTerm p of
        Nothing -> Just polyZero
        Just (ltM, ltC) ->
          -- LT(P) must be a square
          if not (isSquareMono ltM) || not (isSquareRat ltC)
          then Nothing
          else
            let rootM = sqrtMono ltM
                rootC = sqrtRat ltC
                rootLT = polyFromMonomial rootM rootC
            in findRoot p rootLT

-- Helper to iteratively find root
findRoot :: Poly -> Poly -> Maybe Poly
findRoot target currentRoot =
  let remainder = polySub target (polyMul currentRoot currentRoot)
  in if remainder == polyZero
     then Just currentRoot
     else
       case getLeadingTerm remainder of
         Nothing -> Just currentRoot
         Just (remLT_M, remLT_C) ->
           case getLeadingTerm currentRoot of
             Nothing -> Nothing
             Just (rootLT_M, rootLT_C) ->
               -- next_term = LT(Rem) / (2 * LT(Root))
               -- If not divisible, fail
               let factor = 2 * rootLT_C
                   nextC = remLT_C / factor
                   nextM = monomialDiv remLT_M rootLT_M
               in case nextM of
                    Nothing -> Nothing -- Division failed
                    Just m ->
                      let nextTerm = polyFromMonomial m nextC
                          
                          -- Heuristic check: new term must be "smaller" than current root leading term
                          -- to ensure convergence/termination in term order
                          isSmaller = m < rootLT_M
                      in if not isSmaller
                         then Nothing -- Divergence detection (should strictly decrease)
                         else findRoot target (polyAdd currentRoot nextTerm)

polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = Poly (M.singleton m c)

isSquareMono :: Monomial -> Bool
isSquareMono (Monomial m) = all even (M.elems m)

sqrtMono :: Monomial -> Monomial
sqrtMono (Monomial m) = Monomial (M.map (`div` 2) m)

isSquareRat :: Rational -> Bool
isSquareRat r =
  let n = numerator r
      d = denominator r
  in r >= 0 && isSquare n && isSquare d

isSquare :: Integer -> Bool
isSquare x 
  | x < 0 = False
  | otherwise =
      let s = round (sqrt (fromIntegral x :: Double))
      in s * s == x

sqrtRat :: Rational -> Rational
sqrtRat r =
  let n = numerator r
      d = denominator r
  in (toRational (round (sqrt (fromIntegral n :: Double)))) / (toRational (round (sqrt (fromIntegral d :: Double))))
