module Positivity.SOS
  ( checkSOS
  , checkSumOfSquares
  , isPerfectSquare
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (sortBy)
import Data.Ratio (numerator, denominator)
import TermOrder (compareMonomials, TermOrder(..))
import Positivity.SDP (checkSOS_SDP)

-- | Check if polynomial is a Sum of Squares, potentially modulo an ideal (via reducer)
checkSOS :: (Poly -> Poly) -> Poly -> Bool
checkSOS reducer p = isWeightedSOS p || checkSumOfSquares reducer p || checkSOS_SDP (reducer p)

-- | Check for weighted sum of even powers (trivial SOS)
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
          if not (isSquareMono ltM) || not (isSquareRat ltC)
          then Nothing
          else
            let rootM = sqrtMono ltM
                rootC = sqrtRat ltC
                rootLT = polyFromMonomial rootM rootC
            in findRoot p rootLT

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
               let factor = 2 * rootLT_C
                   nextC = remLT_C / factor
                   nextM = monomialDiv remLT_M rootLT_M
               in case nextM of
                    Nothing -> Nothing
                    Just m ->
                      let nextTerm = polyFromMonomial m nextC
                          isSmaller = compareMonomials Lex m rootLT_M == LT
                      in if not isSmaller
                         then Nothing
                         else findRoot target (polyAdd currentRoot nextTerm)

-- ============================================================================
-- Greedy Sum of Squares Decomposition (Rational Cholesky)
-- ============================================================================

checkSumOfSquares :: (Poly -> Poly) -> Poly -> Bool
checkSumOfSquares reducer pRaw =
  let p = reducer pRaw -- Reduce initially too? Yes.
  in if p == polyZero then True
  else
      case getLeadingTerm p of
        Nothing -> True
        Just (ltM, ltC) ->
          -- 1. Check if leading term is valid pivot
          if ltC <= 0 || not (isSquareMono ltM)
          then False
          else
            let
               m = sqrtMono ltM      -- The 'base' monomial
               c = ltC               -- The coefficient
               
               -- 2. Find Cross Terms: Terms in P (excluding LT) divisible by m
               
               (divisible, _rest) = partitionPolyByDivisibility p m
               
               quotient = case polyDivMonomial divisible m of
                            Just q -> q
                            Nothing -> error "Unreachable: partition logic failed"
               
               ltPoly = polyFromMonomial m c
               xPoly = polySub quotient ltPoly
               
               -- 3. Construct the Square to subtract
               
               xHalf = polyScale xPoly (1/2)
               inner = polyAdd ltPoly xHalf
               
               subtraction = polyScale (polyMul inner inner) (1/c)
               
               -- 4. Update P and Reduce
               newP = reducer (polySub p subtraction)
               
            in checkSumOfSquares reducer newP

-- | Partition polynomial into (terms divisible by m, terms not divisible)
partitionPolyByDivisibility :: Poly -> Monomial -> (Poly, Poly)
partitionPolyByDivisibility (Poly mapping) m =
  let (yes, no) = M.partitionWithKey (\k _ -> isDivisible k m) mapping
  in (Poly yes, Poly no)

isDivisible :: Monomial -> Monomial -> Bool
isDivisible (Monomial a) (Monomial b) = M.isSubmapOfBy (<=) b a

polyDivMonomial :: Poly -> Monomial -> Maybe Poly
polyDivMonomial (Poly mapping) m =
  let divided = M.mapKeysMonotonic (\k -> case monomialDiv k m of Just r -> r; Nothing -> k) mapping 
      -- Note: mapKeysMonotonic assumes order preservation. Division preserves Lex order? 
      -- Div by const monomial yes.
      -- We must ensure all keys were divisible.
  in Just (Poly divided)

polyScale :: Poly -> Rational -> Poly
polyScale (Poly m) s = Poly (M.map (*s) m)

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
  in (toRational (round (sqrt (fromIntegral n :: Double)) :: Integer)) / (toRational (round (sqrt (fromIntegral d :: Double)) :: Integer))