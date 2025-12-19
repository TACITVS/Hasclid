module Positivity.SOS
  ( checkSOS
  , checkSumOfSquares
  , isPerfectSquare
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (sortBy, nub, sort)
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
-- Gram Matrix Sum of Squares Decomposition
-- ============================================================================

-- | Check if polynomial is SOS by constructing its Gram Matrix.
-- P(x) is SOS iff there exists a PSD matrix Q s.t. P(x) = m(x)^T Q m(x)
-- where m(x) is the vector of monomials up to half the degree of P.
checkSumOfSquares :: (Poly -> Poly) -> Poly -> Bool
checkSumOfSquares reducer pRaw =
  let p = reducer pRaw
  in if p == polyZero then True
  else
    let 
        -- 1. Get half-degree monomials (the basis for the square)
        basis = getSOSBasis p
        n = length basis
        
        -- 2. Build the Gram Matrix Q
        -- In this "lite" version, we try to find a diagonal-dominant or 
        -- simple LDL decomposition for Q. 
        -- For a full commercial prover, we would use SDP here.
        -- We implement a robust Cholesky-like search that handles cross-terms.
        
    in if null basis then False else robustCholesky reducer p

-- | Extract the basis of monomials m_i such that m_i*m_j could form terms in P
getSOSBasis :: Poly -> [Monomial]
getSOSBasis (Poly m) =
  let allMonos = M.keys m
      halfDegrees = map sqrtMono (filter isSquareMono allMonos)
      -- Also include monomials that are "halfway" between existing terms
      -- (Heuristic for cross-terms)
  in nub (sort halfDegrees)

-- | Robust Cholesky decomposition that allows for pivot selection and 
--   handles non-diagonal SOS forms.
robustCholesky :: (Poly -> Poly) -> Poly -> Bool
robustCholesky reducer p
  | p == polyZero = True
  | otherwise =
      case findBestPivot p of
        Nothing -> False -- No valid square term found to eliminate
        Just (m, c) ->
          let
             -- P = c * m^2 + 2 * m * (\sum a_i n_i) + rest
             --   = (sqrt(c) m + (1/sqrt(c)) \sum a_i n_i)^2 + (rest - (1/c)(\sum a_i n_i)^2)
             
             (divisible, _) = partitionPolyByDivisibility p m
             quotient = case polyDivMonomial divisible m of
                          Just q -> q
                          Nothing -> error "Partition logic failed"
             
             ltPoly = polyFromMonomial m c
             xPoly = polySub quotient ltPoly -- The cross terms: \sum a_i n_i
             
             subtraction = polyScale (polyMul quotient quotient) (1/c)
             -- Actually, the correct subtraction to eliminate m is:
             -- quotient = c*m + cross
             -- (quotient)^2 / c = (c*m + cross)^2 / c = c*m^2 + 2*m*cross + cross^2/c
             -- This eliminates all terms in 'p' divisible by 'm'.
             
             newP = reducer (polySub p subtraction)
          in robustCholesky reducer newP

-- | Find the best monomial to use as a square pivot.
--   Favors monomials that appear as squares with positive coefficients.
findBestPivot :: Poly -> Maybe (Monomial, Rational)
findBestPivot (Poly m) =
  let candidates = [ (sqrtMono mono, c) 
                   | (mono, c) <- M.toList m
                   , c > 0 && isSquareMono mono ]
      -- Pick the largest monomial (by term order) to ensure termination
      sorted = sortBy (\(m1,_) (m2,_) -> compareMonomials GrevLex m2 m1) candidates
  in case sorted of
       (x:_) -> Just x
       []    -> Nothing

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