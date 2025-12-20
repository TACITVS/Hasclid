module Positivity.SOS
  ( checkSOS
  , checkSOSWithLemmas
  , checkSumOfSquares
  , isPerfectSquare
  , SOSCertificate(..)
  , getSOSCertificate
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (sortBy, nub, sort, delete)
import Data.Ratio (numerator, denominator)
import Data.Maybe (isJust, mapMaybe)
import TermOrder (compareMonomials, TermOrder(..))
import Positivity.SDP (checkSOS_SDP)

-- | Certificate showing that a polynomial is a sum of squares.
--   P = sum (coefficients_i * squares_i^2) + sum (lemmata_j) + remainder
data SOSCertificate = SOSCertificate
  { sosTerms :: [(Rational, Poly)] -- (coefficient, base_polynomial) such that coeff * base^2
  , sosLemmas :: [Poly]            -- Known non-negative lemmata used
  , sosRemainder :: Poly           -- Remaining part (should be 0 modulo ideal)
  } deriving (Show, Eq)

emptyCert :: SOSCertificate
emptyCert = SOSCertificate [] [] polyZero

-- | Check if polynomial is a Sum of Squares, potentially modulo an ideal (via reducer)
-- and potentially using a list of known non-negative lemmata.
checkSOS :: (Poly -> Poly) -> Poly -> Bool
checkSOS reducer p = isJust (getSOSCertificate [] reducer p)

-- | Get SOS certificate if it exists
getSOSCertificate :: [Poly] -> (Poly -> Poly) -> Poly -> Maybe SOSCertificate
getSOSCertificate lemmata reducer pRaw = 
  let p = reducer pRaw
  in case getWeightedSOS p of
       Just cert -> Just cert
       Nothing -> 
         case robustCholesky reducer p emptyCert of
           Just cert -> Just cert
           Nothing -> tryLemmaReduction lemmata reducer p

-- | Advanced SOS check that leverages previously proven lemmata.
checkSOSWithLemmas :: [Poly] -> (Poly -> Poly) -> Poly -> Bool
checkSOSWithLemmas lemmata reducer p = isJust (getSOSCertificate lemmata reducer p)

-- | Check for weighted sum of even powers (trivial SOS)
getWeightedSOS :: Poly -> Maybe SOSCertificate
getWeightedSOS (Poly m) =
  if not (M.null m) && all (\(Monomial vars, c) -> c > 0 && all even (M.elems vars)) (M.toList m)
  then let terms = [ (c, Poly (M.singleton (sqrtMono mono) 1)) | (mono, c) <- M.toList m ]
       in Just (emptyCert { sosTerms = terms })
  else Nothing

-- | Check if polynomial is a perfect square (P = Q^2)
isPerfectSquare :: Poly -> Bool
isPerfectSquare p = isJust (polynomialSqrt p)

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
checkSumOfSquares :: (Poly -> Poly) -> Poly -> Bool
checkSumOfSquares reducer pRaw = isJust (robustCholesky reducer pRaw emptyCert)

-- | Extract the basis of monomials m_i such that m_i*m_j could form terms in P
getSOSBasis :: Poly -> [Monomial]
getSOSBasis (Poly m) =
  let allMonos = M.keys m
      halfDegrees = map sqrtMono (filter isSquareMono allMonos)
  in nub (sort halfDegrees)

-- | Robust Cholesky decomposition that allows for pivot selection and 
--   returns a certificate of the squares found.
robustCholesky :: (Poly -> Poly) -> Poly -> SOSCertificate -> Maybe SOSCertificate
robustCholesky reducer p cert
  | p == polyZero = Just cert
  | otherwise =
      case findBestPivot p of
        Nothing -> Nothing -- No valid square term found to eliminate
        Just (m, c) ->
          let
             -- We want to eliminate all terms in p divisible by m.
             -- Let p = c * m^2 + m * Q + R  where R contains no terms divisible by m.
             -- We subtract (1/c) * (c*m + Q/2)^2
             -- = (1/c) * (c^2 m^2 + c*m*Q + Q^2/4)
             -- = c * m^2 + m * Q + Q^2 / (4c)
             
             (divisible, _) = partitionPolyByDivisibility p m
             
             -- rest = m * Q = divisible - c * m^2
             rest = polySub divisible (polyScale (polyFromMonomial (monomialMul m m) 1) c)
             
             -- Q = rest / m
             qPoly = case polyDivMonomial rest m of
                       Just q -> q
                       Nothing -> error "Partition logic failed"
             
             -- base = c*m + Q/2
             base = polyAdd (polyScale (polyFromMonomial m 1) c) (polyScale qPoly (1/2))
             
             -- subtraction = (1/c) * base^2
             subtraction = polyScale (polyMul base base) (1/c)
             
             newP = reducer (polySub p subtraction)
             newCert = cert { sosTerms = (1/c, base) : sosTerms cert }
          in robustCholesky reducer newP newCert

-- | Find the best monomial to use as a square pivot.
findBestPivot :: Poly -> Maybe (Monomial, Rational)
findBestPivot (Poly m) =
  let candidates = [ (sqrtMono mono, c) 
                   | (mono, c) <- M.toList m
                   , c > 0 && isSquareMono mono ]
      sorted = sortBy (\(m1,_) (m2,_) -> compareMonomials GrevLex m2 m1) candidates
  in case sorted of
       (x:_) -> Just x
       []    -> Nothing

-- | Attempt to subtract known non-negative lemmata from the target to make it SOS.
tryLemmaReduction :: [Poly] -> (Poly -> Poly) -> Poly -> Maybe SOSCertificate
tryLemmaReduction [] _ _ = Nothing
tryLemmaReduction (l:ls) reducer p =
  let remainder = reducer (polySub p l)
  in case getSOSCertificate ls reducer remainder of
       Just cert -> Just (cert { sosLemmas = l : sosLemmas cert })
       Nothing -> tryLemmaReduction ls reducer p

-- | Partition polynomial into (terms divisible by m, terms not divisible)
partitionPolyByDivisibility :: Poly -> Monomial -> (Poly, Poly)
partitionPolyByDivisibility (Poly mapping) m =
  let (yes, no) = M.partitionWithKey (\k _ -> isDivisible k m) mapping
  in (Poly yes, Poly no)

isDivisible :: Monomial -> Monomial -> Bool
isDivisible (Monomial a) (Monomial b) = M.isSubmapOfBy (<=) b a

polyDivMonomial :: Poly -> Monomial -> Maybe Poly
polyDivMonomial (Poly mapping) m =
  let dividedList = [ (res, c) | (k, c) <- M.toList mapping, let res = monomialDiv k m ]
  in if any (\(r, _) -> r == Nothing) dividedList
     then Nothing
     else Just (Poly (M.fromList [ (r, c) | (Just r, c) <- dividedList ]))

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