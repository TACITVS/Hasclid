module Positivity.SOS
  ( checkSOS
  , checkSOSWithLemmas
  , checkSumOfSquares
  , isPerfectSquare
  , getSOSCertificate
  , SOSCertificate(..)
  ) where

import Expr
import Polynomial (fromMonomial, scale)
import qualified Data.Map.Strict as M
import Data.List (sortBy, nub, sort, delete)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (isJust, mapMaybe)
import TermOrder (compareMonomials, TermOrder(..))
import Positivity.SOSTypes (trySOSHeuristic, SOSPattern(..), SOSCertificate(..), sqrtRational)

emptyCert :: SOSCertificate
emptyCert = SOSCertificate [] [] polyZero Nothing

-- | Check if polynomial is a Sum of Squares, potentially modulo an ideal (via reducer)
-- and potentially using a list of known non-negative lemmata.
checkSOS :: (Poly -> Poly) -> Poly -> Bool
checkSOS reducer p = isJust (getSOSCertificate [] reducer p)

-- | Get SOS certificate if it exists, using known non-negative variables and lemmata.
getSOSCertificate :: [Poly] -> (Poly -> Poly) -> Poly -> Maybe SOSCertificate
getSOSCertificate lemmata reducer pRaw = 
  let p = reducer pRaw
      -- Extract variables known to be non-negative from lemmata (e.g., v >= 0)
      posVars = mapMaybe posVarName [m | Poly m <- lemmata]
  in case trySOSHeuristic p (map (\l -> Ge (polyToExpr l) (Const 0)) lemmata) of -- Heuristic first
       Just cert -> Just $ cert { sosLemmas = lemmata }
       Nothing -> 
         case getPositionalSOS posVars p of
           Just cert -> Just cert
           Nothing -> 
             case robustCholesky reducer p emptyCert of
               Just cert -> Just cert
               Nothing -> tryLemmaReduction lemmata reducer p

-- | Advanced SOS check that leverages previously proven lemmata.
checkSOSWithLemmas :: [Poly] -> (Poly -> Poly) -> Poly -> Bool
checkSOSWithLemmas lemmata reducer p = isJust (getSOSCertificate lemmata reducer p)

-- | Check if a polynomial is a sum of monomials that are products of known non-negative variables.
getPositionalSOS :: [String] -> Poly -> Maybe SOSCertificate
getPositionalSOS posVars (Poly m) =
  if not (M.null m) && all isNonNegativeTerm (M.toList m)
  then Just (emptyCert { sosLemmas = [Poly m] })
  else Nothing
  where
    isNonNegativeTerm (Monomial vars, c) = 
      c > 0 && all (\(v, e) -> even e || v `elem` posVars) (M.toList vars)

posVarName :: M.Map Monomial Rational -> Maybe String
posVarName m =
  case M.toList m of
    [(Monomial v, coeff)] | coeff == 1 ->
      case M.toList v of
        [(name, 1)] -> Just name
        _ -> Nothing
    _ -> Nothing

-- | Check if polynomial is a perfect square (P = Q^2)
isPerfectSquare :: Poly -> Bool
isPerfectSquare p = isJust (polynomialSqrt p)

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
                rootC = sqrtRational ltC
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

-- | Robust Cholesky decomposition
robustCholesky :: (Poly -> Poly) -> Poly -> SOSCertificate -> Maybe SOSCertificate
robustCholesky reducer p cert
  | p == polyZero = Just cert
  | otherwise =
      let posVars = mapMaybe posVarName [m | Poly m <- sosLemmas cert]
      in case findBestPivotEnhanced posVars p of
        Nothing -> Nothing 
        Just (m, c, maybeV) ->
          let
             pivotPoly = case maybeV of
                           Just v -> polyMul (polyFromVar v) (polyFromMonomial (monomialMul m m) c)
                           Nothing -> polyFromMonomial (monomialMul m m) c
             (divisible, _) = partitionPolyByDivisibility p m
             rest = polySub divisible pivotPoly
             qPoly = case polyDivMonomial rest m of
                       Just q -> q
                       Nothing -> error "Partition logic failed"
          in case maybeV of
               Nothing -> 
                 let base = polyAdd (_polyScale (polyFromMonomial m 1) c) (_polyScale qPoly (1/2))
                     subtraction = _polyScale (polyMul base base) (1/c)
                     newP = reducer (polySub p subtraction)
                     newCert = cert { sosTerms = (1/c, base) : sosTerms cert }
                 in robustCholesky reducer newP newCert
               Just _v -> 
                 let newP = reducer (polySub p pivotPoly)
                     newCert = cert { sosLemmas = pivotPoly : sosLemmas cert }
                 in robustCholesky reducer newP newCert

findBestPivotEnhanced :: [String] -> Poly -> Maybe (Monomial, Rational, Maybe String)
findBestPivotEnhanced posVars (Poly m) =
  let squares = [ (sqrtMono mono, c, Nothing) | (mono, c) <- M.toList m, c > 0, isSquareMono mono ]
      posTerms = [ (sqrtMono (Monomial (M.delete v vm)), c, Just v)
                 | (Monomial vm, c) <- M.toList m, c > 0, v <- posVars
                 , M.findWithDefault 0 v vm == 1, isSquareMono (Monomial (M.delete v vm)) ]
      candidates = squares ++ posTerms
      sorted = sortBy (\(m1,_,_) (m2,_,_) -> compareMonomials GrevLex m2 m1) candidates
  in case sorted of (x:_) -> Just x; [] -> Nothing

tryLemmaReduction :: [Poly] -> (Poly -> Poly) -> Poly -> Maybe SOSCertificate
tryLemmaReduction [] _ _ = Nothing
tryLemmaReduction (l:ls) reducer p =
  let remainder = reducer (polySub p l)
  in case getSOSCertificate ls reducer remainder of
       Just cert -> Just (cert { sosLemmas = l : sosLemmas cert })
       Nothing -> tryLemmaReduction ls reducer p

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

-- Use scale from Polynomial module (local alias for backwards compatibility)
_polyScale :: Poly -> Rational -> Poly
_polyScale p s = scale s p

-- Use fromMonomial from Polynomial module (local alias for backwards compatibility)
polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial = fromMonomial

isSquareMono :: Monomial -> Bool
isSquareMono (Monomial m) = all even (M.elems m)

sqrtMono :: Monomial -> Monomial
sqrtMono (Monomial m) = Monomial (M.map (`div` 2) m)

isSquareRat :: Rational -> Bool
isSquareRat r = r >= 0 && isSquare (numerator r) && isSquare (denominator r)

isSquare :: Integer -> Bool
isSquare x | x < 0 = False | otherwise = let s = integerSqrt x in s * s == x
