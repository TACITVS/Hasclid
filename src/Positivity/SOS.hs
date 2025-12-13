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

-- | Check if polynomial is a Sum of Squares
checkSOS :: Poly -> Bool
checkSOS p = isWeightedSOS p || checkSumOfSquares p

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
-- Algorithm:
-- 1. Pick Leading Term LT(P) = c * m.
-- 2. If c <= 0 or m is not a square, return False.
-- 3. Let base = sqrt(m).
-- 4. Gather all terms in P divisible by base: these form the "linear" part L relative to base.
--    Actually, we want to complete the square for the variable block in `base`.
--    Ideally, P = c * (base + ...)^2 + Remainder.
--    Expansion: c * (base^2 + 2*base*Rest + Rest^2) = c*m + 2*c*base*Rest + ...
--    We need to match the "cross terms" involving `base`.
--    Cross terms are terms T in P such that T is divisible by `base`.
--    Let P_div_base = part of P divisible by `base`.
--    Actually, simply:
--    Candidate Square S = (c * base + 1/2 * (terms_divisible_by_base_excluding_LT) / base )^2
--    No, that's not quite right for multivariate.
--
-- Correct Multivariate Cholesky Step:
-- P = A(x) * x_main^2d + B(x) * x_main^d + C(x) ...
-- We eliminate variable by variable? No, monomial by monomial.
--
-- Simplified Greedy:
-- 1. LT(P) = c * m^2. (c > 0).
-- 2. Let root = m.
-- 3. Find terms in P of form k * m * m' where m > m'.
--    These are the potential cross terms 2 * a * b.
--    We construct a polynomial Q = c * m + sum(k_i * m_i).
--    Such that Q^2 / c matches the LT and cross terms.
--    Actually:
--    P = (c_0 m_0 + c_1 m_1 + ...)^2
--    We build the square term iteratively.
--    Term 1: T1 = LT(P). Must be c * m^2.
--    Root term: R1 = m.
--    Square coeff: K = c.
--    We want to subtract K * (m + X)^2.
--    K * (m^2 + 2mX + X^2) = K m^2 + 2K m X + K X^2.
--    We match 2 K m X with terms in P divisible by m.
--    Let `Cross = Terms in P divisible by m (excluding m^2)`.
--    Then 2 K m X = Cross  =>  X = Cross / (2 K m).
--    So subtraction term is: K * (m + Cross / (2Km))^2
--                          = (1/K) * (K m + Cross / (2m))^2
--                          = (1/c) * (c m + Cross / (2m))^2.
--
--    Note: Cross / (2m) must be a valid polynomial!
--    i.e. All terms in `Cross` must be divisible by `m` and yield monomials < m.
--    Wait, `Cross` are terms divisible by `m`. So `Cross/m` is polynomial.
--    We assume `m` is the "half" monomial (sqrt of LT).
--    Terms divisible by `m` must have degree >= deg(m).
--    We subtract, get Remainder.
--    Recurse on Remainder.
--
--    If at any point Remainder LT is not positive square, fail.
--    If Remainder == 0, Success.

checkSumOfSquares :: Poly -> Bool
checkSumOfSquares p
  | p == polyZero = True
  | otherwise =
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
               -- Actually, we take ALL terms divisible by m.
               -- But strictly, we only care about terms that COULD be 2*m*m_i.
               -- For greedy Cholesky, we take ALL terms divisible by m to clear the column.
               
               (divisible, _rest) = partitionPolyByDivisibility p m
               
               -- divisible contains c*m^2 + other_terms * m
               -- divisible / m = c*m + other_terms
               
               quotient = case polyDivMonomial divisible m of
                            Just q -> q
                            Nothing -> error "Unreachable: partition logic failed"
               
               -- quotient = c*m + X.
               -- We want X.
               -- X = quotient - c*m
               
               ltPoly = polyFromMonomial m c
               xPoly = polySub quotient ltPoly
               
               -- 3. Construct the Square to subtract
               -- S = (1/c) * (c*m + X/2)^2
               --   = (1/c) * (quotient - X/2)^2  <-- No.
               -- Let's stick to formula: K * (m + X/(2K))^2 = (1/K) * (K*m + X/2)^2
               -- K = c.
               -- Inner = c*m + X/2.
               
               xHalf = polyScale xPoly (1/2)
               inner = polyAdd ltPoly xHalf
               
               subtraction = polyScale (polyMul inner inner) (1/c)
               
               -- 4. Update P
               newP = polySub p subtraction
               
            in checkSumOfSquares newP

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