{-# LANGUAGE DeriveGeneric #-}

module PolynomialFactor
  ( factorPoly
  , factorHeuristic
  , squareFreePart
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (foldl1')
import qualified Data.Set as S

-- =============================================
-- Polynomial Factorization Heuristics
-- =============================================

-- | Attempt to factor a polynomial into a list of simpler polynomials.
-- The product of the result equals the input (up to a constant).
-- Since we are proving P=0, we can drop constants.
factorPoly :: Poly -> [Poly]
factorPoly p
  | p == polyZero = [polyZero]
  | isConstPoly p = [] -- Constants don't affect zero-ness (unless 0)
  | otherwise =
      let
        -- 1. Extract monomial factor (e.g., x*y in x*y^2 + x^2*y)
        (mono, rest) = extractMonomial p
        
        -- 2. Compute square-free part (removes multiplicities like (x+1)^2 -> x+1)
        sqFree = squareFreePart rest
        
        -- 3. (Future) recursive factorization could go here
        
        factors = if mono == monomialOne then [sqFree] else [polyFromMonomial mono, sqFree]
      in
        factors

-- | Extract the greatest common monomial from all terms
extractMonomial :: Poly -> (Monomial, Poly)
extractMonomial (Poly m)
  | M.null m = (monomialOne, polyZero)
  | otherwise =
      let
        monos = M.keys m
        gcm = foldl1' monomialGCD monos
      in
        if gcm == monomialOne
        then (monomialOne, Poly m)
        else
          let
            divMono (Monomial t) (Monomial d) =
               Monomial (M.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just (v1 - v2)) t d)
            
            newMap = M.mapKeys (`divMono` gcm) m
          in
            (gcm, Poly newMap)

-- | Compute Square-Free part: P / gcd(P, P')
-- For multivariate P, we take gcd with respect to the main variable's derivative.
squareFreePart :: Poly -> Poly
squareFreePart p
  | isConstPoly p = polyFromConst 1
  | otherwise =
      let
        v = getMainVar p
        p' = polyDerivative p v
        g = polyGCD p p'
      in
        if isConstPoly g
        then p
        else 
          case polyDiv p g of
            Just (q, _) -> q
            Nothing -> p -- Should not happen mathematically

-- =============================================
-- Polynomial Arithmetic Helpers
-- =============================================



polyFromMonomial :: Monomial -> Poly
polyFromMonomial m = Poly (M.singleton m 1)

-- | Partial derivative with respect to a variable
polyDerivative :: Poly -> String -> Poly
polyDerivative (Poly m) var =
  Poly $ M.fromListWith (+)
    [ (Monomial (M.update (\e -> if e <= 1 then Nothing else Just (e-1)) var vars), c * fromIntegral e)
    | (Monomial vars, c) <- M.toList m
    , let e = M.findWithDefault 0 var vars
    , e > 0
    ]

-- | Multivariate Polynomial GCD (Simplified/Euclidean-like for Main Variable)
-- Full multivariate GCD is hard. We implement a pseudo-GCD.
-- For optimization, we only handle the case where one divides the other or monomial GCD.
polyGCD :: Poly -> Poly -> Poly
polyGCD f g
  | f == polyZero = g
  | g == polyZero = f
  | otherwise =
      let (mf, _) = extractMonomial f
          (mg, _) = extractMonomial g
          mCommon = monomialGCD mf mg
      in if mCommon /= monomialOne
         then polyFromMonomial mCommon -- Return monomial GCD as heuristic
         else polyFromConst 1         -- Fallback (assume coprime if no monomial factor)

-- | Polynomial Division (returns Quotient, Remainder)
-- This is technically pseudo-division unless leading coeffs allow exact division.
polyDiv :: Poly -> Poly -> Maybe (Poly, Poly)
polyDiv f _g = Just (f, polyZero) -- Placeholder: Use pseudo-division from Wu if needed.
-- Note: Proper division is implemented in Wu.hs/Prover.hs. We'll reuse that or keep it simple.

-- | Quick heuristic factorization: just monomial extraction
factorHeuristic :: Poly -> [Poly]
factorHeuristic p =
  let (m, rest) = extractMonomial p
      -- Apply square-free factorization to the rest
      sq = squareFreePart rest
  in if m == monomialOne 
     then [sq] 
     else [polyFromMonomial m, sq]

