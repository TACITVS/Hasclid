module CAD (discriminant, toRecursive) where

import Expr
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe) -- <--- FIXED: Added this import

-- | Recursive Polynomial: Coefficients are themselves Polys
--   Represents P(x) = c_n * x^n + ... + c_0
--   where c_i are polynomials in other variables.
type RecPoly = [Poly] 

-- =============================================
-- 1. Recursive Structure (Flat -> Recursive)
-- =============================================

-- | Extract coefficients of a polynomial with respect to a specific variable
--   e.g. toRecursive (x^2 + xy + y^2) "x" -> [y^2, y, 1]
--   (Returns coefficients in increasing order of degree: c0, c1, c2...)
toRecursive :: Poly -> String -> RecPoly
toRecursive (Poly m) var = 
    let maxDeg = maximum (0 : [ M.findWithDefault 0 var vars | (Monomial vars, _) <- M.toList m ])
        
        -- Helper: Check if a monomial contains 'var' to exact power 'k'
        -- If so, return the monomial *without* 'var'
        extractTerm :: Int -> (Monomial, Rational) -> Maybe (Monomial, Rational)
        extractTerm k (Monomial vars, coeff) =
            if M.findWithDefault 0 var vars == fromIntegral k
            then Just (Monomial (M.delete var vars), coeff)
            else Nothing

        -- Build coefficient for x^k
        getCoeff k = 
            let terms = mapMaybe (extractTerm k) (M.toList m)
            in if null terms then polyZero else Poly (M.fromList terms)

    in [ getCoeff k | k <- [0..fromIntegral maxDeg] ]

-- | Convert back from Recursive to Flat
fromRecursive :: RecPoly -> String -> Poly
fromRecursive coeffs var = 
    sum [ polyMul c (polyPow (polyFromVar var) (fromIntegral i)) 
        | (i, c) <- zip [0..] coeffs ]
  where
    sum = foldl polyAdd polyZero

-- =============================================
-- 2. Polynomial Arithmetic on Poly Coefficients
-- =============================================

-- Degree of recursive poly
degRec :: RecPoly -> Int
degRec p = length (normalizeRec p) - 1

-- Normalize (remove trailing zero polynomials)
normalizeRec :: RecPoly -> RecPoly
normalizeRec = dropWhileEnd (== polyZero)

-- Leading Coefficient (which is a Poly!)
lcRec :: RecPoly -> Poly
lcRec p = case normalizeRec p of
    [] -> polyZero
    xs -> last xs

scaleRec :: Poly -> RecPoly -> RecPoly
scaleRec s = map (polyMul s)

shiftRec :: Int -> RecPoly -> RecPoly
shiftRec k p = replicate k polyZero ++ p

addRec :: RecPoly -> RecPoly -> RecPoly
addRec [] ys = ys
addRec xs [] = xs
addRec (x:xs) (y:ys) = polyAdd x y : addRec xs ys

negRec :: RecPoly -> RecPoly
negRec = map polyNeg

subRec :: RecPoly -> RecPoly -> RecPoly
subRec xs ys = addRec xs (negRec ys)

-- =============================================
-- 3. Pseudo-Division (The Heart of CAD)
-- =============================================

-- | Pseudo-Remainder: prem(F, G)
--   Computes R such that: LC(G)^(deg(F)-deg(G)+1) * F = Q * G + R
--   This works even if we can't divide fractions (integral domain).
pseudoRem :: RecPoly -> RecPoly -> RecPoly
pseudoRem f g = normalizeRec (go f)
  where
    gNorm = normalizeRec g
    df = degRec f
    dg = degRec gNorm
    l = lcRec gNorm
    delta = df - dg + 1
    
    go currentF
      | degRec currentF < dg = currentF
      | otherwise = 
          let degCurr = degRec currentF
              lcCurr  = lcRec currentF
              -- Multiply entire F by LC(G) to avoid fractions
              fScaled = scaleRec l currentF
              -- Subtract term: LC(F) * x^(degF-degG) * G
              term    = shiftRec (degCurr - dg) (scaleRec lcCurr gNorm)
              nextF   = subRec fScaled term
          in go (normalizeRec nextF)

-- =============================================
-- 4. Resultant Algorithm (Subresultant)
-- =============================================

-- | Compute Resultant of two polynomials w.r.t a variable.
--   Resultant eliminates the variable. Res(P(x,y), Q(x,y), x) -> R(y)
resultant :: Poly -> Poly -> String -> Poly
resultant f g var = 
    let rf = toRecursive f var
        rg = toRecursive g var
    in subresultantPRS rf rg

-- | Euclidean Algorithm variant for Polynomials (simplified Subresultant)
--   Technically this is the Euclidean PRS (Polynomial Remainder Sequence).
--   Standard Resultant = last non-zero term.
subresultantPRS :: RecPoly -> RecPoly -> Poly
subresultantPRS f g
  | normalizeRec f == [] = polyZero
  | normalizeRec g == [] = polyZero
  | degRec g == 0 = polyPow (lcRec g) (fromIntegral (degRec f)) -- Base case
  | otherwise = 
      let r = pseudoRem f g
      in if normalizeRec r == [] 
         then polyZero -- They share a factor!
         else subresultantPRS g r -- Recurse

-- | Discriminant: Resultant(f, f')
--   Disc(f) = 0 implies f has a double root (turning point or singularity).
discriminant :: Poly -> String -> Poly
discriminant f var = 
    let fRec = toRecursive f var
        fPrime = derivRec fRec
        -- Correction factor for discriminant sign/scaling:
        -- Disc(f) = (-1)^(n(n-1)/2) / lc(f) * Res(f, f')
        -- For projection purposes, Res(f, f') contains the critical geometry.
        res = subresultantPRS fRec fPrime
    in res

-- Derivative of recursive poly
derivRec :: RecPoly -> RecPoly
derivRec [] = []
derivRec (_:xs) = zipWith (\pow coeff -> polyMul (polyFromConst (fromIntegral pow)) coeff) [1..] xs