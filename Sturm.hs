module Sturm where

import Data.List (dropWhileEnd)
import Data.Ratio

-- | Univariate Polynomials are just [Rational] (c0 + c1*x + c2*x^2 ...)
type UPoly = [Rational]

-- | Normalize: Remove trailing zeros
normalize :: UPoly -> UPoly
normalize = dropWhileEnd (== 0)

-- | Degree of polynomial
degree :: UPoly -> Int
degree p = max 0 (length (normalize p) - 1)

-- | Leading Coefficient
lc :: UPoly -> Rational
lc p = case normalize p of
         [] -> 0
         xs -> last xs

-- | Polynomial Addition
addPoly :: UPoly -> UPoly -> UPoly
addPoly [] ys = ys
addPoly xs [] = xs
addPoly (x:xs) (y:ys) = (x+y) : addPoly xs ys

-- | Polynomial Negation
negPoly :: UPoly -> UPoly
negPoly = map negate

-- | Polynomial Multiplication by Scalar
scalePoly :: Rational -> UPoly -> UPoly
scalePoly s = map (*s)

-- | Polynomial Shift (multiply by x^k)
shiftPoly :: Int -> UPoly -> UPoly
shiftPoly k p = replicate k 0 ++ p

-- | Polynomial Remainder (Euclidean Division)
--   Returns (Remainder)
polyRem :: UPoly -> UPoly -> UPoly
polyRem f g
  | gNorm == [] = error "Division by zero polynomial"
  | fNorm == [] = []   -- <--- BUG FIX: Explicitly handle Zero Remainder
  | degree f < degree g = fNorm
  | otherwise = 
      let df = degree f
          dg = degree g
          lf = lc f
          lg = lc g
          -- Term to subtract: (lf/lg) * x^(df-dg)
          factor = lf / lg
          subTerm = scalePoly factor (shiftPoly (df - dg) g)
      in polyRem (normalize (addPoly f (negPoly subTerm))) g
  where fNorm = normalize f
        gNorm = normalize g

-- | Derivative of a polynomial
derivative :: UPoly -> UPoly
derivative [] = []
derivative (_:xs) = zipWith (*) (map fromIntegral [1..]) xs

-- | Evaluate polynomial at value x using Horner's Method
evalPoly :: UPoly -> Rational -> Rational
evalPoly p x = foldr (\c acc -> c + x * acc) 0 p

-- =========================================================================
-- STURM'S ALGORITHM
-- =========================================================================

-- | Generate Sturm Sequence: P0, P1, P2... where Pi = -rem(Pi-2, Pi-1)
sturmSequence :: UPoly -> [UPoly]
sturmSequence p = 
    let p0 = normalize p
        p1 = normalize (derivative p0)
    in go p0 p1
  where
    go p0 p1
      | normalize p1 == [] = [p0]
      | otherwise = p0 : go p1 (normalize (negPoly (polyRem p0 p1)))

-- | Count sign changes in a list of numbers (ignoring zeros)
signChanges :: [Rational] -> Int
signChanges [] = 0
signChanges (x:xs) = go x xs
  where
    go _ [] = 0
    go prev (curr:rest)
      | curr == 0 = go prev rest -- Ignore zeros
      | signum prev /= signum curr = 1 + go curr rest
      | otherwise = go curr rest

-- | Evaluate sign variations at a point x
signVariationsAt :: [UPoly] -> Rational -> Int
signVariationsAt seq x = signChanges (map (`evalPoly` x) seq)

-- | Count roots in interval (a, b]
rootsInInterval :: UPoly -> Rational -> Rational -> Int
rootsInInterval p a b = 
    let seq = sturmSequence p
    in signVariationsAt seq a - signVariationsAt seq b

-- | Count TOTAL real roots (-inf, +inf)
--   We approximate +/- infinity by checking signs of leading coefficients
countRealRoots :: UPoly -> Int
countRealRoots p =
    let seq = sturmSequence p
        
        -- Get signs at +infinity (just leading coeffs)
        signsPosInf = map lc seq
        
        -- Get signs at -infinity (LC * (-1)^deg)
        signsNegInf = map (\poly -> lc poly * (if even (degree poly) then 1 else -1)) seq
        
    in signChanges signsNegInf - signChanges signsPosInf

-- | Check if a polynomial is strictly positive everywhere
isAlwaysPositive :: UPoly -> Bool
isAlwaysPositive p = 
    (lc p > 0) && (countRealRoots p == 0)