module Sturm where

import Data.List (dropWhileEnd)

-- | Univariate Polynomials are just [Rational] (c0 + c1*x + c2*x^2 ...)
type UPoly = [Rational]

-- =========================================================================
-- BASIC POLYNOMIAL OPS
-- =========================================================================

normalize :: UPoly -> UPoly
normalize = dropWhileEnd (== 0)

degree :: UPoly -> Int
degree p = max 0 (length (normalize p) - 1)

lc :: UPoly -> Rational
lc p = case normalize p of [] -> 0; xs -> last xs

addPoly :: UPoly -> UPoly -> UPoly
addPoly [] ys = ys
addPoly xs [] = xs
addPoly (x:xs) (y:ys) = (x+y) : addPoly xs ys

negPoly :: UPoly -> UPoly
negPoly = map negate

scalePoly :: Rational -> UPoly -> UPoly
scalePoly s = map (*s)

shiftPoly :: Int -> UPoly -> UPoly
shiftPoly k p = replicate k 0 ++ p

polyRem :: UPoly -> UPoly -> UPoly
polyRem f g
  | gNorm == [] = error "Division by zero polynomial"
  | fNorm == [] = []
  | degree f < degree g = fNorm
  | otherwise = 
      let df = degree f
          dg = degree g
          lf = lc f
          lg = lc g
          factor = lf / lg
          subTerm = scalePoly factor (shiftPoly (df - dg) g)
      in polyRem (normalize (addPoly f (negPoly subTerm))) g
  where fNorm = normalize f
        gNorm = normalize g

derivative :: UPoly -> UPoly
derivative [] = []
derivative (_:xs) = zipWith (*) (map fromIntegral [1 :: Integer ..]) xs

evalPoly :: UPoly -> Rational -> Rational
evalPoly p x = foldr (\c acc -> c + x * acc) 0 p

-- =========================================================================
-- STURM'S ALGORITHM (Root Counting)
-- =========================================================================

sturmSequence :: UPoly -> [UPoly]
sturmSequence p = 
    let p0 = normalize p
        p1 = normalize (derivative p0)
    in go p0 p1
  where
    go p0 p1
      | normalize p1 == [] = [p0]
      | otherwise = p0 : go p1 (normalize (negPoly (polyRem p0 p1)))

signChanges :: [Rational] -> Int
signChanges [] = 0
signChanges (x:xs) = go x xs
  where
    go _ [] = 0
    go prev (curr:rest)
      | curr == 0 = go prev rest
      | signum prev /= signum curr = 1 + go curr rest
      | otherwise = go curr rest

signVariationsAt :: [UPoly] -> Rational -> Int
signVariationsAt seq x = signChanges (map (`evalPoly` x) seq)

-- | Count roots in (a, b]
rootsInInterval :: [UPoly] -> Rational -> Rational -> Int
rootsInInterval seq a b = signVariationsAt seq a - signVariationsAt seq b

-- | Counts TOTAL real roots (-inf, +inf)
countRealRoots :: UPoly -> Int
countRealRoots p =
    let seq = sturmSequence p
        signsPosInf = map lc seq
        signsNegInf = map (\poly -> lc poly * (if even (degree poly) then 1 else -1)) seq
    in signChanges signsNegInf - signChanges signsPosInf

-- | Cauchy Bound: All real roots are within [-M, M]
cauchyBound :: UPoly -> Rational
cauchyBound p = 
    let norm = normalize p
        an = abs (last norm)
        coeffs = init norm
    in if null coeffs then 1 else 1 + maximum (0 : map (\c -> abs c / an) coeffs)

-- =========================================================================
-- ROOT ISOLATION (The "Base Phase" of CAD)
-- =========================================================================

type Interval = (Rational, Rational)

-- | Find disjoint intervals isolating all real roots
isolateRoots :: UPoly -> [Interval]
isolateRoots p = 
    let seq = sturmSequence p
        bound = cauchyBound p
        totalRoots = rootsInInterval seq (-bound) bound
    in if totalRoots == 0 then []
       else bisectRoots seq (-bound) bound totalRoots

-- | Recursively bisect intervals until each contains exactly 1 root
bisectRoots :: [UPoly] -> Rational -> Rational -> Int -> [Interval]
bisectRoots seq a b count
  | count == 0 = []
  | count == 1 = [(a, b)]
  | otherwise = 
      let mid = (a + b) / 2
          leftCount = rootsInInterval seq a mid
          rightCount = count - leftCount 
      in bisectRoots seq a mid leftCount ++ bisectRoots seq mid b rightCount

-- | Pick Sample Points (Fixed for touching intervals)
samplePoints :: UPoly -> [Rational]
samplePoints p =
    let roots = isolateRoots p
        bound = cauchyBound p * 2
        
        lowers = (-bound) : map snd roots
        uppers = map fst roots ++ [bound]
        regions = zip lowers uppers
        
        -- FIX: Allow a == b (touching intervals). 
        -- If intervals touch at X, X is the sample point.
        points = [ if a == b then a else (a+b)/2 | (a,b) <- regions, a <= b ]
        
    in if null roots then [0] else points
       
-- | Check if polynomial is positive at all sample points
isAlwaysPositive :: UPoly -> Bool
isAlwaysPositive p = 
    let samples = samplePoints p
        values = map (evalPoly p) samples
    in all (> 0) values
