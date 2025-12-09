{-# LANGUAGE DeriveGeneric #-}

module Linearizer (linearizeSystem) where

import Expr
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub, elemIndex, span)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Numeric.Natural (Natural)

-- =============================================
-- Linearization Engine
-- =============================================

-- | Attempts to lower the degree of a polynomial system by finding linear combinations
-- that cancel out the highest-degree terms.
-- Returns the original system augmented with any new lower-degree polynomials found.
linearizeSystem :: [Poly] -> [Poly]
linearizeSystem polys =
  let
    -- 1. Identify the highest degree in the system
    degrees = map polyTotalDegree polys
    maxDeg = if null degrees then 0 else maximum degrees
  in
    if maxDeg < 2
    then polys -- Already linear or constant, nothing to do
    else
      let
        -- 2. Group polynomials that have this max degree
        (candidates, others) = splitByDegree maxDeg polys
      in
        if length candidates < 2
        then polys -- Need at least 2 polynomials to cancel terms against each other
        else
          let
            -- 3. Perform reduction on the top-degree forms
            newPolys = reduceHighestFormsRefined candidates maxDeg
            
            -- 4. Filter out trivial results (0 or existing polys)
            usefulNew = filter (\p -> p /= polyZero && not (p `elem` polys)) newPolys
          in
            -- Repeat until fixed point? For now, just one pass.
            polys ++ usefulNew

-- | Get total degree of a polynomial
polyTotalDegree :: Poly -> Int
polyTotalDegree (Poly m)
  | M.null m = 0
  | otherwise = maximum (map (fromIntegral . monoTotalDegree) (M.keys m))

monoTotalDegree :: Monomial -> Natural
monoTotalDegree (Monomial m) = sum (M.elems m)

splitByDegree :: Int -> [Poly] -> ([Poly], [Poly])
splitByDegree deg polys =
  let check p = polyTotalDegree p == deg
  in (filter check polys, filter (not . check) polys)

-- | Extract monomials of a specific degree from a polynomial
getMonomialsOfDegree :: Int -> Poly -> [Monomial]
getMonomialsOfDegree deg (Poly m) =
  [ mono | (mono, _) <- M.toList m, fromIntegral (monoTotalDegree mono) == deg ]

-- | Build a coefficient vector for the target monomials
rowVector :: Poly -> [Monomial] -> [Rational]
rowVector (Poly m) targets =
  map (\target -> M.findWithDefault 0 target m) targets

-- Refined Reduce Logic (Gaussian Elimination)
reduceHighestFormsRefined :: [Poly] -> Int -> [Poly]
reduceHighestFormsRefined polys deg =
  let
    allMons = S.toList $ S.fromList $ concatMap (getMonomialsOfDegree deg) polys
    -- Matrix: List of Rows. Each Row is (Vector, SourcePoly)
    initialMatrix = [ (rowVector p allMons, p) | p <- polys ]
    
    -- Eliminate column by column
    finalMatrix = gaussianEliminate initialMatrix (length allMons)
  in
    map snd finalMatrix

gaussianEliminate :: [([Rational], Poly)] -> Int -> [([Rational], Poly)]
gaussianEliminate matrix numCols = foldl reduceCol matrix [0 .. numCols - 1]

reduceCol :: [([Rational], Poly)] -> Int -> [([Rational], Poly)]
reduceCol rows colIdx =
  let
    entry row = (fst row) !! colIdx
    
    (pivots, others) = span (\r -> entry r /= 0) rows
    (nonZeros, zeros) = pPartition (\r -> entry r /= 0) rows
  in
    case nonZeros of
      [] -> rows -- No pivot for this column, skip
      (pivot:rest) ->
        let
          pivotVal = entry pivot
          -- Reduce the rest using the pivot
          reducedRest = map (eliminateRow pivot pivotVal colIdx) rest
        in
          -- Keep the pivot, keep the zeros, keep the reduced ones
          pivot : (zeros ++ reducedRest)

pPartition :: (a -> Bool) -> [a] -> ([a], [a])
pPartition p xs = foldr (\x (ys, ns) -> if p x then (x:ys, ns) else (ys, x:ns)) ([], []) xs

eliminateRow :: ([Rational], Poly) -> Rational -> Int -> ([Rational], Poly) -> ([Rational], Poly)
eliminateRow (pivVec, pivPoly) pivVal col (targetVec, targetPoly) =
  let
    targetVal = targetVec !! col
    factor = targetVal / pivVal
    
    -- Update Vector: v' = v - factor * pivot_v
    newVec = zipWith (\t p -> t - factor * p) targetVec pivVec
    
    -- Update Polynomial: P' = P - factor * PivotP
    scaledPivot = scalePoly factor pivPoly
    newPoly = polySub targetPoly scaledPivot
  in
    (newVec, newPoly)
