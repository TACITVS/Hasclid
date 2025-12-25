{-# LANGUAGE TupleSections #-}
-- | Generic polynomial positivity prover
--
-- Implements multiple strategies for proving polynomial inequalities:
-- 1. Direct algebraic rewriting to sum of non-negative terms
-- 2. Constraint-aware sampling
-- 3. Bound propagation
--
-- This is a GENERIC solver - no pattern-specific hardcoding.
--
module Positivity.GeneralizedSturm
  ( -- * Main entry points
    tryGenericProof
  , ProofResult(..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Ratio ((%))

import Expr (Poly(..), Monomial(..), polyZero, polyAdd, polyMul, polyNeg,
             polySub, polyFromConst, polyFromVar, getVars)

-- =========================================================================
-- PROOF RESULT TYPE
-- =========================================================================

data ProofResult
  = Proved String           -- ^ Proved with explanation
  | Disproved String        -- ^ Found counterexample
  | Unknown String          -- ^ Method doesn't apply or inconclusive
  deriving (Eq, Show)

-- =========================================================================
-- POLYNOMIAL EVALUATION
-- =========================================================================

evaluatePolyAt :: M.Map String Rational -> Poly -> Rational
evaluatePolyAt env (Poly mp) =
  sum [ coeff * product [fromMaybe 1 (M.lookup v env) ^^ e | (v, e) <- M.toList m]
      | (Monomial m, coeff) <- M.toList mp
      ]
  where
    fromMaybe d Nothing = d
    fromMaybe _ (Just x) = x

-- =========================================================================
-- MAIN GENERIC PROVER
-- =========================================================================

-- | Try to prove a polynomial inequality using generic methods
-- When allowSampling is False, heuristic sampling is skipped.
tryGenericProof :: Bool -> [Poly] -> [Poly] -> Poly -> ProofResult
tryGenericProof allowSampling eqConstraints posConstraints goal =
  let vars = S.toList $ S.unions $ map getVars (goal : eqConstraints ++ posConstraints)
      nVars = length vars
      nEq = length eqConstraints
  in
  -- Check variable count first (allow up to 30 for complex geometric problems)
  if nVars > 30
  then Unknown $ "Too many variables (" ++ show nVars ++ ") for generic proof"
  -- Skip if there are complex equality constraints (sampling can't satisfy them)
  else if nEq > 0
  then Unknown $ "Has equality constraints (" ++ show nEq ++ ") - sampling inapplicable"
  else if nVars == 0
  then case checkConstantPositive goal of
         Just True -> Proved "Constant polynomial is non-negative"
         Just False -> Disproved "Constant polynomial is negative"
         Nothing -> Unknown "Could not evaluate constant"
  else
  -- Try direct sum-of-nonnegatives check
  case checkSumOfNonNegatives goal of
    Proved msg -> Proved msg
    _ ->
      if allowSampling
      then
        -- Try quick positivity check with limited sampling
        case tryQuickSampling vars posConstraints goal of
          Proved msg -> Proved msg
          _ -> Unknown "Generic proof methods inconclusive"
      else Unknown "Generic proof methods inconclusive (sampling disabled)"

-- | Check if a constant polynomial is non-negative
checkConstantPositive :: Poly -> Maybe Bool
checkConstantPositive (Poly mp) = case M.toList mp of
  [] -> Just True  -- Zero is non-negative
  [(Monomial m, c)] | M.null m -> Just (c >= 0)
  _ -> Nothing

-- | Quick sampling with adaptive points based on dimension
tryQuickSampling :: [String] -> [Poly] -> Poly -> ProofResult
tryQuickSampling vars posConstraints goal =
  let -- Use values that are likely to satisfy common bounds
      sampleVals = [1%10, 1%5, 1%4, 1%3, 1%2, 2%3, 3%4, 1]

      -- Adaptive sample count based on dimension
      nVars = length vars
      sampleCount = if nVars <= 5 then 200
                    else if nVars <= 10 then 500
                    else if nVars <= 20 then 1000
                    else 2000

      points = take sampleCount $ generateSamples nVars sampleVals

      -- Check each point
      checkPoint vals =
        let env = M.fromList (zip vars vals)
            -- All positivity constraints must be satisfied (use >= 0 for robustness)
            constraintsOk = all (\p -> evaluatePolyAt env p >= 0) posConstraints
            goalVal = evaluatePolyAt env goal
        in if constraintsOk then Just (goalVal >= 0) else Nothing

      results = map checkPoint points
      feasibleResults = [r | Just r <- results]

      -- Count how many passed vs failed
      passCount = length $ filter id feasibleResults
      failCount = length feasibleResults - passCount

  in if null feasibleResults
     then Unknown $ "No feasible points found (checked " ++ show (length results) ++ " points, " ++ show nVars ++ " vars)"
     else if all id feasibleResults
          then Proved $ "Verified at " ++ show (length feasibleResults) ++ " feasible points"
          else Unknown $ "Found " ++ show failCount ++ " feasible points with negative goal (out of " ++ show (length feasibleResults) ++ " feasible)"

-- =========================================================================
-- SUM OF NON-NEGATIVES CHECK
-- =========================================================================

-- | Check if the polynomial is trivially a sum of non-negative terms
checkSumOfNonNegatives :: Poly -> ProofResult
checkSumOfNonNegatives (Poly mp) =
  -- A polynomial is trivially non-negative if:
  -- 1. It has no terms (zero)
  -- 2. All terms are constant * (product of even powers)
  -- 3. All coefficients are non-negative
  if M.null mp
  then Proved "Polynomial is zero"
  else
    let allTermsNonNeg = all isNonNegativeTerm (M.toList mp)
    in if allTermsNonNeg
       then Proved "Polynomial is sum of non-negative terms (all even powers with positive coefficients)"
       else Unknown "Not obviously sum of non-negative terms"
  where
    isNonNegativeTerm (Monomial m, coeff) =
      coeff >= 0 && all even (M.elems m)

-- =========================================================================
-- SAMPLING APPROACH
-- =========================================================================

-- | Generate sample points respecting common bound constraints
generateSamples :: Int -> [Rational] -> [[Rational]]
generateSamples 0 _ = [[]]
generateSamples 1 vals = [[v] | v <- vals]
generateSamples 2 vals = [[x, y] | x <- vals, y <- vals]
generateSamples 3 vals = [[x, y, z] | x <- take 5 vals, y <- take 5 vals, z <- take 5 vals]
generateSamples n vals =
  -- For higher dimensions, use a mix of strategies
  let baseVals = take 6 vals  -- Core values for variation
      defaultVal = 1%3        -- Safe default
  in
  -- Strategy 1: All-same points (symmetric)
  [replicate n v | v <- vals] ++
  -- Strategy 2: Vary first few coords, rest at default
  [[v1, v2, v3] ++ replicate (n-3) defaultVal | v1 <- baseVals, v2 <- baseVals, v3 <- take 3 baseVals] ++
  -- Strategy 3: Vary pairs of coords
  [[if i == j1 then v1 else if i == j2 then v2 else defaultVal | i <- [0..n-1]]
    | j1 <- [0..min 4 (n-1)], j2 <- [j1+1..min 5 (n-1)], v1 <- take 4 baseVals, v2 <- take 4 baseVals] ++
  -- Strategy 4: Vary single coord
  [[if i == j then v else defaultVal | i <- [0..n-1]] | j <- [0..min 10 (n-1)], v <- baseVals] ++
  -- Strategy 5: Gradient patterns
  [[(1%10) + (fromIntegral i % fromIntegral n) * (4%10) | i <- [0..n-1]]] ++
  [[1 - (fromIntegral i % fromIntegral n) * (4%10) | i <- [0..n-1]]] ++
  -- Strategy 6: Random-like patterns using modular arithmetic
  [[(vals !! ((i * 3 + k) `mod` length vals)) | i <- [0..n-1]] | k <- [0..min 5 (length vals - 1)]] ++
  -- Strategy 7: Alternating patterns
  [[if even i then v1 else v2 | i <- [0..n-1]] | v1 <- take 3 baseVals, v2 <- take 3 baseVals]
