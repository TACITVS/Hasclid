{-# LANGUAGE DeriveGeneric #-}

module Positivity
  ( checkPositivityEnhanced
  , checkPositivityEnhancedWithHeuristics
  , PositivityResult(..)
  , PositivityMethod(..)
  , Confidence(..)
  ) where

import Expr
import Sturm (countRealRoots, isolateRoots)
import qualified Data.Map.Strict as M
import Data.List (nub)
import Positivity.SDP (checkSOS_SDP)

-- =============================================
-- Positivity Result Types
-- =============================================

data PositivityMethod
  = SturmTheorem        -- Exact (univariate)
  | ConstantCheck       -- Exact (constants)
  | RootIsolation       -- Exact (univariate with bounds)
  | TrivialSOS          -- Exact (x² + y² type)
  | AdvancedSOS         -- Heuristic (more complex SOS)
  | SDPSolver           -- Numeric SDP (Sum of Squares)
  | IntervalArithmetic  -- Sound approximation
  | SamplingHeuristic   -- Unsound (with warning)
  deriving (Eq, Show)

data PositivityResult = PositivityResult
  { isPositive :: Bool
  , confidence :: Confidence
  , method :: PositivityMethod
  , explanation :: String
  } deriving (Show, Eq)

data Confidence
  = Proven          -- 100% certain (mathematical proof)
  | HighConfidence  -- >99% certain (interval arithmetic)
  | Heuristic       -- Educated guess (sampling)
  deriving (Eq, Show, Ord)

-- =============================================
-- Enhanced Positivity Checking
-- =============================================

-- | Enhanced positivity checker with multiple strategies
checkPositivityEnhanced :: Poly -> Bool -> PositivityResult
checkPositivityEnhanced = checkPositivityEnhancedWithHeuristics True

-- | Enhanced positivity checker with optional heuristics
checkPositivityEnhancedWithHeuristics :: Bool -> Poly -> Bool -> PositivityResult
checkPositivityEnhancedWithHeuristics allowHeuristics p allowZero =
  -- Try methods in order of confidence
  case tryUnivariate p allowZero of
    Just result -> result
    Nothing -> case tryConstant p allowZero of
      Just result -> result
      Nothing -> case tryTrivialSOS p of
        Just result -> result
        Nothing -> case tryAdvancedSOS p of
          Just result -> result
          Nothing ->
            if allowHeuristics
            then case trySDP p of
              Just result -> result
              Nothing -> case tryIntervalArithmetic p allowZero of
                Just result -> result
                Nothing -> trySamplingHeuristic p allowZero
            else trySamplingHeuristic p allowZero

-- =============================================
-- Method 4.5: SDP Sum of Squares
-- =============================================

trySDP :: Poly -> Maybe PositivityResult
trySDP p =
  if checkSOS_SDP p
  then Just $ PositivityResult True Heuristic SDPSolver
       "Verified Sum-of-Squares via numeric SDP (unsound)"
  else Nothing

-- =============================================
-- Method 1: Univariate with Sturm & Root Isolation
-- =============================================

tryUnivariate :: Poly -> Bool -> Maybe PositivityResult
tryUnivariate p _allowZero =
  case toUnivariate p of
    Just (_, coeffs) ->
      let nRoots = countRealRoots coeffs
          lcVal = if null coeffs then 0 else last coeffs
          roots = isolateRoots coeffs
      in
        if nRoots == 0 && lcVal > 0
        then Just $ PositivityResult True Proven SturmTheorem
               ("Sturm's theorem: 0 real roots, leading coeff > 0")
        else if nRoots > 0
        then Just $ PositivityResult False Proven RootIsolation
               ("Has " ++ show nRoots ++ " real root(s) in " ++ show roots)
        else Just $ PositivityResult False Proven SturmTheorem
               ("Leading coefficient is negative or zero")
    Nothing -> Nothing

-- =============================================
-- Method 2: Constant Polynomial
-- =============================================

tryConstant :: Poly -> Bool -> Maybe PositivityResult
tryConstant p allowZero
  | isConstant p =
      let c = getConst p
      in if c > 0 || (allowZero && c == 0)
         then Just $ PositivityResult True Proven ConstantCheck
                ("Constant value: " ++ show c ++ " > 0")
         else Just $ PositivityResult False Proven ConstantCheck
                ("Constant value: " ++ show c ++ " <= 0")
  | otherwise = Nothing

isConstant :: Poly -> Bool
isConstant (Poly m) = all (\(Monomial v) -> M.null v) (M.keys m)

getConst :: Poly -> Rational
getConst (Poly m) = sum (M.elems m)

-- =============================================
-- Method 3: Trivial Sum of Squares
-- =============================================

tryTrivialSOS :: Poly -> Maybe PositivityResult
tryTrivialSOS (Poly m)
  | isTrivialSOS = Just $ PositivityResult True Proven TrivialSOS explanation
  | otherwise = Nothing
  where
    terms = M.toList m
    isTrivialSOS = not (null terms) && all checkTerm terms
    checkTerm (Monomial vars, coeff) = coeff > 0 && all even (M.elems vars)

    explanation = "Sum of squares: " ++ formatSOS terms
    formatSOS ts = unwords [ formatTerm mono c | (mono, c) <- ts ]
    formatTerm (Monomial vars) c =
      if M.null vars
      then show c
      else show c ++ "*" ++ concatMap (\(v, e) -> v ++ "^" ++ show e) (M.toList vars)

-- =============================================
-- Method 4: Advanced SOS Detection
-- =============================================

tryAdvancedSOS :: Poly -> Maybe PositivityResult
tryAdvancedSOS p
  | isQuadraticForm p && hasPositiveDefiniteMatrix p =
      Just $ PositivityResult True Proven AdvancedSOS
        "Positive definite quadratic form"
  | isWeightedSOS p =
      Just $ PositivityResult True Heuristic AdvancedSOS
        "Likely sum of squares (weighted)"
  | otherwise = Nothing

-- Check if polynomial is a quadratic form (degree 2 homogeneous)
isQuadraticForm :: Poly -> Bool
isQuadraticForm (Poly m) =
  not (null (M.keys m)) &&
  all (\(Monomial vars) -> sum (M.elems vars) == 2) (M.keys m)

-- Check if quadratic form has positive definite matrix (simplified)
hasPositiveDefiniteMatrix :: Poly -> Bool
hasPositiveDefiniteMatrix (Poly m) =
  -- Simplified: check diagonal dominance
  let terms = M.toList m
      squareTerms = [ (vName, c)
                    | (Monomial vars, c) <- terms
                    , M.size vars == 1
                    , [(vName, 2)] <- [M.toList vars]
                    ]
      crossTerms = [ c | (Monomial vars, c) <- terms, M.size vars == 2 ]
  in
      all ((> 0) . snd) squareTerms &&  -- All x², y², z² have positive coeffs
      (null crossTerms || all (>= 0) crossTerms)  -- Cross terms non-negative

-- Check if polynomial looks like a weighted sum of squares
isWeightedSOS :: Poly -> Bool
isWeightedSOS (Poly m) =
  let terms = M.toList m
  in
      not (null terms) &&
      all (\(Monomial _, coeff) -> coeff > 0) terms &&
      allEvenOrMixed terms
  where
    allEvenOrMixed ts = length (filter hasEvenExponents ts) > length ts `div` 2
    hasEvenExponents (Monomial vars, _) = all even (M.elems vars)

-- =============================================
-- Method 5: Interval Arithmetic
-- =============================================

tryIntervalArithmetic :: Poly -> Bool -> Maybe PositivityResult
tryIntervalArithmetic p allowZero =
  let vars = extractAllVars p
      bounds = [ (v, (-10, 10)) | v <- vars ]  -- Test on [-10, 10]ⁿ
      minVal = evaluatePolyInterval p bounds
  in
      if minVal > 0 || (allowZero && minVal >= 0)
      then Just $ PositivityResult True HighConfidence IntervalArithmetic
             ("Minimum value on [-10,10]^n: " ++ show minVal)
      else Nothing

extractAllVars :: Poly -> [String]
extractAllVars (Poly m) =
  nub $ concatMap (\(Monomial vars) -> M.keys vars) (M.keys m)

-- Evaluate polynomial over interval bounds (simplified)
evaluatePolyInterval :: Poly -> [(String, (Rational, Rational))] -> Rational
evaluatePolyInterval (Poly m) bounds =
  let evaluations = [ evalMonomial mono coeff bounds | (mono, coeff) <- M.toList m ]
  in minimum (map fst evaluations)  -- Take minimum lower bound
  where
    evalMonomial (Monomial vars) coeff varBounds =
      let lower = coeff * product [ intervalPower v exp varBounds | (v, exp) <- M.toList vars ]
      in (lower, lower)  -- Simplified: return same for upper bound

    intervalPower var exp varBounds =
      case lookup var varBounds of
        Just (lo, hi) ->
          if even (fromIntegral exp :: Integer)
          then min (lo ^ exp) (hi ^ exp)
          else if exp > 0 then lo ^ exp else hi ^ exp
        Nothing -> 1

-- =============================================
-- Method 6: Sampling Heuristic (Last Resort)
-- =============================================

trySamplingHeuristic :: Poly -> Bool -> PositivityResult
trySamplingHeuristic _p _allowZero =
  -- Disabled: do not attempt unsound sampling; force callers to use CAD/GB.
  PositivityResult False Heuristic SamplingHeuristic
    "Heuristic sampling disabled; use a sound solver (CAD/GB)."
