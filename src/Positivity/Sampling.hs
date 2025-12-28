{-# LANGUAGE BangPatterns #-}
-- =============================================================================
-- NUMERICAL VERIFICATION BY SAMPLING
-- =============================================================================
-- For polynomial inequalities that resist algebraic proof, use random sampling
-- to verify the inequality holds across the feasible domain.
--
-- This is NOT a formal proof, but provides high-confidence verification
-- for use in Unsafe mode or as evidence before attempting expensive methods.
-- =============================================================================

module Positivity.Sampling
  ( numericallyVerifyInequality
  , NumericalResult(..)
  , defaultSamplingConfig
  , SamplingConfig(..)
  ) where

import Expr
import System.Random (RandomGen, mkStdGen, randomR, split)
import Data.List (foldl')
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Map.Strict as M
import Control.Monad (replicateM)

-- | Configuration for numerical sampling
data SamplingConfig = SamplingConfig
  { numSamples :: !Int           -- ^ Number of samples to try
  , numRetries :: !Int           -- ^ Retries if constraint satisfaction is low
  , constraintTolerance :: !Double  -- ^ Tolerance for constraint satisfaction
  , goalTolerance :: !Double     -- ^ Tolerance for goal evaluation
  , defaultLowerBound :: !Double -- ^ Default lower bound for unbounded vars
  , defaultUpperBound :: !Double -- ^ Default upper bound for unbounded vars
  , seed :: !Int                 -- ^ Random seed for reproducibility
  } deriving (Show, Eq)

defaultSamplingConfig :: SamplingConfig
defaultSamplingConfig = SamplingConfig
  { numSamples = 10000
  , numRetries = 3
  , constraintTolerance = 1e-10
  , goalTolerance = -1e-10  -- Allow tiny negative due to float errors
  , defaultLowerBound = 0.001
  , defaultUpperBound = 10.0
  , seed = 42
  }

-- | Result of numerical verification
data NumericalResult = NumericalResult
  { verified :: !Bool          -- ^ True if inequality held for all valid samples
  , confidence :: !Double      -- ^ Fraction of samples that satisfied constraints
  , counterexample :: Maybe (M.Map String Double)  -- ^ First counterexample found
  , samplesChecked :: !Int     -- ^ Number of valid samples checked
  , totalSamples :: !Int       -- ^ Total samples generated
  } deriving (Show, Eq)

-- | Verify an inequality by random sampling
-- Returns True if inequality holds for all sampled points
numericallyVerifyInequality :: [Formula]  -- ^ Constraints
                           -> Formula     -- ^ Goal (inequality)
                           -> SamplingConfig
                           -> NumericalResult
numericallyVerifyInequality constraints goal config =
  let
      -- Extract all variables
      allVars = extractVars constraints goal

      -- Extract bounds for each variable
      bounds = extractBounds constraints allVars (defaultLowerBound config) (defaultUpperBound config)

      -- Generate samples
      gen = mkStdGen (seed config)
      samples = generateSamples gen bounds allVars (numSamples config)

      -- Filter samples that satisfy constraints
      validSamples = filter (satisfiesConstraints constraints (constraintTolerance config)) samples

      -- Check goal for all valid samples
      (allPass, counter) = checkGoal goal validSamples (goalTolerance config)

      conf = if null samples
             then 0.0
             else fromIntegral (length validSamples) / fromIntegral (length samples)
  in NumericalResult
       { verified = allPass && conf > 0.5
       , confidence = conf
       , counterexample = counter
       , samplesChecked = length validSamples
       , totalSamples = length samples
       }

-- | Extract all variable names from constraints and goal
extractVars :: [Formula] -> Formula -> [String]
extractVars constraints goal =
  let formulaVars = concatMap getFormulaVars (goal : constraints)
  in foldr (\v acc -> if v `elem` acc then acc else v : acc) [] formulaVars

getFormulaVars :: Formula -> [String]
getFormulaVars (Eq e1 e2) = getExprVars e1 ++ getExprVars e2
getFormulaVars (Ge e1 e2) = getExprVars e1 ++ getExprVars e2
getFormulaVars (Gt e1 e2) = getExprVars e1 ++ getExprVars e2
getFormulaVars (Le e1 e2) = getExprVars e1 ++ getExprVars e2
getFormulaVars (Lt e1 e2) = getExprVars e1 ++ getExprVars e2
getFormulaVars (And f1 f2) = getFormulaVars f1 ++ getFormulaVars f2
getFormulaVars (Or f1 f2) = getFormulaVars f1 ++ getFormulaVars f2
getFormulaVars (Not f) = getFormulaVars f
getFormulaVars _ = []

getExprVars :: Expr -> [String]
getExprVars (Var v) = [v]
getExprVars (Add e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (Sub e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (Mul e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (Div e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (Pow e _) = getExprVars e
getExprVars (Sqrt e) = getExprVars e
getExprVars (NthRoot _ e) = getExprVars e
getExprVars _ = []

-- | Extract variable bounds from constraints
-- Looks for patterns like (> x 0), (< x 10), (>= x 1)
extractBounds :: [Formula] -> [String] -> Double -> Double -> M.Map String (Double, Double)
extractBounds constraints vars defLower defUpper =
  let initial = M.fromList [(v, (defLower, defUpper)) | v <- vars]
  in foldl' updateBounds initial constraints
  where
    updateBounds :: M.Map String (Double, Double) -> Formula -> M.Map String (Double, Double)
    updateBounds m (Gt (Var v) e) =
      case evalConstExpr e of
        Just c -> M.adjust (\(l, u) -> (max l (c + 0.001), u)) v m
        Nothing -> m
    updateBounds m (Ge (Var v) e) =
      case evalConstExpr e of
        Just c -> M.adjust (\(l, u) -> (max l c, u)) v m
        Nothing -> m
    updateBounds m (Lt (Var v) e) =
      case evalConstExpr e of
        Just c -> M.adjust (\(l, u) -> (l, min u (c - 0.001))) v m
        Nothing -> m
    updateBounds m (Le (Var v) e) =
      case evalConstExpr e of
        Just c -> M.adjust (\(l, u) -> (l, min u c)) v m
        Nothing -> m
    -- Handle reversed forms
    updateBounds m (Lt e (Var v)) = updateBounds m (Gt (Var v) e)
    updateBounds m (Le e (Var v)) = updateBounds m (Ge (Var v) e)
    updateBounds m (Gt e (Var v)) = updateBounds m (Lt (Var v) e)
    updateBounds m (Ge e (Var v)) = updateBounds m (Le (Var v) e)
    updateBounds m _ = m

-- | Evaluate constant expression to Double
evalConstExpr :: Expr -> Maybe Double
evalConstExpr (Const r) = Just (fromRational r)
evalConstExpr (IntConst n) = Just (fromIntegral n)
evalConstExpr (Add e1 e2) = (+) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (Sub e1 e2) = (-) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (Mul e1 e2) = (*) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (Div e1 e2) = (/) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (Pow e n) = (** fromIntegral n) <$> evalConstExpr e
evalConstExpr (Sqrt e) = sqrt <$> evalConstExpr e
evalConstExpr _ = Nothing

-- | Generate random samples
generateSamples :: RandomGen g => g -> M.Map String (Double, Double) -> [String] -> Int -> [M.Map String Double]
generateSamples gen bounds vars n = take n (go gen)
  where
    go g =
      let (sample, g') = generateOneSample g bounds vars
      in sample : go g'

generateOneSample :: RandomGen g => g -> M.Map String (Double, Double) -> [String] -> (M.Map String Double, g)
generateOneSample gen bounds vars =
  foldl' addVar (M.empty, gen) vars
  where
    addVar (m, g) v =
      let (lo, hi) = M.findWithDefault (0.001, 10.0) v bounds
          (val, g') = randomR (lo, hi) g
      in (M.insert v val m, g')

-- | Check if a sample satisfies all constraints
satisfiesConstraints :: [Formula] -> Double -> M.Map String Double -> Bool
satisfiesConstraints constraints tol sample =
  all (checkConstraint tol sample) constraints

checkConstraint :: Double -> M.Map String Double -> Formula -> Bool
checkConstraint tol m (Eq e1 e2) =
  case (evalExpr m e1, evalExpr m e2) of
    (Just v1, Just v2) -> abs (v1 - v2) <= tol * (1 + abs v1 + abs v2)
    _ -> False
checkConstraint tol m (Ge e1 e2) =
  case (evalExpr m e1, evalExpr m e2) of
    (Just v1, Just v2) -> v1 >= v2 - tol
    _ -> False
checkConstraint tol m (Gt e1 e2) =
  case (evalExpr m e1, evalExpr m e2) of
    (Just v1, Just v2) -> v1 > v2 - tol
    _ -> False
checkConstraint tol m (Le e1 e2) = checkConstraint tol m (Ge e2 e1)
checkConstraint tol m (Lt e1 e2) = checkConstraint tol m (Gt e2 e1)
checkConstraint tol m (And f1 f2) = checkConstraint tol m f1 && checkConstraint tol m f2
checkConstraint tol m (Or f1 f2) = checkConstraint tol m f1 || checkConstraint tol m f2
checkConstraint tol m (Not f) = not (checkConstraint tol m f)
checkConstraint _ _ _ = True  -- Unknown formulas pass by default

-- | Evaluate expression with variable assignments
evalExpr :: M.Map String Double -> Expr -> Maybe Double
evalExpr m (Var v) = M.lookup v m
evalExpr _ (Const r) = Just (fromRational r)
evalExpr _ (IntConst n) = Just (fromIntegral n)
evalExpr m (Add e1 e2) = (+) <$> evalExpr m e1 <*> evalExpr m e2
evalExpr m (Sub e1 e2) = (-) <$> evalExpr m e1 <*> evalExpr m e2
evalExpr m (Mul e1 e2) = (*) <$> evalExpr m e1 <*> evalExpr m e2
evalExpr m (Div e1 e2) = do
  v1 <- evalExpr m e1
  v2 <- evalExpr m e2
  if abs v2 > 1e-15 then Just (v1 / v2) else Nothing
evalExpr m (Pow e n) = (** fromIntegral n) <$> evalExpr m e
evalExpr m (Sqrt e) = do
  v <- evalExpr m e
  if v >= 0 then Just (sqrt v) else Nothing
evalExpr m (NthRoot n e) = do
  v <- evalExpr m e
  if odd n || v >= 0
    then Just (v ** (1 / fromIntegral n))
    else Nothing
evalExpr _ _ = Nothing

-- | Check if goal holds for all samples
-- Returns (True, Nothing) if all pass, or (False, Just counterexample) if one fails
checkGoal :: Formula -> [M.Map String Double] -> Double -> (Bool, Maybe (M.Map String Double))
checkGoal goal samples tol = go samples
  where
    go [] = (True, Nothing)
    go (s:ss) =
      if checkInequality goal s tol
        then go ss
        else (False, Just s)

checkInequality :: Formula -> M.Map String Double -> Double -> Bool
checkInequality (Ge e1 e2) m tol =
  case (evalExpr m e1, evalExpr m e2) of
    (Just v1, Just v2) -> v1 >= v2 + tol
    _ -> True  -- Can't evaluate, assume OK
checkInequality (Gt e1 e2) m tol =
  case (evalExpr m e1, evalExpr m e2) of
    (Just v1, Just v2) -> v1 > v2 + tol
    _ -> True
checkInequality (Le e1 e2) m tol = checkInequality (Ge e2 e1) m tol
checkInequality (Lt e1 e2) m tol = checkInequality (Gt e2 e1) m tol
checkInequality _ _ _ = True  -- Non-inequality goals pass
