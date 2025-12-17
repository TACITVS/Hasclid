{-|
Module: Preprocessing
Description: Intelligent preprocessing for automatic theorem proving

This module implements preprocessing strategies that commercial theorem provers use:
1. Automatic variable substitution from theory
2. 2D geometry detection and z-coordinate elimination
3. Early simplification before expensive polynomial operations
4. Constraint propagation and redundancy elimination

PHILOSOPHY: A true automatic theorem prover must be intelligent about problem structure,
not just apply brute force algorithms.
-}

module Preprocessing
  ( preprocess
  , PreprocessingResult(..)
  , detectDimension
  , extractSubstitutions
  , applySubstitutionsFormula
  , applySubstitutionsExpr
  , simplifyTheory
  ) where

import Expr
import Data.List (nub, find)
import Data.Maybe (mapMaybe, isJust)
import qualified Data.Map.Strict as M

-- | Result of preprocessing
data PreprocessingResult = PreprocessingResult
  { preprocessedTheory :: Theory
  , preprocessedGoal :: Formula
  , substitutions :: M.Map String Expr  -- Variable -> Value substitutions
  , dimension :: GeometryDimension
  , eliminatedVars :: [String]
  , simplificationLog :: [String]
  } deriving (Show, Eq)

data GeometryDimension = Dimension1D | Dimension2D | Dimension3D | DimensionUnknown
  deriving (Show, Eq)

-- | Main preprocessing pipeline
preprocess :: M.Map String Expr -> Theory -> Formula -> PreprocessingResult
preprocess initialSubs theory goal =
  let
    -- Step 1: Extract simple substitutions from theory (x = c, y = 0, etc.)
    extractedSubs = extractSubstitutions theory
    -- Merge with initial substitutions (initialSubs takes precedence for point coordinates)
    subs = M.union initialSubs extractedSubs
    log1 = ["Extracted " ++ show (M.size extractedSubs) ++ " substitutions from theory, " ++
            show (M.size initialSubs) ++ " from point definitions: " ++
            show (take 5 $ M.toList subs)]

    -- Step 2: Detect geometry dimension (check both theory and substitutions)
    dim = detectDimension subs theory goal
    log2 = log1 ++ ["Detected dimension: " ++ show dim]

    -- Step 3: Add dimension-specific substitutions (e.g., all z-coords = 0 for 2D)
    subs' = case dim of
              Dimension2D -> addZeroZCoordinates theory subs
              _ -> subs
    log3 = if dim == Dimension2D && M.size subs' > M.size subs
           then log2 ++ ["2D detected: eliminated " ++ show (M.size subs' - M.size subs) ++ " z-coordinates"]
           else log2

    -- Step 4: Apply substitutions to theory and goal
    theory' = map (applySubstitutionsFormula subs') theory
    goal' = applySubstitutionsFormula subs' goal
    log4 = log3 ++ ["Applied " ++ show (M.size subs') ++ " substitutions"]

    -- Step 5: Simplify theory (remove tautologies, redundant constraints)
    theory'' = simplifyTheory theory'
    log5 = if length theory'' < length theory'
           then log4 ++ ["Simplified theory: " ++ show (length theory' - length theory'') ++ " redundant constraints removed"]
           else log4

  in PreprocessingResult
       { preprocessedTheory = theory''
       , preprocessedGoal = goal'
       , substitutions = subs'
       , dimension = dim
       , eliminatedVars = M.keys subs'
       , simplificationLog = log5
       }

-- | Detect geometry dimension by examining theory constraints and substitutions
-- INTELLIGENCE: If all z-coordinates are constrained to 0, it's 2D geometry
detectDimension :: M.Map String Expr -> Theory -> Formula -> GeometryDimension
detectDimension subs theory goal =
  let
    allFormulas = goal : theory
    zVars = filter (isPrefixOf "z") $ nub $ concatMap varsInFormula allFormulas

    -- Check if all z-variables are explicitly set to 0 (in theory or substitutions)
    allZerosAre0 = all (\zv -> hasConstraint zv (Const 0) theory || hasSubstitution zv (Const 0) subs) zVars

    -- If we have z-variables and they're all 0, it's 2D
    -- If we have no z-variables at all, assume 2D
    hasZVars = not (null zVars)
  in
    if hasZVars && allZerosAre0 then Dimension2D
    else if not hasZVars then Dimension2D  -- No z-vars means 2D
    else DimensionUnknown

  where
    isPrefixOf pre str = take (length pre) str == pre

    hasConstraint var val formulas =
      any (\f -> case f of
                   Eq (Var v) e -> v == var && e == val
                   Eq e (Var v) -> v == var && e == val
                   _ -> False) formulas

    hasSubstitution var val subsMap =
      case M.lookup var subsMap of
        Just e -> e == val
        Nothing -> False

-- | Extract simple substitutions from theory
-- PATTERNS:
--   (= x c) where c is a constant → x ↦ c
--   (= x 0) → x ↦ 0
--   (= (+ x c1) c2) → x ↦ c2 - c1
extractSubstitutions :: Theory -> M.Map String Expr
extractSubstitutions theory =
  M.fromList $ mapMaybe extractSub theory
  where
    extractSub :: Formula -> Maybe (String, Expr)
    extractSub (Eq (Var v) e) | isConstantExpr e = Just (v, e)
    extractSub (Eq e (Var v)) | isConstantExpr e = Just (v, e)
    -- TODO: Add more sophisticated extraction patterns
    extractSub _ = Nothing

    -- Check if expression contains only constants (no variables)
    isConstantExpr :: Expr -> Bool
    isConstantExpr (Const _) = True
    isConstantExpr (IntConst _) = True
    isConstantExpr (Add e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Sub e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Mul e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Div e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Pow e _) = isConstantExpr e
    isConstantExpr _ = False

-- | For 2D geometry, add z-coordinate = 0 substitutions
addZeroZCoordinates :: Theory -> M.Map String Expr -> M.Map String Expr
addZeroZCoordinates theory subs =
  let
    allVars = nub $ concatMap varsInFormula theory
    zVars = filter (\v -> take 1 v == "z" && length v > 1) allVars
    zeroSubs = [(zv, Const 0) | zv <- zVars, not (M.member zv subs)]
  in M.union subs (M.fromList zeroSubs)

-- | Apply substitutions to a formula
applySubstitutionsFormula :: M.Map String Expr -> Formula -> Formula
applySubstitutionsFormula subs = mapFormula (applySubstitutionsExpr subs)

-- | Apply substitutions to an expression
-- Note: Midpoint and Parallel are now expanded at the :assume level, not here
applySubstitutionsExpr :: M.Map String Expr -> Expr -> Expr
applySubstitutionsExpr subs = go
  where
    go (Var v) = case M.lookup v subs of
                   Just e -> e
                   Nothing -> Var v
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Sub e1 e2) = Sub (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (Div e1 e2) = Div (go e1) (go e2)
    go (Mod e1 e2) = Mod (go e1) (go e2)
    go (Pow e n) = Pow (go e) n
    go (Sqrt e) = Sqrt (go e)
    go (Dist2 p1 p2) =
      -- Expand dist2 to coordinate form so substitutions can be applied
      let x1 = Var ("x" ++ p1); y1 = Var ("y" ++ p1); z1 = Var ("z" ++ p1)
          x2 = Var ("x" ++ p2); y2 = Var ("y" ++ p2); z2 = Var ("z" ++ p2)
          dx = Sub (go x1) (go x2)
          dy = Sub (go y1) (go y2)
          dz = Sub (go z1) (go z2)
      in Add (Add (Mul dx dx) (Mul dy dy)) (Mul dz dz)
    go (Collinear p1 p2 p3) =
      -- Expand collinear to coordinate form (determinant)
      Collinear p1 p2 p3  -- TODO: expand if needed
    go (Dot p1 p2 p3 p4) =
      -- Expand dot product to coordinate form
      Dot p1 p2 p3 p4  -- TODO: expand if needed
    go (Circle p c r) =
      -- Expand circle to distance form
      let distSq = go (Dist2 p c)
          radSq = Pow (go r) 2
      in Sub distSq radSq
    go (Midpoint p1 p2 p3) = Midpoint p1 p2 p3  -- Keep as-is, expanded at assume level
    go (Perpendicular p1 p2 p3 p4) = Perpendicular p1 p2 p3 p4
    go (Parallel p1 p2 p3 p4) = Parallel p1 p2 p3 p4  -- Keep as-is, expanded at assume level
    go (AngleEq2D p1 p2 p3 p4 p5 p6) = AngleEq2D p1 p2 p3 p4 p5 p6
    go (Determinant rows) = Determinant (map (map go) rows)
    go e = e  -- Constants, IntConst, IntVar

-- | Map a function over all expressions in a formula
mapFormula :: (Expr -> Expr) -> Formula -> Formula
mapFormula f (Eq e1 e2) = Eq (f e1) (f e2)
mapFormula f (Le e1 e2) = Le (f e1) (f e2)
mapFormula f (Lt e1 e2) = Lt (f e1) (f e2)
mapFormula f (Ge e1 e2) = Ge (f e1) (f e2)
mapFormula f (Gt e1 e2) = Gt (f e1) (f e2)
mapFormula f (Divides e1 e2) = Divides (f e1) (f e2)
mapFormula f (And f1 f2) = And (mapFormula f f1) (mapFormula f f2)
mapFormula f (Or f1 f2) = Or (mapFormula f f1) (mapFormula f f2)
mapFormula f (Not form) = Not (mapFormula f form)
mapFormula f (Forall qvs form) = Forall qvs (mapFormula f form)
mapFormula f (Exists qvs form) = Exists qvs (mapFormula f form)

-- | Simplify theory by removing tautologies and redundant constraints
simplifyTheory :: Theory -> Theory
simplifyTheory theory =
  let
    -- Remove obvious tautologies like (= 0 0), (= x x)
    notTautology (Eq e1 e2) = e1 /= e2
    notTautology (Le (Const c1) (Const c2)) = c1 > c2  -- Keep only non-trivial
    notTautology _ = True

    -- Remove duplicates
    simplified = nub $ filter notTautology theory
  in simplified

-- | Get all variables mentioned in a formula
varsInFormula :: Formula -> [String]
varsInFormula (Eq e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Le e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Lt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Ge e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Gt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Divides e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (And f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Or f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Not f) = varsInFormula f
varsInFormula (Forall _ f) = varsInFormula f
varsInFormula (Exists _ f) = varsInFormula f

-- | Get all variables in an expression
varsInExpr :: Expr -> [String]
varsInExpr (Var v) = [v]
varsInExpr (Add e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Sub e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Mul e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Div e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Mod e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Pow e _) = varsInExpr e
varsInExpr (Sqrt e) = varsInExpr e
varsInExpr (Dist2 p1 p2) = ["x"++p1, "y"++p1, "z"++p1, "x"++p2, "y"++p2, "z"++p2]
varsInExpr (Collinear p1 p2 p3) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3]
varsInExpr (Dot p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (Circle _ _ r) = varsInExpr r
varsInExpr (Midpoint p1 p2 p3) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3]
varsInExpr (Perpendicular p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (Parallel p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (AngleEq2D p1 p2 p3 p4 p5 p6) = concatMap (\p -> ["x"++p, "y"++p]) [p1, p2, p3, p4, p5, p6]
varsInExpr (Determinant rows) = concatMap (concatMap varsInExpr) rows
varsInExpr _ = []
