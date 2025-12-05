{-|
Module: ProblemAnalyzer
Description: Analyze theorem proving problems to determine optimal solver strategy

This module analyzes the structure of a problem (theory + goal) to determine:
- Problem complexity (number of variables, degree, etc.)
- Problem type (algebraic, geometric, inequality, etc.)
- Best solver recommendation (Gröbner, Wu, CAD, etc.)

DESIGN: This is a PURE ADDITION - it doesn't modify any existing code.
It can be used by a router, or ignored if you want to use manual methods.
-}

module ProblemAnalyzer
  ( -- * Problem Profile
    ProblemProfile(..)
  , Complexity(..)
  , ProblemType(..)
  , GeometricFeatures(..)

    -- * Analysis Functions
  , analyzeProblem
  , analyzeTheory
  , analyzeFormula
  , estimateComplexity
  , classifyProblem

    -- * Utility Functions
  , extractVariables
  , extractSymbolicParams
  , countPolynomials
  ) where

import Expr
import Data.List (nub)

-- =============================================
-- Data Types
-- =============================================

-- | Complete profile of a theorem proving problem
data ProblemProfile = ProblemProfile
  { numVariables :: Int              -- Total number of variables
  , numConstraints :: Int            -- Number of assumptions
  , maxDegree :: Int                 -- Highest polynomial degree
  , hasSymbolicParams :: Bool        -- Has parameters like S, T, etc.
  , symbolicParams :: [String]       -- List of symbolic parameters
  , problemType :: ProblemType       -- Classification
  , geometricFeatures :: GeometricFeatures  -- Geometric characteristics
  , estimatedComplexity :: Complexity -- Computational complexity estimate
  , variables :: [String]            -- All variables in problem
  } deriving (Show, Eq)

-- | Problem complexity level
data Complexity
  = Trivial       -- Can solve instantly (<1s)
  | Low           -- Easy (1-5s)
  | Medium        -- Moderate (5-30s)
  | High          -- Hard (30s-5min)
  | VeryHigh      -- Very hard (5min+)
  | Infeasible    -- Too complex for automatic solving
  deriving (Show, Eq, Ord)

-- | Problem classification
data ProblemType
  = PureAlgebraic        -- Only polynomial equations
  | PureInequality       -- Only inequalities
  | Mixed                -- Equations and inequalities
  | Geometric            -- Geometric constraints (distance, perp, etc.)
  | SinglePositivity     -- Just proving p > 0
  | Unknown              -- Cannot classify
  deriving (Show, Eq)

-- | Geometric problem characteristics
data GeometricFeatures = GeometricFeatures
  { hasDistanceConstraints :: Bool
  , hasPerpendicularityConstraints :: Bool
  , hasCollinearityConstraints :: Bool
  , hasParallelConstraints :: Bool
  , numPoints :: Int
  } deriving (Show, Eq)

-- =============================================
-- Main Analysis Function
-- =============================================

-- | Analyze a complete problem: theory + goal
analyzeProblem :: Theory -> Formula -> ProblemProfile
analyzeProblem theory goal =
  let
      -- Collect all variables
      theoryVars = concatMap extractVarsFromFormula theory
      goalVars = extractVarsFromFormula goal
      allVars = nub (theoryVars ++ goalVars)

      -- Identify symbolic parameters (single uppercase letters)
      symbolicParams' = filter isSymbolicParam allVars

      -- Count constraints
      numCon = length theory

      -- Analyze geometric features
      geoFeatures = analyzeGeometricFeatures theory goal

      -- Determine problem type
      probType = classifyProblem theory goal geoFeatures

      -- Estimate polynomial degrees (approximate)
      maxDeg = estimateMaxDegree theory goal

      -- Estimate complexity
      complexity = estimateComplexity
                    (length allVars)
                    numCon
                    maxDeg
                    (not $ null symbolicParams')
                    probType
  in
      ProblemProfile
        { numVariables = length allVars
        , numConstraints = numCon
        , maxDegree = maxDeg
        , hasSymbolicParams = not (null symbolicParams')
        , symbolicParams = symbolicParams'
        , problemType = probType
        , geometricFeatures = geoFeatures
        , estimatedComplexity = complexity
        , variables = allVars
        }

-- =============================================
-- Theory Analysis
-- =============================================

-- | Analyze just the theory (assumptions)
analyzeTheory :: Theory -> ProblemProfile
analyzeTheory theory = analyzeProblem theory (Eq (Const 0) (Const 0))

-- | Analyze just a formula
analyzeFormula :: Formula -> ProblemProfile
analyzeFormula formula = analyzeProblem [] formula

-- =============================================
-- Problem Classification
-- =============================================

-- | Classify the type of problem
classifyProblem :: Theory -> Formula -> GeometricFeatures -> ProblemType
classifyProblem theory goal geoFeatures
  | any containsQuantifier (goal : theory) = Unknown
  -- Single inequality goal, no equations
  | null theory && isSingleInequality goal = SinglePositivity

  -- Has geometric primitives
  | hasGeometricPrimitives geoFeatures = Geometric

  -- All equations
  | all isEquality theory && isEquality goal = PureAlgebraic

  -- All inequalities
  | all isInequality theory && isInequality goal = PureInequality

  -- Mixed
  | any isInequality theory || isInequality goal = Mixed

  -- Default
  | otherwise = Unknown
  where
    isEquality (Eq _ _) = True
    isEquality (Forall _ f) = isEquality f
    isEquality (Exists _ f) = isEquality f
    isEquality _ = False

    isInequality (Ge _ _) = True
    isInequality (Gt _ _) = True
    isInequality (Forall _ f) = isInequality f
    isInequality (Exists _ f) = isInequality f
    isInequality _ = False

    isSingleInequality (Ge _ _) = True
    isSingleInequality (Gt _ _) = True
    isSingleInequality _ = False

    hasGeometricPrimitives gf =
      hasDistanceConstraints gf ||
      hasPerpendicularityConstraints gf ||
      hasCollinearityConstraints gf ||
      hasParallelConstraints gf

-- | Analyze geometric features in the problem
analyzeGeometricFeatures :: Theory -> Formula -> GeometricFeatures
analyzeGeometricFeatures theory goal =
  let allFormulas = goal : theory
      allExprs = concatMap extractExprsFromFormula allFormulas
  in GeometricFeatures
       { hasDistanceConstraints = any isDist2 allExprs
       , hasPerpendicularityConstraints = any isPerpendicular allExprs
       , hasCollinearityConstraints = any isCollinear allExprs
       , hasParallelConstraints = any isParallel allExprs
       , numPoints = countUniquePoints allExprs
       }
  where
    isDist2 (Dist2 _ _) = True
    isDist2 _ = False

    isPerpendicular (Perpendicular _ _ _ _) = True
    isPerpendicular _ = False

    isCollinear (Collinear _ _ _) = True
    isCollinear _ = False

    isParallel (Parallel _ _ _ _) = True
    isParallel _ = False

    countUniquePoints exprs = length $ nub $ concatMap extractPointNames exprs

    extractPointNames expr = case expr of
      Dist2 p1 p2 -> [p1, p2]
      Perpendicular p1 p2 p3 p4 -> [p1, p2, p3, p4]
      Collinear p1 p2 p3 -> [p1, p2, p3]
      Parallel p1 p2 p3 p4 -> [p1, p2, p3, p4]
      _ -> []

-- =============================================
-- Complexity Estimation
-- =============================================

-- | Estimate computational complexity of a problem
estimateComplexity :: Int -> Int -> Int -> Bool -> ProblemType -> Complexity
estimateComplexity numVars numCons maxDeg hasSymbolic probType
  -- Trivial cases
  | numVars == 0 = Trivial
  | numCons == 0 = Trivial

  -- Single positivity check
  | probType == SinglePositivity && numVars <= 1 = Low
  | probType == SinglePositivity && numVars <= 3 = Medium

  -- Geometric problems (Wu's method scales well)
  | probType == Geometric && numVars <= 4 = Low
  | probType == Geometric && numVars <= 8 = Medium
  | probType == Geometric = High

  -- CAD-suitable problems (1D-2D, numeric)
  | probType == PureInequality && numVars == 1 && not hasSymbolic = Low
  | probType == PureInequality && numVars == 2 && not hasSymbolic = Medium
  | probType == PureInequality && numVars >= 3 = VeryHigh

  -- Gröbner basis problems
  | numVars <= 3 && maxDeg <= 3 = Low
  | numVars <= 4 && maxDeg <= 3 = Medium
  | numVars <= 5 && maxDeg <= 4 = High
  | numVars >= 6 && maxDeg >= 5 = VeryHigh
  | hasSymbolic && numVars >= 4 = High

  -- Default: based on variable count
  | numVars <= 2 = Low
  | numVars <= 4 = Medium
  | numVars <= 6 = High
  | otherwise = VeryHigh

-- =============================================
-- Variable and Parameter Extraction
-- =============================================

-- | Extract all variables from a theory
extractVariables :: Theory -> [String]
extractVariables theory = nub $ concatMap extractVarsFromFormula theory

-- | Extract symbolic parameters (like S, T - single uppercase letters)
extractSymbolicParams :: Theory -> Formula -> [String]
extractSymbolicParams theory goal =
  let allVars = extractVariables (goal : theory)
  in filter isSymbolicParam allVars

-- | Check if a variable name looks like a symbolic parameter
isSymbolicParam :: String -> Bool
isSymbolicParam [c] = c `elem` "STUVWPQR"  -- Common parameter names
isSymbolicParam _ = False

-- | Extract variables from a formula
extractVarsFromFormula :: Formula -> [String]
extractVarsFromFormula formula =
  case formula of
    Forall qs f ->
      let bound = map qvName qs
      in filter (`notElem` bound) (extractVarsFromFormula f)
    Exists qs f ->
      let bound = map qvName qs
      in filter (`notElem` bound) (extractVarsFromFormula f)
    _ ->
      nub $ concatMap extractVarsFromExpr (extractExprsFromFormula formula)

-- | Extract all expressions from a formula
extractExprsFromFormula :: Formula -> [Expr]
extractExprsFromFormula (Eq l r) = [l, r]
extractExprsFromFormula (Ge l r) = [l, r]
extractExprsFromFormula (Gt l r) = [l, r]
extractExprsFromFormula (Forall _ f) = extractExprsFromFormula f
extractExprsFromFormula (Exists _ f) = extractExprsFromFormula f

-- | Extract all variable names from an expression
extractVarsFromExpr :: Expr -> [String]
extractVarsFromExpr (Var v) = [v]
extractVarsFromExpr (Const _) = []
extractVarsFromExpr (IntVar v) = [v]
extractVarsFromExpr (IntConst _) = []
extractVarsFromExpr (Add e1 e2) = extractVarsFromExpr e1 ++ extractVarsFromExpr e2
extractVarsFromExpr (Sub e1 e2) = extractVarsFromExpr e1 ++ extractVarsFromExpr e2
extractVarsFromExpr (Mul e1 e2) = extractVarsFromExpr e1 ++ extractVarsFromExpr e2
extractVarsFromExpr (Div e1 e2) = extractVarsFromExpr e1 ++ extractVarsFromExpr e2
extractVarsFromExpr (Pow e _) = extractVarsFromExpr e
extractVarsFromExpr (Sqrt e) = extractVarsFromExpr e
extractVarsFromExpr (Dist2 p1 p2) = [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y"]
extractVarsFromExpr (Collinear p1 p2 p3) =
  [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y", p3 ++ "x", p3 ++ "y"]
extractVarsFromExpr (Dot p1 p2 p3 p4) =
  [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y",
   p3 ++ "x", p3 ++ "y", p4 ++ "x", p4 ++ "y"]
extractVarsFromExpr (Circle p1 p2 _) = [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y"]
extractVarsFromExpr (Midpoint p1 p2 p3) =
  [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y", p3 ++ "x", p3 ++ "y"]
extractVarsFromExpr (Perpendicular p1 p2 p3 p4) =
  [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y",
   p3 ++ "x", p3 ++ "y", p4 ++ "x", p4 ++ "y"]
extractVarsFromExpr (Parallel p1 p2 p3 p4) =
  [p1 ++ "x", p1 ++ "y", p2 ++ "x", p2 ++ "y",
   p3 ++ "x", p3 ++ "y", p4 ++ "x", p4 ++ "y"]
extractVarsFromExpr (AngleEq2D a b c d e f) =
  [a ++ "x", a ++ "y", b ++ "x", b ++ "y", c ++ "x", c ++ "y",
   d ++ "x", d ++ "y", e ++ "x", e ++ "y", f ++ "x", f ++ "y"]
extractVarsFromExpr (AngleEq2DAbs a b c d e f) =
  [a ++ "x", a ++ "y", b ++ "x", b ++ "y", c ++ "x", c ++ "y",
   d ++ "x", d ++ "y", e ++ "x", e ++ "y", f ++ "x", f ++ "y"]
extractVarsFromExpr (Determinant rows) = concatMap extractVarsFromExpr (concat rows)

-- =============================================
-- Polynomial Analysis Utilities
-- =============================================

-- | Count polynomials in theory
countPolynomials :: Theory -> Int
countPolynomials = length

-- | Estimate maximum polynomial degree (conservative)
estimateMaxDegree :: Theory -> Formula -> Int
estimateMaxDegree theory goal =
  let allFormulas = goal : theory
      allExprs = concatMap extractExprsFromFormula allFormulas
      degrees = map estimateExprDegree allExprs
  in if null degrees then 0 else maximum degrees

-- | Estimate degree of an expression (conservative upper bound)
estimateExprDegree :: Expr -> Int
estimateExprDegree (Var _) = 1
estimateExprDegree (Const _) = 0
estimateExprDegree (IntVar _) = 1
estimateExprDegree (IntConst _) = 0
estimateExprDegree (Add e1 e2) = max (estimateExprDegree e1) (estimateExprDegree e2)
estimateExprDegree (Sub e1 e2) = max (estimateExprDegree e1) (estimateExprDegree e2)
estimateExprDegree (Mul e1 e2) = estimateExprDegree e1 + estimateExprDegree e2
estimateExprDegree (Div e1 e2) = estimateExprDegree e1 + estimateExprDegree e2
estimateExprDegree (Pow e n) = fromIntegral n * estimateExprDegree e
estimateExprDegree (Sqrt e) = max 1 (estimateExprDegree e)
estimateExprDegree (Dist2 _ _) = 2  -- (x1-x2)^2 + (y1-y2)^2
estimateExprDegree (Collinear _ _ _) = 2
estimateExprDegree (Dot _ _ _ _) = 2
estimateExprDegree (Circle _ _ _) = 2
estimateExprDegree (Midpoint _ _ _) = 1
estimateExprDegree (Perpendicular _ _ _ _) = 2
estimateExprDegree (Parallel _ _ _ _) = 2
-- Angle equality polynomial reaches degree up to 12 (cosDiff/sinDiff degree 6, then squared)
estimateExprDegree (AngleEq2D _ _ _ _ _ _) = 12
estimateExprDegree (AngleEq2DAbs _ _ _ _ _ _) = 12
estimateExprDegree (Determinant rows) =
  let rowDegrees = map (map estimateExprDegree) rows
      -- crude upper bound: sum of max degrees per row (Laplace expansion worst case)
      maxRow = maximum (0 : map maximum rowDegrees)
  in maxRow * length rows
