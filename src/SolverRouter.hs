{-|
Module: SolverRouter
Description: Intelligent routing system for automatic solver selection

This module analyzes a theorem proving problem and automatically selects
the most appropriate solving method based on problem characteristics.

Available solvers:
- Gröbner Basis: General-purpose algebraic equation solver
- Wu's Method: Optimized for geometric theorem proving
- CAD: Cylindrical Algebraic Decomposition for inequalities

The router uses ProblemAnalyzer to classify problems and estimate complexity,
then selects the solver most likely to succeed efficiently.

DESIGN: This is a PURE ADDITION that provides :auto command.
All existing commands (:prove, :wu, :cad) remain unchanged.
-}

module SolverRouter
  ( -- * Automatic Solver Selection
    autoSolve
  , autoSolveWithTrace

    -- * Solver Selection Logic
  , SolverChoice(..)
  , selectAlgebraicSolver
  , explainSolverChoice

    -- * Result Types
  , AutoSolveResult(..)
  , formatAutoSolveResult
  ) where

import Expr
import ProblemAnalyzer
import GeoSolver (solveGeoWithTrace, GeoResult(..), formatGeoResult)
import Wu (wuProve, wuProveWithTrace, WuTrace, formatWuTrace)
import Prover (proveTheoryWithOptions, ProofTrace, formatProofTrace, buchberger)
import CADLift (evaluateInequalityCAD)
import qualified Data.Set as S

-- =============================================
-- Data Types
-- =============================================

-- | Available solving methods
data SolverChoice
  = UseGeoSolver    -- Geometric constraint propagation (FASTEST - Phase 1)
  | UseWu           -- Wu's method (fast for geometry)
  | UseGroebner     -- Gröbner basis (general purpose)
  | UseCAD          -- CAD (for inequalities, limited to 1D-2D)
  | Unsolvable      -- Problem too complex or type not supported
  deriving (Show, Eq)

-- | Result of automatic solver selection and execution
data AutoSolveResult = AutoSolveResult
  { selectedSolver :: SolverChoice
  , solverReason :: String           -- Why this solver was chosen
  , problemProfile :: ProblemProfile -- Analysis of the problem
  , isProved :: Bool                 -- Was the theorem proved?
  , proofReason :: String            -- Explanation of result
  , detailedTrace :: Maybe String    -- Optional detailed trace
  } deriving (Show, Eq)

-- =============================================
-- Main Automatic Solving Functions
-- =============================================

-- | Automatically select and run the best solver for a problem
-- ARCHITECTURE: Two-phase solving approach
--
-- PHASE 1: Fast Geometric Constraint Propagation (the "screwdriver")
--   Try GeoSolver first - uses constraint propagation, not polynomial algebra
--   Returns in milliseconds with GeoProved/GeoDisproved/GeoUnknown
--
-- PHASE 2: Algebraic Solvers (the "hammers")
--   If GeoSolver returns GeoUnknown, fall back to Wu/Gröbner/CAD
--   These use polynomial manipulation - slower but more general
autoSolve :: Theory -> Formula -> AutoSolveResult
autoSolve theory goal =
  let
    -- Analyze the problem structure
    profile = analyzeProblem theory goal
  in
    -- PHASE 1: Try fast geometric constraint propagation first
    case solveGeoWithTrace theory goal of
      GeoProved reason steps ->
        -- Success! GeoSolver proved it via constraint propagation
        AutoSolveResult
          { selectedSolver = UseGeoSolver
          , solverReason = "Fast geometric constraint propagation (PHASE 1)"
          , problemProfile = profile
          , isProved = True
          , proofReason = reason
          , detailedTrace = Just (unlines steps)
          }

      GeoDisproved reason steps ->
        -- Success! GeoSolver disproved it via constraint propagation
        AutoSolveResult
          { selectedSolver = UseGeoSolver
          , solverReason = "Fast geometric constraint propagation (PHASE 1)"
          , problemProfile = profile
          , isProved = False
          , proofReason = reason
          , detailedTrace = Just (unlines steps)
          }

      GeoUnknown reason ->
        -- GeoSolver insufficient, fall back to PHASE 2 (algebraic solvers)
        let
          -- Select appropriate algebraic solver
          solver = selectAlgebraicSolver profile goal

          -- Get explanation for choice
          solverReason' = "Geometric propagation insufficient. Fallback to PHASE 2: " ++
                         explainSolverChoice solver profile

          -- Execute the chosen algebraic solver
          (proved, proofMsg, trace) = executeSolver solver theory goal
        in
          AutoSolveResult
            { selectedSolver = solver
            , solverReason = solverReason'
            , problemProfile = profile
            , isProved = proved
            , proofReason = proofMsg
            , detailedTrace = trace
            }

-- | Automatic solve with verbose trace information
autoSolveWithTrace :: Theory -> Formula -> Bool -> AutoSolveResult
autoSolveWithTrace theory goal verbose =
  let result = autoSolve theory goal
  in result

-- =============================================
-- Solver Selection Logic
-- =============================================

-- | Select the most appropriate ALGEBRAIC solver (PHASE 2)
-- This is called only if GeoSolver returns GeoUnknown in PHASE 1
selectAlgebraicSolver :: ProblemProfile -> Formula -> SolverChoice
selectAlgebraicSolver profile goal
  -- RULE 1: Unsupported formula types
  | not (isEquality goal) && not (isInequality goal) = Unsolvable

  -- RULE 2: Too complex (avoid hanging)
  | estimatedComplexity profile >= VeryHigh = Unsolvable

  -- RULE 3: Pure geometric problems → Wu's method
  | problemType profile == Geometric = UseWu

  -- RULE 4: Inequalities with 1-2 variables → CAD
  | isInequality goal && numVariables profile <= 2 && not (hasSymbolicParams profile) = UseCAD

  -- RULE 5: Pure algebraic with symbolic parameters → Wu's method
  -- (Wu handles symbolic parameters better than Gröbner for geometry)
  | problemType profile == PureAlgebraic && hasSymbolicParams profile &&
    hasGeometricVars profile = UseWu

  -- RULE 6: Single positivity check (p > 0) with few vars → CAD
  | problemType profile == SinglePositivity && numVariables profile <= 2 = UseCAD

  -- RULE 7: Pure algebraic equations → Gröbner basis (general purpose)
  | problemType profile == PureAlgebraic = UseGroebner

  -- RULE 8: Mixed equations and inequalities → Use Gröbner for equations part
  | problemType profile == Mixed && isEquality goal = UseGroebner

  -- DEFAULT: Gröbner basis (most reliable general-purpose method)
  | otherwise = UseGroebner

-- | Check if formula is an equality
isEquality :: Formula -> Bool
isEquality (Eq _ _) = True
isEquality _ = False

-- | Check if formula is an inequality
isInequality :: Formula -> Bool
isInequality (Ge _ _) = True
isInequality (Gt _ _) = True
isInequality _ = False

-- | Check if variables suggest geometric origin (coordinate names like xA, yB, etc.)
hasGeometricVars :: ProblemProfile -> Bool
hasGeometricVars profile =
  any isGeometricVar (variables profile)
  where
    isGeometricVar ('x':_) = True
    isGeometricVar ('y':_) = True
    isGeometricVar ('z':_) = True
    isGeometricVar _ = False

-- =============================================
-- Solver Execution
-- =============================================

-- | Execute the selected solver
-- Returns: (is_proved, reason, optional_trace)
executeSolver :: SolverChoice -> Theory -> Formula -> (Bool, String, Maybe String)

executeSolver UseWu theory goal =
  case goal of
    Eq _ _ ->
      let (proved, reason) = wuProve theory goal
          trace = wuProveWithTrace theory goal
      in (proved, reason, Just (formatWuTrace trace))
    _ -> (False, "Wu's method only supports equality goals", Nothing)

executeSolver UseGroebner theory goal =
  case goal of
    Eq _ _ ->
      let (proved, reason, trace, _) = proveTheoryWithOptions buchberger Nothing theory goal
      in (proved, reason, Just (formatProofTrace trace))
    _ -> (False, "Gröbner basis method only supports equality goals", Nothing)

executeSolver UseCAD theory goal =
  case goal of
    Ge l r -> executeCADInequality theory l r False
    Gt l r -> executeCADInequality theory l r True
    _ -> (False, "CAD only supports inequality goals (>, >=)", Nothing)

executeSolver Unsolvable _ _ =
  (False, "Problem is too complex or type not supported by automatic solver", Nothing)

-- | Execute CAD for an inequality
executeCADInequality :: Theory -> Expr -> Expr -> Bool -> (Bool, String, Maybe String)
executeCADInequality theory lhs rhs _isStrict =
  -- Extract variables (simplified - full CAD needs more work)
  let vars = extractVariablesFromExpr lhs ++ extractVariablesFromExpr rhs
      uniqueVars = S.toList $ S.fromList vars
  in if length uniqueVars > 2
     then (False, "CAD limited to 1-2 variables (3D+ causes exponential blowup)", Nothing)
     else
       -- For now, return unsupported - full CAD integration needs more work
       (False, "CAD inequality proving not yet fully integrated with router", Nothing)

-- | Extract variable names from an expression
extractVariablesFromExpr :: Expr -> [String]
extractVariablesFromExpr (Var v) = [v]
extractVariablesFromExpr (Const _) = []
extractVariablesFromExpr (Add e1 e2) = extractVariablesFromExpr e1 ++ extractVariablesFromExpr e2
extractVariablesFromExpr (Sub e1 e2) = extractVariablesFromExpr e1 ++ extractVariablesFromExpr e2
extractVariablesFromExpr (Mul e1 e2) = extractVariablesFromExpr e1 ++ extractVariablesFromExpr e2
extractVariablesFromExpr (Div e1 e2) = extractVariablesFromExpr e1 ++ extractVariablesFromExpr e2
extractVariablesFromExpr (Pow e _) = extractVariablesFromExpr e
extractVariablesFromExpr _ = []

-- =============================================
-- Explanation and Formatting
-- =============================================

-- | Explain why a particular solver was chosen
explainSolverChoice :: SolverChoice -> ProblemProfile -> String
explainSolverChoice UseGeoSolver profile =
  "Geometric Constraint Propagation: " ++
  "Fast-path geometric reasoner using constraint propagation (milliseconds), not polynomial algebra"

explainSolverChoice UseWu profile =
  "Selected Wu's Method: " ++
  (case problemType profile of
     Geometric -> "Problem has geometric structure (distances, perpendicularity, etc.)"
     PureAlgebraic | hasSymbolicParams profile ->
       "Algebraic problem with symbolic parameters and geometric variables"
     _ -> "Wu's method is optimal for this problem structure")

explainSolverChoice UseGroebner profile =
  "Selected Gröbner Basis: " ++
  (case problemType profile of
     PureAlgebraic -> "General algebraic equation solving"
     Mixed -> "Mixed equations - using Gröbner for equation part"
     _ -> "General-purpose method for this problem type")

explainSolverChoice UseCAD profile =
  "Selected CAD: " ++
  (case problemType profile of
     PureInequality -> "Inequality reasoning in " ++ show (numVariables profile) ++ " variable(s)"
     SinglePositivity -> "Positivity check for polynomial"
     _ -> "Real algebraic geometry with inequalities")

explainSolverChoice Unsolvable profile =
  case estimatedComplexity profile of
    VeryHigh -> "Problem too complex (VeryHigh complexity with " ++
                show (numVariables profile) ++ " variables, degree " ++
                show (maxDegree profile) ++ ")"
    Infeasible -> "Problem is infeasible for automatic solving"
    _ -> "Problem type not supported by available solvers"

-- | Format the result of automatic solving for display
formatAutoSolveResult :: AutoSolveResult -> Bool -> String
formatAutoSolveResult result verbose =
  unlines $
    [ "=== AUTOMATIC SOLVER ==="
    , ""
    , "Problem Analysis:"
    , "  Type: " ++ show (problemType (problemProfile result))
    , "  Variables: " ++ show (numVariables (problemProfile result))
    , "  Constraints: " ++ show (numConstraints (problemProfile result))
    , "  Max Degree: " ++ show (maxDegree (problemProfile result))
    , "  Complexity: " ++ show (estimatedComplexity (problemProfile result))
    ] ++
    (if hasSymbolicParams (problemProfile result)
     then ["  Symbolic Parameters: " ++ unwords (symbolicParams (problemProfile result))]
     else []) ++
    [ ""
    , "Solver Selection:"
    , "  " ++ solverReason result
    , ""
    , "Result: " ++ (if isProved result then "PROVED" else "NOT PROVED")
    , "  " ++ proofReason result
    ] ++
    (if verbose
     then case detailedTrace result of
            Just trace -> ["", "Detailed Trace:", trace]
            Nothing -> []
     else [])
