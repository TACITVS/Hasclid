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
  , executeSolver

    -- * Solver Selection Logic
  , SolverChoice(..)
  , selectAlgebraicSolver
  , explainSolverChoice

    -- * Result Types
  , AutoSolveResult(..)
  , formatAutoSolveResult
  , SolverOptions(..)
  , defaultSolverOptions
  , GroebnerBackend(..)
  , intSolve
  , intSat
  , proveExistentialConstructive
  ) where

import Expr
import ProblemAnalyzer
import GeoSolver (solveGeoWithTrace, GeoResult(..))
import Wu (wuProve, wuProveWithTrace, formatWuTrace, proveExistentialWu)
import Prover (proveTheoryWithOptions, formatProofTrace, buchberger, subPoly, intSolve, intSat, IntSolveOptions(..), IntSolveOutcome(..), defaultIntSolveOptions, reasonOutcome, proveExistentialConstructive)
import CADLift (evaluateInequalityCAD, solveQuantifiedFormulaCAD)
import CADLift (proveFormulaCAD)
import Positivity (checkPositivityEnhanced, isPositive, explanation, PositivityResult(..), Confidence(..))
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import BuchbergerOpt (buchbergerWithStrategy, SelectionStrategy(..))
import TermOrder (TermOrder(..), compareMonomials)
import F4Lite (f4LiteGroebner, reduceWithF4)
import Geometry.WLOG (applyWLOG)
import Positivity.SOS (checkSOS)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (delete)
import Data.Maybe (isJust)

-- =============================================
-- Data Types
-- =============================================

-- | Available solving methods
data SolverChoice
  = UseGeoSolver    -- Geometric constraint propagation (FASTEST - Phase 1)
  | UseWu           -- Wu's method (fast for geometry)
  | UseGroebner     -- Gröbner basis (general purpose)
  | UseConstructiveWu -- Constructive existence using Wu's Method (Triangularization)
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

data SolverOptions = SolverOptions
  { intOptions :: IntSolveOptions
  , useOptimizedGroebner :: Bool
  , selectionStrategyOpt :: SelectionStrategy
  , groebnerBackend :: GroebnerBackend
  , f4UseBatch :: Bool
  } deriving (Show, Eq)

data GroebnerBackend = BuchbergerBackend | F4Backend deriving (Show, Eq)

defaultSolverOptions :: SolverOptions
defaultSolverOptions = SolverOptions
  { intOptions = defaultIntSolveOptions
  , useOptimizedGroebner = True
  , selectionStrategyOpt = SugarStrategy
  , groebnerBackend = BuchbergerBackend
  , f4UseBatch = True
  }

-- | Optimize solver options based on problem profile
--   Switch to F4 for complex problems where batch reduction pays off.
optimizeGroebnerOptions :: ProblemProfile -> SolverOptions -> SolverOptions
optimizeGroebnerOptions profile opts =
  -- Heuristic for F4:
  -- 1. High number of constraints (>= 5): Batch reduction is effective.
  -- 2. Geometric problems with many points (>= 5): Likely generates many polynomials.
  -- 3. High degree (>= 4): Reductions are expensive, matrix ops might vectorize better (conceptually).
  let useF4 = numConstraints profile >= 5 ||
              (problemType profile == Geometric && numPoints (geometricFeatures profile) >= 5) ||
              maxDegree profile >= 4
  in if useF4
     then opts { groebnerBackend = F4Backend }
     else opts

-- | Pick the Gröbner routine based on solver options
selectGroebner :: SolverOptions -> ([Poly] -> [Poly])
selectGroebner opts =
  let ord = compareMonomials GrevLex  -- Use GrevLex for best performance
  in case groebnerBackend opts of
       F4Backend ->
         f4LiteGroebner ord (selectionStrategyOpt opts) (f4UseBatch opts)
       BuchbergerBackend ->
         if useOptimizedGroebner opts
           then buchbergerWithStrategy ord (selectionStrategyOpt opts)
           else buchberger

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
autoSolve :: SolverOptions -> Theory -> Formula -> AutoSolveResult
autoSolve opts theory goal =
  let
    -- Analyze the problem structure
    profile = analyzeProblem theory goal
    groebner = selectGroebner opts
  in
    case goal of
      Exists qs inner
        | all (\q -> qvType q == QuantInt) qs
        , not (any containsQuantifier theory) ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                outcome = intSat (intOptions opts) (theoryInt ++ [innerInt])
                proved = intResult outcome == Just True
                reason = case intResult outcome of
                           Just True  -> "Integer existential proved satisfiable. " ++ reasonOutcome outcome True
                           Just False -> "Integer existential is unsatisfiable. " ++ reasonOutcome outcome False
                           Nothing    -> "Integer existential parsed but solver is incomplete for this goal."
            in AutoSolveResult
                 { selectedSolver = UseGroebner
                 , solverReason = "Integer existential handled by integer solver"
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = reason
                 , detailedTrace = Nothing
                 }
      Forall qs _
        | all (\q -> qvType q == QuantInt) qs
        , not (any containsQuantifier theory) ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                goalInt = promoteIntVars intNames goal
                (proved, reason, _, _) = proveTheoryWithOptions groebner Nothing theoryInt goalInt
            in AutoSolveResult
                 { selectedSolver = UseGroebner
                 , solverReason = "Integer universal handled by integer solver (negation refutation)"
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = reason
                 , detailedTrace = Nothing
                 }
      Exists qs _
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) ->
            let (proved, reason, _, _) = proveTheoryWithOptions groebner Nothing theory goal
            in AutoSolveResult
                 { selectedSolver = UseCAD
                 , solverReason = "Real existential handled by CAD satisfiability"
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = reason
                 , detailedTrace = Nothing
                 }
      Forall qs _
        | all (\q -> qvType q == QuantReal) qs
        , all (\q -> isJust (qvLower q) && isJust (qvUpper q)) qs
        , not (any containsQuantifier theory) ->
            let (proved, reason, _, _) = proveTheoryWithOptions groebner Nothing theory goal
            in AutoSolveResult
                 { selectedSolver = UseGroebner
                 , solverReason = "Bounded real universal handled by linear interval check"
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = reason
                 , detailedTrace = Nothing
                 }
      -- Unbounded or partially bounded real universals via CAD refutation
      Forall qs _
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) ->
            let (proved, reason, _, _) = proveTheoryWithOptions groebner Nothing theory goal
            in AutoSolveResult
                 { selectedSolver = UseCAD
                 , solverReason = "Real universal handled by CAD refutation"
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = reason
                 , detailedTrace = Nothing
                 }
      
      -- Geometric Inequality Fast Path (WLOG + F4 + SOS)
      Ge _ _ | problemType profile == Geometric ->
        let (proved, reason, trace) = proveGeometricInequality opts theory goal
        in if proved
           then AutoSolveResult
                  { selectedSolver = UseGroebner
                  , solverReason = "Geometric Inequality (Fast Path: WLOG+F4+SOS)"
                  , problemProfile = profile
                  , isProved = True
                  , proofReason = reason
                  , detailedTrace = trace
                  }
           else
             let (proved', reason', trace') = executeSolver UseCAD opts profile theory goal
             in AutoSolveResult
                  { selectedSolver = UseCAD
                  , solverReason = "Geometric Inequality (Fallback to CAD after SOS failure)"
                  , problemProfile = profile
                  , isProved = proved'
                  , proofReason = reason'
                  , detailedTrace = trace'
                  }

      -- Pure inequality / positivity goals: route directly to CAD/positivity (sound), never heuristics
      Ge _ _ | problemType profile == PureInequality || problemType profile == SinglePositivity ->
        let (proved, reason, trace) = executeSolver UseCAD opts profile theory goal
        in AutoSolveResult
             { selectedSolver = UseCAD
             , solverReason = "Inequality/positivity goal routed to CAD (sound positivity path)"
             , problemProfile = profile
             , isProved = proved
             , proofReason = reason
             , detailedTrace = trace
             }
      Gt _ _ | problemType profile == PureInequality || problemType profile == SinglePositivity ->
        let (proved, reason, trace) = executeSolver UseCAD opts profile theory goal
        in AutoSolveResult
             { selectedSolver = UseCAD
             , solverReason = "Strict inequality goal routed to CAD (sound positivity path)"
             , problemProfile = profile
             , isProved = proved
             , proofReason = reason
             , detailedTrace = trace
             }
      _ | containsQuantifier goal || any containsQuantifier theory ->
            let (proved, reason, trace) = executeSolver UseCAD opts profile theory goal
            in AutoSolveResult
              { selectedSolver = UseCAD
              , solverReason = "Quantified formula routed to CAD (Full QE support)"
              , problemProfile = profile
              , isProved = proved
              , proofReason = reason
              , detailedTrace = trace
              }
      _ ->
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

          GeoUnknown _ ->
            -- GeoSolver insufficient, fall back to PHASE 2 (algebraic solvers)
            let
              solver = selectAlgebraicSolver profile goal
              solverReason' = "Geometric propagation insufficient. Fallback to PHASE 2: " ++ explainSolverChoice solver profile
              (proved, proofMsg, trace) = executeSolver solver opts profile theory goal
            in AutoSolveResult
                 { selectedSolver = solver
                 , solverReason = solverReason'
                 , problemProfile = profile
                 , isProved = proved
                 , proofReason = proofMsg
                 , detailedTrace = trace
                 }

-- | Attempt to prove geometric inequality using WLOG + F4 + SOS
proveGeometricInequality :: SolverOptions -> Theory -> Formula -> (Bool, String, Maybe String)
proveGeometricInequality opts theory goal =
  case goal of
    Ge lhs rhs -> trySOS (Sub lhs rhs)
    Gt lhs rhs -> trySOS (Sub lhs rhs) -- TODO: Strictness check
    _ -> (False, "Not an inequality", Nothing)
  where
    trySOS targetExpr =
      let
          -- 1. Apply WLOG
          (thWLOG, wlogLog) = applyWLOG theory goal
          
          -- 2. Substitution (Pre-processing)
          -- Identify definitions: v = expr
          subMap = buildSubMap thWLOG
          
          isDefinition (Eq (Var _) _) = True
          isDefinition _ = False
          
          -- Apply substitution to remaining constraints
          -- We filter out definitions because they are inlined
          eqConstraints = 
            [ toPolySub subMap (Sub l r) 
            | eq@(Eq l r) <- thWLOG 
            , not (isDefinition eq)
            ]
          
          -- Apply to target
          targetPoly = toPolySub subMap targetExpr
          
          -- 3. F4 Reduction
          -- Use GrevLex for reduction efficiency
          ord = compareMonomials GrevLex
          
          -- We reduce the target polynomial modulo the geometric constraints
          reduced = reduceWithF4 ord eqConstraints targetPoly
          
          -- 4. Check SOS
          isSOS = checkSOS reduced
      in
        if isSOS
        then (True, "Proved via WLOG + Substitution + F4 + SOS", Just (unlines wlogLog ++ "\nReduced Poly (SOS): " ++ show reduced))
        else (False, "SOS check failed after F4 reduction", Just (unlines wlogLog ++ "\nReduced Poly (Not SOS): " ++ show reduced))

-- Promote bound variable names to IntVar inside a formula (and expressions)
promoteIntVars :: [String] -> Formula -> Formula
promoteIntVars names f = goF names f
  where
    goF ns (Eq l r) = Eq (goE ns l) (goE ns r)
    goF ns (Ge l r) = Ge (goE ns l) (goE ns r)
    goF ns (Gt l r) = Gt (goE ns l) (goE ns r)
    goF ns (Le l r) = Le (goE ns l) (goE ns r)
    goF ns (Lt l r) = Lt (goE ns l) (goE ns r)
    goF ns (And a b) = And (goF ns a) (goF ns b)
    goF ns (Or a b) = Or (goF ns a) (goF ns b)
    goF ns (Not x) = Not (goF ns x)
    goF ns (Forall qs f') =
      let ns' = foldr (delete . qvName) ns qs
      in Forall qs (goF ns' f')
    goF ns (Exists qs f') =
      let ns' = foldr (delete . qvName) ns qs
      in Exists qs (goF ns' f')

    goE ns (Var v) | v `elem` ns = IntVar v
    goE _  e@(IntVar _) = e
    goE _  e@(IntConst _) = e
    goE _  e@(Const _) = e
    goE ns (Add a b) = Add (goE ns a) (goE ns b)
    goE ns (Sub a b) = Sub (goE ns a) (goE ns b)
    goE ns (Mul a b) = Mul (goE ns a) (goE ns b)
    goE ns (Div a b) = Div (goE ns a) (goE ns b)
    goE ns (Pow e n) = Pow (goE ns e) n
    goE ns (Sqrt e) = Sqrt (goE ns e)
    goE ns (Determinant rows) = Determinant (map (map (goE ns)) rows)
    goE ns (Circle p c r) = Circle p c (goE ns r)
    goE _  other = other

-- | Automatic solve with verbose trace information
autoSolveWithTrace :: SolverOptions -> Theory -> Formula -> Bool -> AutoSolveResult
autoSolveWithTrace opts theory goal _ =
  autoSolve opts theory goal

-- =============================================
-- Solver Selection Logic
-- =============================================

-- | Select the most appropriate ALGEBRAIC solver (PHASE 2)
-- This is called only if GeoSolver returns GeoUnknown in PHASE 1
selectAlgebraicSolver :: ProblemProfile -> Formula -> SolverChoice
selectAlgebraicSolver profile goal
  -- If sqrt appears, force CAD so we can use polynomialization + CAD
  | containsSqrtFormula goal = UseCAD
  -- If integers appear, try integer evaluator first (handled in executeSolver); keep router permissive
  | containsIntFormula goal = UseGroebner
  
  -- RULE 0: Existential Equalities -> Constructive Wu (Triangularization)
  | isExistentialEquality goal = UseConstructiveWu

  -- RULE 1: Unsupported formula types
  | not (isEquality goal) && not (isInequality goal) = Unsolvable

  -- RULE 2: Too complex (avoid hanging) - Relaxed for Algebraic Equality
  | estimatedComplexity profile >= VeryHigh && not (isEquality goal) = Unsolvable

  -- RULE 3: Pure geometric problems → prefer Groebner for small/simple equalities
  -- Heuristic: small variable/constraint counts benefit from direct algebraic proof
  | problemType profile == Geometric
  , isEquality goal
  , numVariables profile <= 30
  , numConstraints profile <= 30 = UseGroebner
  -- Geometric inequalities: route to CAD (avoid Wu)
  | problemType profile == Geometric
  , isInequality goal = UseCAD
  | problemType profile == Geometric = UseWu

  -- RULE 4: Inequalities with 1-2 variables → CAD
  | isInequality goal && numVariables profile <= 2 && not (hasSymbolicParams profile) = UseCAD

  -- RULE 5: Pure algebraic with symbolic parameters → Wu's method
  -- (Wu handles symbolic parameters better than Gröbner for geometry)
  | problemType profile == PureAlgebraic && hasSymbolicParams profile &&
    hasGeometricVars profile = UseWu

  -- RULE 6: Single positivity check (p > 0) → CAD (no variable limit!)
  | problemType profile == SinglePositivity = UseCAD

  -- RULE 7: Pure algebraic equations → Gröbner basis (general purpose)
  | problemType profile == PureAlgebraic = UseGroebner

  -- RULE 8: Mixed equations and inequalities → Use Gröbner for equations part
  | problemType profile == Mixed && isEquality goal = UseGroebner

  -- DEFAULT: Gröbner basis (most reliable general-purpose method)
  | otherwise = UseGroebner

-- | Check if formula is an existential quantification of equalities
isExistentialEquality :: Formula -> Bool
isExistentialEquality (Exists _ body) = isEqualityBody body
isExistentialEquality _ = False

isEqualityBody :: Formula -> Bool
isEqualityBody (Eq _ _) = True
isEqualityBody (And f1 f2) = isEqualityBody f1 && isEqualityBody f2
isEqualityBody (Or f1 f2) = isEqualityBody f1 || isEqualityBody f2 -- Relaxed for disjunction of equalities
isEqualityBody _ = False

-- | Check if formula is an equality
isEquality :: Formula -> Bool
isEquality (Eq _ _) = True
isEquality (Forall _ f) = isEquality f
isEquality (Exists _ f) = isEquality f
isEquality _ = False

-- | Check if formula is an inequality
isInequality :: Formula -> Bool
isInequality (Ge _ _) = True
isInequality (Gt _ _) = True
isInequality (Forall _ f) = isInequality f
isInequality (Exists _ f) = isInequality f
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
-- Numeric angle fast-path
-- =============================================

maybeEvalNumericAngleEquality :: Theory -> Expr -> Expr -> Maybe (Bool, String)
maybeEvalNumericAngleEquality theory l r =
  case (l, r) of
    (AngleEq2D a b c d e f, Const _) -> evalAngle False a b c d e f
    (AngleEq2DAbs a b c d e f, Const _) -> evalAngle True a b c d e f
    _ -> Nothing
  where
    lookupCoord p axis = M.lookup (axis ++ p) coordMap
    coordMap = M.fromList [ (v, c) | Eq (Var v) (Const c) <- theory ]

    evalAngle allowAbs a b c d e f = do
      ax <- lookupCoord a "x"; ay <- lookupCoord a "y"
      bx <- lookupCoord b "x"; by <- lookupCoord b "y"
      cx <- lookupCoord c "x"; cy <- lookupCoord c "y"
      dx <- lookupCoord d "x"; dy <- lookupCoord d "y"
      ex <- lookupCoord e "x"; ey <- lookupCoord e "y"
      fx <- lookupCoord f "x"; fy <- lookupCoord f "y"

      let vx (x1,y1) (x0,y0) = (x1 - x0, y1 - y0)
          dot (x1,y1) (x2,y2) = x1*x2 + y1*y2
          cross (x1,y1) (x2,y2) = x1*y2 - y1*x2
          norm2 (x1,y1) = x1*x1 + y1*y1

          u1 = vx (bx,by) (ax,ay); v1 = vx (cx,cy) (bx,by)
          u2 = vx (ex,ey) (dx,dy); v2 = vx (fx,fy) (ex,ey)

          dot1 = dot u1 v1; dot2 = dot u2 v2
          l1   = norm2 u1 * norm2 v1
          l2   = norm2 u2 * norm2 v2
          cross1 = cross u1 v1
          cross2 = cross u2 v2

          cosDiff = dot1 * l2 - dot2 * l1
      if l1 == 0 || l2 == 0
        then Just (False, "Degenerate angle (zero length vector)")
        else if allowAbs
          then
            let sinAbsDiff = cross1*cross1 * l2*l2 - cross2*cross2 * l1*l1
            in if cosDiff == 0 && sinAbsDiff == 0
               then Just (True, "Angle equality holds numerically (abs) after substitution")
               else Just (False, "Angle equality fails numerically (abs) after substitution")
          else
            let sinDiff = cross1 * l2 - cross2 * l1
            in if cosDiff == 0 && sinDiff == 0
               then Just (True, "Angle equality holds numerically after substitution")
               else Just (False, "Angle equality fails numerically after substitution")

-- CAD-based sqrt elimination path
-- =============================================
-- Solver Execution
-- =============================================

-- | Execute the selected solver
-- Returns: (is_proved, reason, optional_trace)
executeSolver :: SolverChoice -> SolverOptions -> ProblemProfile -> Theory -> Formula -> (Bool, String, Maybe String)
executeSolver solver opts profile theory goal =
  let hasSqrt = containsSqrtFormula goal || any containsSqrtFormula theory
      hasInt = containsIntFormula goal || any containsIntFormula theory
      hasDiv = containsDivFormula goal || any containsDivFormula theory
      
      -- Dynamic backend optimization
      opts' = optimizeGroebnerOptions profile opts
      groebner = selectGroebner opts'
  in case goal of
       -- Fast-path: squares are non-negative
       Ge (Pow _ 2) (Const c) | c <= 0 -> (True, "Non-negativity of a square (>= 0)", Nothing)
       Gt (Pow _ 2) (Const c) | c < 0  -> (True, "Strictly positive since square > negative constant", Nothing)
       _ ->
         case solver of
           UseWu ->
             if hasInt then runInt opts theory goal
             else if hasDiv then runCadRational theory goal
             else if hasSqrt then runCadSqrt theory goal
             else case goal of
                    Eq _ _ ->
                      let (proved, reason) = wuProve theory goal
                          trace = wuProveWithTrace theory goal
                      in (proved, reason, Just (formatWuTrace trace))
                    _ -> (False, "Wu's method only supports equality goals", Nothing)

           UseGroebner ->
             if hasInt then runInt opts theory goal
             else if hasDiv then runCadRational theory goal
             else if hasSqrt then runCadSqrt theory goal
             else case goal of
                    Eq l r ->
                      case maybeEvalNumericAngleEquality theory l r of
                        Just (res, msg) -> (res, msg, Nothing)
                        Nothing ->
                          let subM = buildSubMap theory
                              diffPoly = subPoly (toPolySub subM l) (toPolySub subM r)
                              isConstPoly (Poly m) = all (\(Monomial vars, _) -> M.null vars) (M.toList m)
                              constValue (Poly m) = sum (M.elems m)
                          in if isConstPoly diffPoly
                             then let c = constValue diffPoly
                                  in if c == 0
                                     then (True, "Equality holds after numeric substitution (constant 0)", Nothing)
                                     else (False, "Constants not equal after substitution (" ++ show c ++ ")", Nothing)
                             else
                               let (proved, reason, trace, _) = proveTheoryWithOptions groebner Nothing theory goal
                               in (proved, reason, Just (formatProofTrace trace))
                    _ -> runCadRational theory goal

           UseConstructiveWu ->
             let (proved, reason, trace) = proveExistentialWu theory goal
             in (proved, reason, Just (formatWuTrace trace))

           UseCAD ->
             if hasInt then runInt opts theory goal
             else if hasDiv then runCadRational theory goal
             else if hasSqrt then runCadSqrt theory goal
             else if containsQuantifier goal || any containsQuantifier theory
                  then
                    let (theory', goal') = preprocessForCAD theory goal
                        proved = solveQuantifiedFormulaCAD theory' goal'
                        msgBase = if proved then "Proved by CAD (Quantifier Elimination)" else "Refuted by CAD (Quantifier Elimination)"
                        msg = msgBase ++ " after rational/sqrt elimination"
                    in (proved, msg, Nothing)
                  else
                    case goal of
                      Ge l r -> executeCADInequality theory l r False
                      Gt l r -> executeCADInequality theory l r True
                      Le l r -> executeCADInequality theory r l False  -- Flip: l <= r becomes r >= l
                      Lt l r -> executeCADInequality theory r l True   -- Flip: l < r becomes r > l
                      _ -> (False, "CAD only supports inequality goals (>, >=, <, <=)", Nothing)

           Unsolvable ->
             (False, "Problem is too complex or type not supported by automatic solver", Nothing)

-- | Execute CAD for an inequality
executeCADInequality :: Theory -> Expr -> Expr -> Bool -> (Bool, String, Maybe String)
executeCADInequality theory lhs rhs isStrict =
  let hasSqrt = containsSqrtExpr lhs || containsSqrtExpr rhs || any containsSqrtFormula theory
  in if hasSqrt
     then runCadSqrt theory (Ge lhs rhs)
     else
       let allowZero = not isStrict
           subM = buildSubMap theory
           diffPoly = subPoly (toPolySub subM lhs) (toPolySub subM rhs)
       in
         -- Fast path for numeric contradictions
         (case diffPoly of
            Poly m | null m -> (True, "Inequality holds (difference is constant 0)", Nothing)
            Poly m | M.size m == 1 ->
              case M.toList m of
                -- Check if it's ACTUALLY a constant (monomial with no variables)
                [(Monomial vars, c)] | M.null vars ->
                  if isStrict && c > 0
                  then (True, "Constant > 0", Nothing)
                  else if allowZero && c >= 0
                  then (True, "Constant >= 0", Nothing)
                  else (False, "Constant inequality fails", Nothing)
                -- Not a constant, fall through to general case
                _ ->
                  case toUnivariate diffPoly of
                    Just (_, coeffs) ->
                      let res = checkPositivityEnhanced diffPoly allowZero
                          zeroRoots = any (== 0) coeffs
                      in
                        if confidence res == Heuristic
                        then
                          -- Fall back to CAD for a formal answer instead of accepting heuristic sampling
                          let vars = S.toList (extractPolyVars diffPoly)
                              constraints = [ subPoly (toPolySub subM l) (toPolySub subM r) | Eq l r <- theory ]
                              holds = evaluateInequalityCAD constraints diffPoly vars
                              msg = if holds
                                    then "CAD check (" ++ show (length vars) ++ "D) succeeded (positivity heuristic rejected)"
                                    else "CAD check (" ++ show (length vars) ++ "D) found countercell (positivity heuristic rejected)"
                          in (holds, msg, Nothing)
                        else
                          let ok = if allowZero && zeroRoots then True else isPositive res
                              msg = if allowZero && zeroRoots
                                    then "Allowing zero root: polynomial is non-negative with roots at 0"
                                    else explanation res
                          in (ok, msg, Nothing)
                    Nothing ->
                      -- Extract variables from the POLYNOMIAL (after substitution), not the expression
                      let vars = S.toList (extractPolyVars diffPoly)
                          constraints = [ subPoly (toPolySub subM l) (toPolySub subM r) | Eq l r <- theory ]
                          holds = evaluateInequalityCAD constraints diffPoly vars
                          msg = if holds
                                then "CAD check (" ++ show (length vars) ++ "D) succeeded"
                                else "CAD check (" ++ show (length vars) ++ "D) found countercell"
                      in (holds, msg, Nothing)
            _ ->
              case toUnivariate diffPoly of
                Just (_, coeffs) ->
                  let res = checkPositivityEnhanced diffPoly allowZero
                      zeroRoots = any (== 0) coeffs
                  in
                    if confidence res == Heuristic
                    then
                      let vars = S.toList (extractPolyVars diffPoly)
                          constraints = [ subPoly (toPolySub subM l) (toPolySub subM r) | Eq l r <- theory ]
                          holds = evaluateInequalityCAD constraints diffPoly vars
                          msg = if holds
                                then "CAD check (" ++ show (length vars) ++ "D) succeeded (positivity heuristic rejected)"
                                else "CAD check (" ++ show (length vars) ++ "D) found countercell (positivity heuristic rejected)"
                      in (holds, msg, Nothing)
                    else
                      let ok = if allowZero && zeroRoots then True else isPositive res
                          msg = if allowZero && zeroRoots
                                then "Allowing zero root: polynomial is non-negative with roots at 0"
                                else explanation res
                      in (ok, msg, Nothing)
                Nothing ->
                  -- Extract variables from the POLYNOMIAL (after substitution), not the expression
                  let vars = S.toList (extractPolyVars diffPoly)
                  in
                     let constraints = [ subPoly (toPolySub subM l) (toPolySub subM r) | Eq l r <- theory ]
                         holds = evaluateInequalityCAD constraints diffPoly vars
                         msg = if holds
                               then "CAD check (" ++ show (length vars) ++ "D) succeeded"
                               else "CAD check (" ++ show (length vars) ++ "D) found countercell"
                     in (holds, msg, Nothing))


-- | Extract variables from a polynomial (actual free variables after substitution)
extractPolyVars :: Poly -> S.Set String
extractPolyVars (Poly m) =
  S.fromList $ concatMap (\(Monomial vars) -> M.keys vars) (M.keys m)

-- CAD-based rational elimination path
runCadRational :: Theory -> Formula -> (Bool, String, Maybe String)
runCadRational theory goal =
  let (th', goal') = eliminateRational theory goal
      hasSqrt = containsSqrtFormula goal' || any containsSqrtFormula th'
      (th'', goal'') = if hasSqrt
                         then eliminateSqrt th' goal'
                         else (th', goal')
      proved = proveFormulaCAD th'' goal''
      msg = if proved
            then "Proved via CAD with rational" ++
                 (if hasSqrt then " and sqrt" else "") ++ " elimination"
            else "Not proved via CAD with rational elimination"
  in (proved, msg, Nothing)

-- CAD-based sqrt elimination path
runCadSqrt :: Theory -> Formula -> (Bool, String, Maybe String)
runCadSqrt theory goal =
  let (th', goal') = eliminateSqrt theory goal
      proved = proveFormulaCAD th' goal'
      msg = if proved
            then "Proved via CAD with sqrt elimination"
            else "Not proved via CAD with sqrt elimination"
  in (proved, msg, Nothing)

-- Shared preprocessing for CAD/QE: eliminate rationals, then sqrts
preprocessForCAD :: Theory -> Formula -> (Theory, Formula)
preprocessForCAD th goal =
  let (thR, goalR) = eliminateRational th goal
      hasSqrt = containsSqrtFormula goalR || any containsSqrtFormula thR
  in if hasSqrt
     then eliminateSqrt thR goalR
     else (thR, goalR)

-- Integer fast-path
runInt :: SolverOptions -> Theory -> Formula -> (Bool, String, Maybe String)
runInt opts theory goal =
  let outcome = intSolve (intOptions opts) theory goal
  in case intResult outcome of
       Just True  -> (True, reasonOutcome outcome True, Nothing)
       Just False -> (False, reasonOutcome outcome False, Nothing)
       Nothing    ->
         let base = "Integer domain parsed but solver is incomplete for this goal (non-linear or insufficient data)."
             extra = if intBruteCandidate outcome
                     then " A bounded brute-force search is available; enable it with :bruteforce on."
                     else ""
         in (False, base ++ extra, Nothing)

-- =============================================
-- Explanation and Formatting
-- =============================================

-- | Explain why a particular solver was chosen
explainSolverChoice :: SolverChoice -> ProblemProfile -> String
explainSolverChoice UseGeoSolver _ =
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
explainSolverChoice UseConstructiveWu _ =
  "Constructive Wu: existential/triangularization route chosen for geometry-style goals."

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
