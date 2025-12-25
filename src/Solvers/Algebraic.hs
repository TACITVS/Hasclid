{-|
Module: Solvers.Algebraic
Description: Algebraic solvers for the Unified Core Architecture

This module provides implementations of:
- Sum-of-Squares (SOS) solver for polynomial positivity
- Cylindrical Algebraic Decomposition (CAD) solver for inequalities
- Geometric solver using symbolic propagation
-}
module Solvers.Algebraic
  ( -- * SOS Solver
    SOSSolver(..)
    -- * CAD Solver
  , CADSolver(..)
    -- * Geometric Solver
  , GeoSolver(..)
  ) where

import Core.Solver
import Core.Problem
import Core.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (partition, isPrefixOf, isSuffixOf, intercalate)
import Data.Maybe (isJust, listToMaybe, mapMaybe)

import qualified CADLift
import Positivity.SOS (SOSCertificate(..), getSOSCertificate)
import qualified Positivity.SDP as SDP
import qualified Positivity.SymmetricSOS as SymSOS
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import BuchbergerOpt (SelectionStrategy(..))
import TermOrder (TermOrder(..), compareMonomials)
import F4Lite (f4LiteGroebner, reduceWithBasis)
import qualified Geometry.WLOG
import qualified Geometry.Barycentric
import GeoSolver (solveGeoWithTrace, GeoResult(..))
import Expr
import ProofMode (ProofMode(..), defaultProofMode)

-- =============================================
-- SOS Solver
-- =============================================

-- | Sum-of-Squares solver for proving polynomial non-negativity.
-- Uses a cascade of methods: Ono's inequality, SOS decomposition, then SDP.
data SOSSolver = SOSSolver
  { sosPoints :: [String]     -- ^ Known geometric points
  , sosProofMode :: ProofMode -- ^ Sound or Unsafe mode
  }

instance Solver SOSSolver where
  name _ = "Sum-of-Squares (SOS)"
  solve (SOSSolver pts mode) prob = do
    let theory = assumptions prob
        theGoal = goal prob
        (proved, reason, trace) = proveInequalitySOS pts mode theory theGoal
    return $ ProofResult
      { resultStatus = if proved then Proved else Failed reason
      , resultTrace = maybe [] (:[]) trace
      , resultTime = 0.0
      }

-- | Main SOS proving logic
proveInequalitySOS :: [String] -> ProofMode -> [Formula] -> Formula -> (Bool, String, Maybe String)
proveInequalitySOS pts mode theoryRaw goalRaw =
  -- First, try direct Ono inequality check (bypasses Gröbner for this pattern)
  case SymSOS.checkOnoInequality theoryRaw goalRaw of
    SymSOS.OnoProved proof -> (True, "Proved via Ono's inequality (AM-GM)", Just proof)
    _ ->
      -- Second, try generic Barrow/polynomial prover
      case SymSOS.checkBarrowInequality mode theoryRaw goalRaw of
        SymSOS.BarrowProved proof -> (True, "Proved via generic polynomial methods", Just proof)
        _ ->
          -- Standard SOS path
          let (thPrep, goalPrep, _) = eliminateRational theoryRaw goalRaw
              (thPoly, goalPoly, _) = eliminateSqrt thPrep goalPrep
          in case goalPoly of
            Ge _ _ -> trySOS pts mode thPoly goalPoly
            Gt _ _ -> trySOS pts mode thPoly goalPoly
            _ -> (False, "Not an inequality after preprocessing", Nothing)

-- | Try SOS decomposition on prepared polynomials
trySOS :: [String] -> ProofMode -> [Formula] -> Formula -> (Bool, String, Maybe String)
trySOS pts mode theory goalFull =
  let -- Apply WLOG and barycentric transformations
      (thWLOG, wlogLog) = Geometry.WLOG.applyWLOG theory goalFull
      (thBary, goalBary, baryLog) = Geometry.Barycentric.applyBarycentric pts thWLOG goalFull
      fullLog = wlogLog ++ baryLog

      -- Extract all variables and create term ordering
      allVarsSet = extractPolyVarsList (formulaToPolys goalBary ++ concatMap formulaToPolys thBary)
      isCoord v = any (`isPrefixOf` v) ["x", "y", "z", "cx", "ux", "uy"]
      isSide v  = (v `elem` ["a", "b", "c", "a2", "b2", "c2", "s", "p", "q", "r"]) ||
                  (any (`isSuffixOf` v) ["2", "s"] && not (isCoord v))
      groupCoords = S.filter isCoord allVarsSet
      groupSides  = S.filter (\v -> isSide v && not (S.member v groupCoords)) allVarsSet
      groupRest   = S.difference allVarsSet (S.union groupCoords groupSides)
      ordType = Block [ (S.toList groupCoords, Lex)
                      , (S.toList groupSides,  Lex)
                      , (S.toList groupRest,   GrevLex)
                      ]
      ord = compareMonomials ordType

      -- Identify positive variables
      posVars = [ v | v <- S.toList allVarsSet
                , "zz_sqrt_aux" `isPrefixOf` v || "ba_" `isPrefixOf` v || isAlwaysNonNeg v thBary ]

      -- Build Gröbner basis for reduction
      subMapTransformed = buildSubMap thBary
      targetPoly = toPolySub subMapTransformed (case goalBary of
                     Ge l r -> Sub l r
                     Gt l r -> Sub l r
                     _ -> Const 0)

      isDefinition (Eq (Var _) _) = True
      isDefinition _ = False

      eqConstraints = [ toPolySub subMapTransformed (Sub l r)
                      | eq@(Eq l r) <- thBary
                      , not (isDefinition eq)
                      ]
      eqConstraintsSOS = filter (not . isSumConstraint) eqConstraints

      basis = f4LiteGroebner ord SugarStrategy True eqConstraintsSOS
      reducer = reduceWithBasis ord basis

      -- Build known lemmas for SOS
      knownLemmas = mapMaybe (listToMaybe . formulaToPolys)
        [Ge (Var v) (Const 0) | v <- posVars]

      -- Try SOS certificate
      maybeCert = getSOSCertificate knownLemmas reducer targetPoly

      -- Try SDP if no certificate and in Unsafe mode
      sdpVerified =
        if isJust maybeCert
        then False
        else case mode of
               Unsafe -> SDP.checkSOS_SDP (reducer targetPoly)
               Sound -> False

      success = isJust maybeCert || sdpVerified

      -- Build trace
      intro = "\nALGEBRAIC PROOF DEVELOPMENT:\n" ++ replicate 40 '-' ++ "\n"
      setup = if null fullLog then "" else "[1] Geometric Setup:\n" ++ unlines (map ("    * " ++) fullLog) ++ "\n"
      step1 = "[2] Polynomial Transformation:\n    " ++ prettyFormula goalFull ++ " >= 0\n"
      step2 = "[3] Canonical Reduction:\n    " ++ prettyPolyNice (reducer targetPoly) ++ " >= 0\n"
      step3 = "[4] SOS Decomposition:\n    " ++ (case maybeCert of
                Just c -> formatSOSCertificate c
                Nothing -> if sdpVerified then "SDP-Based Feasibility Verified" else "No certificate found") ++ " >= 0\n"
      fullTrace = intro ++ setup ++ step1 ++ step2 ++ step3

  in (success, if success then "Proved via Sum-of-Squares" else "SOS check failed", Just fullTrace)

-- =============================================
-- CAD Solver
-- =============================================

-- | Cylindrical Algebraic Decomposition solver.
-- Preconditions with Gröbner basis for better performance.
data CADSolver = CADSolver

instance Solver CADSolver where
  name _ = "Hybrid Groebner-CAD"
  solve _ prob = do
    let (proved, reason, trace) = runCadRational (assumptions prob) (goal prob)
    return $ ProofResult
      { resultStatus = if proved then Proved else Failed reason
      , resultTrace = maybe [] (:[]) trace
      , resultTime = 0.0
      }

-- | Run CAD with Gröbner basis preconditioning
runCadRational :: [Formula] -> Formula -> (Bool, String, Maybe String)
runCadRational theory goal =
  let (th', goal', _) = eliminateSqrt theory goal
      (eqs, ineqs) = partition isEquality th'

      eqPolys = concatMap formulaToPolys eqs

      -- Create block term ordering
      allVarsSet = extractPolyVarsList (eqPolys ++ concatMap formulaToPolys ineqs ++ formulaToPolys goal')
      isCoord v = any (`isPrefixOf` v) ["x", "y", "z", "cx", "ux", "uy"]
      isSide v  = (v `elem` ["a", "b", "c", "a2", "b2", "c2", "s", "p", "q", "r"]) ||
                  (any (`isSuffixOf` v) ["2", "s"] && not (isCoord v))
      groupCoords = S.filter isCoord allVarsSet
      groupSides  = S.filter (\v -> isSide v && not (S.member v groupCoords)) allVarsSet
      groupRest   = S.difference allVarsSet (S.union groupCoords groupSides)
      ord = compareMonomials (Block [ (S.toList groupCoords, Lex)
                                    , (S.toList groupSides,  Lex)
                                    , (S.toList groupRest,   GrevLex)
                                    ])

      -- Compute Gröbner basis and reducer
      basis = f4LiteGroebner ord SugarStrategy True eqPolys
      reducer = reduceWithBasis ord basis

      -- Reduce goal
      reducedGoal = case goal' of
        Ge l r -> Ge (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        Gt l r -> Gt (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        _      -> goal'

      -- Reduce inequalities
      reduceIneq :: Formula -> Formula
      reduceIneq f = case f of
        Ge l r -> Ge (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        Gt l r -> Gt (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        _      -> f

      reducedIneqs = map reduceIneq ineqs

      -- Build trace
      vars = S.toList (extractPolyVarsList (formulaToPolys reducedGoal ++ concatMap formulaToPolys reducedIneqs))
      traceMsg = unlines
        [ "Hybrid Groebner-CAD Strategy:"
        , "  1. Partitioned theory: " ++ show (length eqs) ++ " equalities, " ++ show (length ineqs) ++ " inequalities."
        , "  2. Computed Hierarchical Elimination Basis (Block Order)."
        , "  3. Reduced System Variables: " ++ show vars
        , "  4. Invoking CAD on reduced system..."
        ]

      proved = CADLift.proveFormulaCAD reducedIneqs reducedGoal

  in (proved, "Hybrid Groebner-CAD", Just traceMsg)

-- =============================================
-- Geometric Solver
-- =============================================

-- | Fast geometric solver using symbolic constraint propagation.
data GeoSolver = GeoSolver

instance Solver GeoSolver where
  name _ = "Geometric Solver"
  solve _ prob = do
    case solveGeoWithTrace (assumptions prob) (goal prob) of
      GeoProved r s -> return $ ProofResult Proved (r:s) 0.0
      GeoDisproved r s -> return $ ProofResult (Disproved r) (r:s) 0.0
      GeoUnknown r -> return $ ProofResult (Failed r) [] 0.0

-- =============================================
-- Helper Functions
-- =============================================

formulaToPolys :: Formula -> [Poly]
formulaToPolys formula =
  case formula of
    Eq lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Ge lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Gt lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Le lhs rhs -> [toPolySub M.empty (Sub rhs lhs)]
    Lt lhs rhs -> [toPolySub M.empty (Sub rhs lhs)]
    And f1 f2 -> formulaToPolys f1 ++ formulaToPolys f2
    Or f1 f2 -> formulaToPolys f1 ++ formulaToPolys f2
    Not f -> formulaToPolys f
    Forall _ f -> formulaToPolys f
    Exists _ f -> formulaToPolys f
    _ -> []

extractPolyVarsList :: [Poly] -> S.Set String
extractPolyVarsList = foldr (S.union . extractPolyVars) S.empty

extractPolyVars :: Poly -> S.Set String
extractPolyVars (Poly m) = S.fromList $ concatMap extractMonoVars (M.toList m)
  where extractMonoVars (Monomial vars, _) = M.keys vars

isEquality :: Formula -> Bool
isEquality (Eq _ _) = True
isEquality (Forall _ f) = isEquality f
isEquality (Exists _ f) = isEquality f
isEquality _ = False

isAlwaysNonNeg :: String -> [Formula] -> Bool
isAlwaysNonNeg v theory = any matchesNonNeg theory
  where
    matchesNonNeg (Ge (Var x) _) = x == v
    matchesNonNeg (Gt (Var x) _) = x == v
    matchesNonNeg _ = False

isSumConstraint :: Poly -> Bool
isSumConstraint p =
  let vars = extractPolyVars p
  in S.member "ba_u" vars && S.member "ba_v" vars && S.member "ba_w" vars &&
     polyDegreeIn p "ba_u" == 1

polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  if M.null m then 0
  else maximum (0 : [ fromIntegral (M.findWithDefault 0 var vars)
                    | (Monomial vars, _) <- M.toList m ])

formatSOSCertificate :: SOSCertificate -> String
formatSOSCertificate cert =
  let terms = sosTerms cert
      lemmas = sosLemmas cert
      formatTerm (c, p) = (if c == 1 then "" else prettyRational c ++ "*") ++ "(" ++ prettyPolyNice p ++ ")^2"
      sumParts = map formatTerm (reverse terms) ++ map prettyPolyNice lemmas
  in if null sumParts
     then "0"
     else intercalate " + " sumParts ++
          (if sosRemainder cert == polyZero then "" else " + [" ++ prettyPolyNice (sosRemainder cert) ++ "]")
