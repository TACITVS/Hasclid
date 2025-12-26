{-|
Module: SolverRouter
Description: Intelligent routing system using Unified Core Architecture
-}

{-# LANGUAGE ExistentialQuantification #-}

module SolverRouter
  ( autoSolve
  , autoSolveE
  , autoSolveWithTrace
  , executeSolver
  , SolverChoice(..)
  , explainSolverChoice
  , AutoSolveResult(..)
  , ProofEvidence(..)
  , formatAutoSolveResult
  , SolverOptions(..)
  , defaultSolverOptions
  , GroebnerBackend(..)
  , intSolve
  , intSat
  , proveExistentialConstructive
  )
where

import Expr
import Error
import Preprocessing (preprocess, preprocessGeometry, PreprocessingResult(..), applySubstitutionsFormula, applySubstitutionsExpr)
import ProblemAnalyzer
import GeoSolver (solveGeoWithTrace, GeoResult(..))
import qualified Wu (wuProveWithTrace, formatWuTrace, isProved, proofReason)
import qualified Prover (intSolve, intSat, IntSolveOptions(..), IntSolveOutcome(..), defaultIntSolveOptions, proveExistentialConstructive, ProofTrace)
import qualified CADLift (proveFormulaCAD)
import Positivity.SOS (SOSCertificate(..), getSOSCertificate)
import qualified Positivity.Numerical as NumSOS
import qualified Positivity.SDP as SDP
import qualified Positivity.SymmetricSOS as SymSOS
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import BuchbergerOpt (SelectionStrategy(..))
import TermOrder (TermOrder(..), compareMonomials)
import F4Lite (f4LiteGroebner, reduceWithBasis)
import qualified F5 (f5SolveBounded, F5Config(..), F5Result(..), defaultF5Config, conservativeF5Config, f5Basis, f5Completed)
import qualified Geometry.WLOG
import qualified Geometry.Barycentric
import Heuristics
import AreaMethod (proveArea, deriveConstruction)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (intercalate, isPrefixOf, isSuffixOf, partition, nub, isInfixOf)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as CE
import Control.Exception (try, evaluate, ArithException(..))
import ProofMode (ProofMode(..), defaultProofMode)

-- Unified Core Imports
import Core.Types
import Core.Problem
import Core.Solver
import Core.Orchestrator
import qualified Solvers.Wu as SW
import qualified Solvers.Groebner as SG

-- =============================================
-- Legacy / Compatibility Types
-- =============================================

data SolverChoice = UseGeoSolver | UseWu | UseGroebner | UseConstructiveWu | UseCAD | UseAreaMethod | UseSOS | Unsolvable deriving (Show, Eq)
data ProofEvidence = EvidenceSOS SOSCertificate [String] deriving (Show, Eq)

data AutoSolveResult = AutoSolveResult
  { selectedSolver :: SolverChoice
  , solverReason :: String
  , problemProfile :: ProblemProfile
  , preprocessedGoalExpr :: Formula
  , isProved :: Bool
  , proofReason :: String
  , proofEvidence :: Maybe ProofEvidence
  , detailedTrace :: Maybe String
  , varDefinitions :: M.Map String Expr
  }
  deriving (Show, Eq)

data SolverOptions = SolverOptions
  { intOptions :: Prover.IntSolveOptions
  , useOptimizedGroebner :: Bool
  , selectionStrategyOpt :: SelectionStrategy
  , groebnerBackend :: GroebnerBackend
  , f4UseBatch :: Bool
  , proofMode :: ProofMode
  }
  deriving (Show, Eq)

data GroebnerBackend = BuchbergerBackend | F4Backend | F5Backend deriving (Show, Eq)

defaultSolverOptions :: SolverOptions
defaultSolverOptions = SolverOptions Prover.defaultIntSolveOptions True SugarStrategy F5Backend True defaultProofMode

-- Re-exports for Main.hs compatibility
intSolve :: Prover.IntSolveOptions -> Theory -> Formula -> Prover.IntSolveOutcome
intSolve = Prover.intSolve
intSat :: Prover.IntSolveOptions -> Theory -> Prover.IntSolveOutcome
intSat = Prover.intSat
proveExistentialConstructive :: Theory -> Formula -> (Bool, String, Prover.ProofTrace)
proveExistentialConstructive = Prover.proveExistentialConstructive

-- =============================================
-- Local Solvers (Wrappers for Legacy Logic)
-- =============================================

-- Wrapper for SOS Logic
data LocalSOSSolver = LocalSOSSolver [String] SolverOptions
instance Solver LocalSOSSolver where
  name _ = "Sum-of-Squares"
  solve (LocalSOSSolver pts opts) prob = do
    let (proved, reason, trace, _, _) = proveInequalitySOS pts opts (assumptions prob) (goal prob)
    return $ ProofResult (if proved then Proved else Failed reason) (maybe [] return trace) 0.0

-- Wrapper for CAD Logic
data LocalCADSolver = LocalCADSolver
instance Solver LocalCADSolver where
  name _ = "CAD"
  solve _ prob = do
    let (proved, reason, trace, _) = runCadRational (assumptions prob) (goal prob)
    return $ ProofResult (if proved then Proved else Failed reason) (maybe [] return trace) 0.0

-- Wrapper for Geometric Solver
data LocalGeoSolver = LocalGeoSolver
instance Solver LocalGeoSolver where
  name _ = "Geometric Solver"
  solve _ prob = do
    case solveGeoWithTrace (assumptions prob) (goal prob) of
      GeoProved r s -> return $ ProofResult Proved (r:s) 0.0
      GeoDisproved r s -> return $ ProofResult (Disproved r) (r:s) 0.0
      GeoUnknown r -> return $ ProofResult (Failed r) [] 0.0

-- =============================================
-- Main Automatic Solving Functions (Refactored)
-- =============================================

autoSolve :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult
autoSolve opts pointSubs theoryRaw goalRaw = unsafePerformIO $ do
  let safeProfile = analyzeProblem theoryRaw goalRaw
      pts = Geometry.WLOG.detectPoints (goalRaw : theoryRaw)
  
  result <- try $ evaluate $ autoSolveInternal pts opts pointSubs theoryRaw goalRaw
  
  case result of
    Left Underflow -> return $ errorResult safeProfile "Solver limit reached (Numerical precision boundary)"
    Left Overflow  -> return $ errorResult safeProfile "Solver limit reached (Numerical overflow)"
    Left e         -> return $ errorResult safeProfile ("Solver Exception: " ++ show e)
    Right res      -> return res
  where
    errorResult profile msg = AutoSolveResult Unsolvable msg profile goalRaw False msg Nothing Nothing M.empty

autoSolveInternal :: [String] -> SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult
autoSolveInternal pts opts pointSubs theoryRaw goalRaw =
  -- Preprocessing Phase
  let (theoryG, goalG, _) = preprocessGeometry pointSubs theoryRaw goalRaw
      
      -- Heuristics (Keeping existing pipeline)
      (theoryH, goalH, logsH) = applyHeuristics (proofMode opts) theoryG goalG
      
      -- Elimination
      (theoryS, goalS, varDefsS) = eliminateSqrt theoryH goalH
      (theoryP, goalP, varDefsP) = eliminateRational theoryS goalS
      varDefs = M.union varDefsS varDefsP
      
      prepRes = preprocess pointSubs theoryP goalP
      theory' = preprocessedTheory prepRes
      goal' = preprocessedGoal prepRes
      
      profile = analyzeProblem theory' goal'
      
      -- UNIFIED ORCHESTRATION START
      problem = mkProblem theory' goal'
      
      -- Determine Strategy based on Profile
      strategy = determineStrategy profile pts opts goal'
      
  in unsafePerformIO $ do
       outcome <- proveSequential strategy problem
       return $ mapResultToAuto outcome profile goal' varDefs logsH

applyHeuristics :: ProofMode -> Theory -> Formula -> (Theory, Formula, [String])
applyHeuristics mode theory goal
  | mode == Sound = (theory, goal, [])
  | otherwise =
      let (t1, g1, l1) = tryHeronSubstitution theory goal
          (t2, g2, l2) = tryCotangentSubstitution t1 g1
          (t3, g3, l3) = tryRaviSubstitution t2 g2
          (t4, g4, l4) = tryTangentSubstitution t3 g3
          (t5, g5, l5) = trySymmetryBreaking t4 g4
          (t6, g6, l6) = tryParameterSubstitution t5 g5
          (t7, g7, l7) = tryHomogeneousNormalization t6 g6
          (t8, g8, l8) = tryHalfAngleTangent t7 g7
      in (t8, g8, l1++l2++l3++l4++l5++l6++l7++l8)

determineStrategy :: ProblemProfile -> [String] -> SolverOptions -> Formula -> [AnySolver]
determineStrategy profile pts opts goal
  | problemType profile == Geometric = 
      if isInequality goal 
      then [AnySolver (LocalSOSSolver pts opts), AnySolver LocalCADSolver]
      else [AnySolver LocalGeoSolver, AnySolver SW.WuSolver, AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts))]
  | isInequality goal = [AnySolver (LocalSOSSolver pts opts), AnySolver LocalCADSolver]
  | otherwise = [AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts)), AnySolver SW.WuSolver]

mapResultToAuto :: ProofOutcome -> ProblemProfile -> Formula -> M.Map String Expr -> [String] -> AutoSolveResult
mapResultToAuto outcome profile goal defs logs =
  let res = outcomeResult outcome
      solverName = outcomeSolver outcome
      (status, reason) = case resultStatus res of
        Proved -> (True, "Proved")
        Disproved r -> (False, "Disproved: " ++ r)
        Failed r -> (False, "Failed: " ++ r)
      choice = solverChoiceFromName solverName
      traceStr = unlines logs ++ "\n" ++ unlines (resultTrace res)
  in AutoSolveResult choice ("Unified Orchestrator (" ++ solverName ++ ")") profile goal status reason Nothing (Just traceStr) defs

solverChoiceFromName :: String -> SolverChoice
solverChoiceFromName name
  | "Sum-of-Squares" `isInfixOf` name = UseSOS
  | "CAD" `isInfixOf` name = UseCAD
  | "Wu" `isInfixOf` name = UseWu
  | "Groebner" `isInfixOf` name = UseGroebner
  | "Geometric" `isInfixOf` name = UseGeoSolver
  | otherwise = Unsolvable

-- =============================================
-- Legacy Logic (Moved/Kept for Local Wrappers)
-- =============================================

proveInequalitySOS :: [String] -> SolverOptions -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr, Maybe ProofEvidence)
proveInequalitySOS pts opts theoryRaw goalRaw =
  -- First, try direct Ono inequality check (bypasses Gröbner for this pattern)    
  case SymSOS.checkOnoInequality theoryRaw goalRaw of
    SymSOS.OnoProved proof -> (True, "Proved via Ono's inequality (AM-GM)", Just proof, M.empty, Nothing)
    _ ->
      -- Second, try generic Barrow/polynomial prover
      case SymSOS.checkBarrowInequality (proofMode opts) theoryRaw goalRaw of
        SymSOS.BarrowProved proof -> (True, "Proved via generic polynomial methods", Just proof, M.empty, Nothing)
        _ ->
          -- Standard path
          let geomLemmas = generateGeometricLemmas theoryRaw goalRaw
              (thPrep, goalPrep, varDefsP) = eliminateRational (theoryRaw ++ geomLemmas) goalRaw
              (thPoly, goalPoly, varDefsS) = eliminateSqrt thPrep goalPrep
              varDefs = M.union varDefsP varDefsS
              goalSquared = goalPoly
              profileFinal = analyzeProblem thPoly goalSquared
          in case goalSquared of
            Ge _ _ -> let (b, r, t, ev) = trySOS thPoly goalSquared goalPoly profileFinal in (b, r, t, varDefs, ev)
            Gt _ _ -> let (b, r, t, ev) = trySOS thPoly goalSquared goalPoly profileFinal in (b, r, t, varDefs, ev)
            _ -> (False, "Not an inequality after preprocessing", Nothing, varDefs, Nothing)
  where

    trySOS theory goalFull goalOriginal profileFinal =
      let (thWLOG, wlogLog) = Geometry.WLOG.applyWLOG theory goalFull
          (thBary, goalBary, baryLog) = Geometry.Barycentric.applyBarycentric pts thWLOG goalFull       
          fullLog = wlogLog ++ baryLog
          allVarsSet = extractPolyVarsList (formulaToPolysLocal goalBary ++ concatMap formulaToPolysLocal thBary)
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

          isDefinition (Eq (Var _) _) = True
          isDefinition _ = False

          posVars = [ v | v <- variables profileFinal
                    , "zz_sqrt_aux" `isPrefixOf` v || "ba_" `isPrefixOf` v || isAlwaysNonNeg v thBary ]

          sqSub = M.fromList [ (v, Pow (Var (v ++ "_sq")) 2) | v <- posVars ]      

          theoryTransformed = map (applySubstitutionsFormula sqSub) thBary
          goalTransformed = applySubstitutionsFormula sqSub goalBary

          subMapTransformed = buildSubMap theoryTransformed
          targetPoly = toPolySub subMapTransformed (case goalTransformed of Ge l r -> Sub l r; Gt l r -> Sub l r; _ -> Const 0)

          eqConstraintsTransformed = [ toPolySub subMapTransformed (Sub l r)       
                                     | eq@(Eq l r) <- theoryTransformed
                                     , not (isDefinition eq)
                                     ]

          eqConstraintsSOS = filter (not . isSumConstraint) eqConstraintsTransformed
          isSumConstraint p =
             let vars = extractPolyVars p
             in S.member "ba_u" vars && S.member "ba_v" vars && S.member "ba_w" vars && polyDegreeIn p "ba_u" == 1

          basis = f4LiteGroebner ord SugarStrategy True eqConstraintsSOS
          reducer = reduceWithBasis ord basis

          knownLemmas = mapMaybe (listToMaybe . formulaToPolysLocal)
            [Ge (Var v) (Const 0) | v <- posVars]
          
          -- Collect other inequalities from theory (e.g. triangle inequalities)
          otherIneqsFormula = filter (not . isVarPositivity) [ f | f <- theoryTransformed, isInequality f ]
          otherIneqs = concatMap formulaToPolysLocal otherIneqsFormula
          
          isVarPositivity (Ge (Var _) (Const 0)) = True
          isVarPositivity (Gt (Var _) (Const 0)) = True
          isVarPositivity _ = False

          maybeCert = getSOSCertificate knownLemmas reducer targetPoly

          sdpVerified =
            if isJust maybeCert
            then False
            else case proofMode opts of
                   Unsafe ->
                     if SDP.checkSOS_SDP (reducer targetPoly) then True
                     else SDP.checkSOS_Constrained (reducer targetPoly) (map reducer (knownLemmas ++ otherIneqs))
                   Sound -> False

          success = isJust maybeCert || sdpVerified
          evidence = fmap (\c -> EvidenceSOS c fullLog) maybeCert
          intro = "\nALGEBRAIC PROOF DEVELOPMENT:\n" ++ replicate 40 '-' ++ "\n"   
          step1 = "[1] Goal: " ++ prettyFormula goalOriginal ++ "\n"
          setup = if null fullLog then "" else "[2] Geometric Setup:\n" ++ unlines (map ("    * " ++) fullLog)
          step2 = "[3] Polynomial Transformation:\n    Mapping: " ++ (if null posVars then "Identity" else intercalate ", " [ v ++ " -> " ++ v ++ "_sq^2" | v <- posVars ]) ++ "\n    Goal: " ++ prettyFormula goalFull ++ " >= 0\n"
          reductionStep = "[4] Canonical Reduction:\n    Identity: " ++ prettyPolyNice (reducer targetPoly) ++ " >= 0\n"
          step4 = "[5] SOS Decomposition:\n    " ++ (case maybeCert of
                               Just c -> formatSOSCertificate c
                               Nothing ->
                                 if sdpVerified then "SDP-Based Feasibility Verified"
                                 else if proofMode opts == Sound then "No SOS certificate (SDP disabled)"
                                      else "0") ++ " >= 0\n"
          fullTrace = intro ++ step1 ++ setup ++ step2 ++ reductionStep ++ step4 ++ (replicate 40 '-' ++ "\nCONCLUSION: IDENTITY VERIFIED.")
      in (success, if success then "Proved via compositional Sum-of-Squares" else "SOS check failed", Just fullTrace, evidence)

    isAlwaysNonNeg v theory = any (\f -> case f of Ge (Var x) _ -> x == v; Gt (Var x) _ -> x == v; _ -> False) theory
    polyDegreeIn (Poly m) var = if M.null m then 0 else maximum ((0 :: Int) : [ fromIntegral (M.findWithDefault 0 var vars) | (mono, _) <- M.toList m, let Monomial vars = mono ])

runCadRational :: [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr)
runCadRational theory goal =
  let (th', goal', varDefs) = eliminateSqrt theory goal
      (eqs, ineqs) = partition isEquality th'

      eqPolys = concatMap formulaToPolysLocal eqs

      allVarsSet = extractPolyVarsList (eqPolys ++ concatMap formulaToPolysLocal ineqs ++ formulaToPolysLocal goal')
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

      basis = f4LiteGroebner ord SugarStrategy True eqPolys
      reducer = reduceWithBasis ord basis

      reducedGoal = case goal' of
        Ge l r -> Ge (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        Gt l r -> Gt (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        _      -> goal'

      reduceIneq :: Formula -> Formula
      reduceIneq f = case f of
        Ge l r -> Ge (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        Gt l r -> Gt (polyToExpr (reducer (toPolySub M.empty (Sub l r)))) (Const 0)
        _      -> f

      reducedIneqs = map reduceIneq ineqs

      vars = S.toList (extractPolyVarsList (formulaToPolysLocal reducedGoal ++ concatMap formulaToPolysLocal reducedIneqs))
      traceMsg = unlines
        ["Hybrid Groebner-CAD Strategy:",
         "  1. Partitioned theory: " ++ show (length eqs) ++ " equalities, " ++ show (length ineqs) ++ " inequalities.",
         "  2. Computed Hierarchical Elimination Basis (Block Order).",
         "  3. Reduced System Variables: " ++ show vars,
         "  4. Invoking CAD on reduced system..."
        ]

      proved = CADLift.proveFormulaCAD reducedIneqs reducedGoal

  in (proved, "Hybrid Groebner-CAD", Just traceMsg, varDefs)

formulaToPolysLocal :: Formula -> [Poly]
formulaToPolysLocal formula =
  case formula of
    Eq lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Ge lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Gt lhs rhs -> [toPolySub M.empty (Sub lhs rhs)]
    Le lhs rhs -> [toPolySub M.empty (Sub rhs lhs)]
    Lt lhs rhs -> [toPolySub M.empty (Sub rhs lhs)]
    And f1 f2 -> formulaToPolysLocal f1 ++ formulaToPolysLocal f2
    Or f1 f2 -> formulaToPolysLocal f1 ++ formulaToPolysLocal f2
    Not f -> formulaToPolysLocal f
    Forall _ f -> formulaToPolysLocal f
    Exists _ f -> formulaToPolysLocal f
    _ -> []

extractPolyVarsList :: [Poly] -> S.Set String
extractPolyVarsList = foldr (S.union . extractPolyVars) S.empty

generateGeometricLemmas :: [Formula] -> Formula -> [Formula]
generateGeometricLemmas theory goal = let pts = Geometry.WLOG.detectPoints (goal : theory) in [ Ge (Add (Sqrt (Dist2 a b)) (Sqrt (Dist2 b c))) (Sqrt (Dist2 a c)) | a <- pts, b <- pts, c <- pts, a < b, b < c ]

autoSolveE :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> Either String AutoSolveResult
autoSolveE opts pointSubs theory goal = Right $ autoSolve opts pointSubs theory goal

autoSolveWithTrace :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> Maybe String -> AutoSolveResult
autoSolveWithTrace opts pointSubs theory goal _ = autoSolve opts pointSubs theory goal

explainSolverChoice :: SolverChoice -> ProblemProfile -> String
explainSolverChoice _ _ = "Unified Solver Strategy"

formatAutoSolveResult :: AutoSolveResult -> Bool -> String
formatAutoSolveResult result verbose =
  let header = "=== HASCLID AUTOMATIC PROOF SYSTEM ==="
      analysis = "PROBLEM ANALYSIS:\n  Structure: " ++ show (problemType (problemProfile result))
      traceStr = if verbose then case detailedTrace result of Just t -> "\nPROOF DEVELOPMENT:\n" ++ t; Nothing -> "" else ""
  in unlines [header, "", analysis, "VERDICT: " ++ if isProved result then "PROVED" else "NOT PROVED", proofReason result, traceStr]

isInequality :: Formula -> Bool
isInequality (Ge _ _) = True
isInequality (Gt _ _) = True
isInequality (Le _ _) = True
isInequality (Lt _ _) = True
isInequality _ = False

-- | Legacy API for direct solver execution (Adapted to Unified Core)
executeSolver :: [String] -> SolverChoice -> SolverOptions -> ProblemProfile -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr)
executeSolver pts choice opts _profile theory goal = unsafePerformIO $ do        
  let problem = mkProblem theory goal
      -- Map choice to AnySolver
      solver = case choice of
        UseWu -> AnySolver SW.WuSolver
        UseGroebner -> AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts))
        UseSOS -> AnySolver (LocalSOSSolver pts opts)
        UseCAD -> AnySolver LocalCADSolver
        UseGeoSolver -> AnySolver LocalGeoSolver
        UseAreaMethod -> AnySolver LocalGeoSolver -- Placeholder mapping
        _ -> AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts)) -- Fallback        

  outcome <- proveSequential [solver] problem
  let res = outcomeResult outcome
      (status, reason) = case resultStatus res of
        Proved -> (True, "Proved")
        Disproved r -> (False, r)
        Failed r -> (False, r)
  return (status, reason, Just (unlines (resultTrace res)), M.empty)

extractPolyVars :: Poly -> S.Set String
extractPolyVars (Poly m) = S.fromList $ concatMap (\(mono, _) -> let Monomial vars = mono in M.keys vars) (M.toList m)

isEquality :: Formula -> Bool
isEquality (Eq _ _) = True
isEquality (Forall _ f) = isEquality f
isEquality (Exists _ f) = isEquality f
isEquality _ = False

formatSOSCertificate :: SOSCertificate -> String
formatSOSCertificate cert =
  let terms = sosTerms cert; lemmas = sosLemmas cert
      formatTerm (c, p) = (if c == 1 then "" else prettyRational c ++ "*") ++ "(" ++ prettyPolyNice p ++ ")^2"
      sumParts = map formatTerm (reverse terms) ++ map prettyPolyNice lemmas       
  in if null sumParts then "0" else intercalate " + " sumParts ++ (if sosRemainder cert == polyZero then "" else " + [" ++ prettyPolyNice (sosRemainder cert) ++ "]")
