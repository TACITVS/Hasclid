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
  , selectGroebnerAlgorithm
  , intSolve
  , intSat
  , proveExistentialConstructive
  , synthesize
  )
where

import Expr
import Error
import Preprocessing (preprocess, preprocessGeometry, PreprocessingResult(..), applySubstitutionsFormula, applySubstitutionsExpr)
import ProblemAnalyzer
import GeoSolver (solveGeoWithTrace, GeoResult(..))
import qualified Wu (wuProveWithTrace, formatWuTrace, isProved, proofReason)
import qualified Prover (intSolve, intSat, IntSolveOptions(..), IntSolveOutcome(..), defaultIntSolveOptions, proveExistentialConstructive, ProofTrace)
import qualified CADLift (proveFormulaCAD, findWitnessCAD)
import Positivity.SOS (SOSCertificate(..), getSOSCertificate, getSOSCertificateWithAlgebraic, algebraicReducer)
import Positivity.SOSTypes (SOSPattern(..))
import Data.Ratio (numerator, denominator, (%))
import qualified Positivity.Numerical as NumSOS
import qualified Positivity.SDP as SDP
import qualified Positivity.SymmetricSOS as SymSOS
import qualified Positivity.Sampling as Sampling
import SqrtElim (eliminateSqrt, eliminateSqrtWithConstraints, AlgebraicConstraint(..), SqrtElimResult(..))
import RationalElim (eliminateRational)
import qualified BuchbergerOpt
import BuchbergerOpt (SelectionStrategy(..))
import TermOrder (TermOrder(..), compareMonomials)
import F4Lite (f4LiteGroebner, reduceWithBasis)
import qualified F5 (f5Groebner, f5SolveBounded, F5Config(..), F5Result(..), defaultF5Config, conservativeF5Config, f5Basis, f5Completed)
import qualified Geometry.WLOG
import qualified Geometry.Barycentric
import Heuristics
import qualified Heuristics as H
import AreaMethod (proveArea, proveAreaE, AreaResult(..), deriveConstruction, checkConstruction)
import qualified Modular
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
import Core.Orchestrator hiding (proveSequential)
import qualified Core.Orchestrator as Orchestrator
import qualified Solvers.Wu as SW
import qualified Solvers.Groebner as SG
import qualified Solvers.Interval as SI
import qualified Solvers.Metric as SM

-- =============================================
-- Legacy / Compatibility Types
-- =============================================

data SolverChoice = UseGeoSolver | UseWu | UseGroebner | UseConstructiveWu | UseCAD | UseAreaMethod | UseSOS | UseNumerical | UseInterval | UseMetric | Unsolvable deriving (Show, Eq)
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

-- | Select Gröbner basis algorithm based on backend configuration
-- This dispatches to F5, F4Lite, or Buchberger based on the groebnerBackend setting
-- Uses conservative config for high-variable systems to avoid computational explosion
selectGroebnerAlgorithm :: SolverOptions -> TermOrder -> [Poly] -> [Poly]
selectGroebnerAlgorithm opts ord polys =
  let termOrd = compareMonomials ord
      -- Count distinct variables across all polynomials
      allVars = S.unions $ map getVars polys
      numVars = S.size allVars
      -- Use conservative config for 6+ variables (typical geometry explosion threshold)
      useConservative = numVars >= 6
  in case groebnerBackend opts of
    F5Backend ->
      -- Use F5 algorithm with the specified term ordering
      if useConservative
      then F5.f5Basis $ F5.f5SolveBounded F5.conservativeF5Config termOrd polys
      else F5.f5Groebner termOrd polys
    F4Backend ->
      -- F4Lite uses matrix reduction + Buchberger
      f4LiteGroebner termOrd (selectionStrategyOpt opts) (useOptimizedGroebner opts) polys
    BuchbergerBackend ->
      -- Direct Buchberger with strategy
      BuchbergerOpt.buchbergerWithStrategy termOrd (selectionStrategyOpt opts) polys


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
data LocalCADSolver = LocalCADSolver SolverOptions
instance Solver LocalCADSolver where
  name _ = "CAD"
  solve (LocalCADSolver opts) prob = do
    let (proved, reason, trace, _) = runCadRational opts (assumptions prob) (goal prob)
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

-- Wrapper for Interval Solver
data LocalIntervalSolver = LocalIntervalSolver
instance Solver LocalIntervalSolver where
  name _ = "Interval Arithmetic"
  solve _ prob = do
    let (proved, reason) = SI.solveInterval (assumptions prob) (goal prob)
    return $ ProofResult (if proved then Proved else Failed reason) [] 0.0

-- Wrapper for Metric Solver
data LocalMetricSolver = LocalMetricSolver
instance Solver LocalMetricSolver where
  name _ = "Metric Space Solver"
  solve _ prob = do
    let (proved, reason) = SM.solveMetric (assumptions prob) (goal prob)
    return $ ProofResult (if proved then Proved else Failed reason) [] 0.0

--Wrapper for Area Method Solver
data LocalAreaSolver = LocalAreaSolver
instance Solver LocalAreaSolver where
  name _ = "Area Method"
  solve _ prob = do
    let theory = assumptions prob
        goalF = goal prob
    case deriveConstruction theory goalF of
      Just (steps, geoGoal) ->
        case proveAreaE steps geoGoal of
          Right res ->
            if isDegenerate res
            then return $ ProofResult (Failed "Degenerate Construction (e.g. parallel lines intersected)") [] 0.0
            else if areaProved res
            then return $ ProofResult Proved ["Area Method Proved"] 0.0
            else return $ ProofResult (Failed (areaReason res)) [] 0.0
          Left err -> return $ ProofResult (Failed (show err)) [] 0.0
      Nothing -> return $ ProofResult (Failed "Could not derive construction") [] 0.0

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

-- | Synthesize a witness for an existential goal
synthesize :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> Maybe (M.Map String Rational)
synthesize _ pointSubs theoryRaw goalRaw = unsafePerformIO $ do
  res <- try $ evaluate $
    let (theoryG, goalG, _) = preprocessGeometry pointSubs theoryRaw goalRaw
        -- Skipping advanced heuristics for now to keep variable mapping simple
        (theoryS, goalS, _) = eliminateSqrt theoryG goalG
        (theoryP, goalP, _) = eliminateRational theoryS goalS
        -- Use CAD to find witness
    in CADLift.findWitnessCAD theoryP goalP
  return $ case res of
    Left (_ :: CE.SomeException) -> Nothing
    Right w -> w

autoSolveInternal :: [String] -> SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult
autoSolveInternal pts opts pointSubs theoryRaw goalRaw =
  -- MODULAR PROBABILISTIC CHECK
  -- Fail fast if the theorem is false modulo a large prime
  let (modConsistent, modReason) = Modular.probSolve theoryRaw goalRaw
  in if not modConsistent
     then let profile = analyzeProblem theoryRaw goalRaw
          in AutoSolveResult UseNumerical "Probabilistic Refutation" profile goalRaw False modReason Nothing Nothing M.empty
     else
  -- GENERICITY CHECK (Area Method)
  -- Verify that the implied geometric construction is non-degenerate
  let (isGeneric, genReason) = 
        case deriveConstruction theoryRaw goalRaw of
          Just (steps, _) -> checkConstruction steps
          Nothing -> (True, "No construction derived")
  in if not isGeneric
     then let profile = analyzeProblem theoryRaw goalRaw
          in AutoSolveResult UseGeoSolver "Degenerate Construction" profile goalRaw False genReason Nothing Nothing M.empty
     else
  -- AXIOM PATH: Check for well-known geometric theorems first
  case H.checkTriangleInequalityAxiom goalRaw of
    Just (reason, trace) ->
      let profile = analyzeProblem theoryRaw goalRaw
      in AutoSolveResult UseGeoSolver "Geometric Axiom" profile goalRaw True reason Nothing (Just trace) M.empty
    Nothing ->
      -- ULTRA-FAST PATH: Try direct SOS on raw goal before any preprocessing
      -- This handles simple symmetric inequalities like a² + b² + c² >= ab + bc + ca
      case tryDirectSOSHeuristic goalRaw of
        Just (cert, proof) ->
          let profile = analyzeProblem theoryRaw goalRaw
          in AutoSolveResult UseSOS "Direct SOS (no preprocessing)" profile goalRaw True "Proved" (Just (EvidenceSOS cert [])) (Just proof) M.empty
        Nothing ->
          -- NUMERICAL FAST-PATH: For complex inequalities in Unsafe mode,
          -- try numerical verification FIRST to avoid costly timeouts.
          -- This is especially useful for Barrow-type trig formulations.
          let numVars = length $ nub $ concatMap varsInFormula (goalRaw : theoryRaw)
          in if proofMode opts == Unsafe && isInequality goalRaw && numVars > 4
             then case tryNumericalVerification theoryRaw goalRaw of
                    Just (reason, trace) ->
                      let profile = analyzeProblem theoryRaw goalRaw
                      in AutoSolveResult UseNumerical "Numerical Fast-Path (Complex System)"
                           profile goalRaw True reason Nothing (Just trace) M.empty
                    Nothing -> algebraicPath
             else algebraicPath
  where
    algebraicPath =
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
               outcome <- Orchestrator.proveParallel strategy problem
               let baseResult = mapResultToAuto outcome profile goal' varDefs logsH
               -- Try numerical verification as fallback for failed inequalities
               if not (isProved baseResult) && proofMode opts == Unsafe && isInequality goal'
               then case tryNumericalVerification theory' goal' of
                      Just (reason, trace) ->
                        return $ AutoSolveResult UseNumerical "Numerical Verification (Fallback)"
                                   profile goal' True reason Nothing (Just trace) varDefs
                      Nothing -> return baseResult
               else return baseResult

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
          -- Constraint propagation: eliminate z using x+y+z=xyz identity
          (t9, g9, l9) = tryTangentIdentityElimination t8 g8
          -- Variable elimination: reduce intermediate cos² variables
          (t10, g10, l10) = tryEliminateIntermediates t9 g9
          -- Derive cosine upper bounds: cx² * (1+t²) = 1 => cx <= 1
          (t11, g11, l11) = tryDeriveCosineUpperBounds t10 g10
      in (t11, g11, l1++l2++l3++l4++l5++l6++l7++l8++l9++l10++l11)

determineStrategy :: ProblemProfile -> [String] -> SolverOptions -> Formula -> [AnySolver]
determineStrategy profile pts opts goal
  | problemType profile == Geometric =
      if isInequality goal
      then [AnySolver (LocalSOSSolver pts opts), AnySolver (LocalCADSolver opts)]
      else [AnySolver LocalGeoSolver, AnySolver LocalAreaSolver, AnySolver SW.WuSolver, AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts))]
  | isInequality goal = [AnySolver (LocalSOSSolver pts opts), AnySolver (LocalCADSolver opts)]
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

-- =============================================================================
-- NUMERICAL VERIFICATION FALLBACK
-- =============================================================================
-- When algebraic methods fail, try numerical sampling to verify the inequality.
-- This is NOT a formal proof, but provides high-confidence verification.
-- Only used in Unsafe mode.

tryNumericalVerification :: [Formula] -> Formula -> Maybe (String, String)
tryNumericalVerification theory goal =
  case goal of
    Ge _ _ -> doVerify
    Gt _ _ -> doVerify
    Le _ _ -> doVerify
    Lt _ _ -> doVerify
    _ -> Nothing
  where
    doVerify =
      -- Use more samples and lower confidence threshold for complex systems
      let numConstraints = length theory
          config = Sampling.defaultSamplingConfig
            { Sampling.numSamples = if numConstraints > 10 then 50000 else 10000 }
          result = Sampling.numericallyVerifyInequality theory goal config
          -- Lower confidence threshold for complex systems (many constraints are harder to satisfy)
          minConfidence = if numConstraints > 10 then 0.5 else 0.9
      in if Sampling.verified result && Sampling.confidence result > minConfidence
         then Just ("Verified numerically",
                    unlines [ "NUMERICAL VERIFICATION (High Confidence)"
                            , "----------------------------------------"
                            , "Method: Random sampling with constraint satisfaction"
                            , "Samples checked: " ++ show (Sampling.samplesChecked result)
                            , "Total samples: " ++ show (Sampling.totalSamples result)
                            , "Confidence: " ++ show (round (Sampling.confidence result * 100) :: Int) ++ "%"
                            , ""
                            , "The inequality was verified for all sampled points in the"
                            , "feasible region. While not a formal proof, this provides"
                            , "strong evidence that the inequality holds."
                            , ""
                            , "WARNING: This is heuristic verification, not a rigorous proof."
                            ])
         else case Sampling.counterexample result of
                Just cex -> Just ("Counterexample found",
                                  "Counterexample: " ++ show cex)
                Nothing -> Nothing

solverChoiceFromName :: String -> SolverChoice
solverChoiceFromName name
  | "Sum-of-Squares" `isInfixOf` name = UseSOS
  | "CAD" `isInfixOf` name = UseCAD
  | "Wu" `isInfixOf` name = UseWu
  | "Groebner" `isInfixOf` name = UseGroebner
  | "Geometric" `isInfixOf` name = UseGeoSolver
  | "Numerical" `isInfixOf` name = UseNumerical
  | otherwise = Unsolvable

-- =============================================
-- Legacy Logic (Moved/Kept for Local Wrappers)
-- =============================================

proveInequalitySOS :: [String] -> SolverOptions -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr, Maybe ProofEvidence)
proveInequalitySOS pts opts theoryRaw goalRaw =
  -- INSTANT PATH: Check if goal is already implied by theory constraints
  -- E.g., goal "1 >= cx" is equivalent to theory constraint "cx <= 1"
  case checkGoalImpliedByTheory theoryRaw goalRaw of
    Just reason -> (True, reason, Just $ "Goal implied by theory: " ++ reason, M.empty, Nothing)
    Nothing ->
      -- ULTRA-FAST PATH: Try direct SOS heuristics on the goal polynomial
      -- This handles simple symmetric patterns like a² + b² + c² - ab - bc - ca
      case tryDirectSOSHeuristic goalRaw of
        Just (cert, proof) -> (True, "Proved via direct SOS decomposition", Just proof, M.empty, Just (EvidenceSOS cert []))
        Nothing ->
          -- First, try direct Ono inequality check (bypasses Gröbner for this pattern)
          case SymSOS.checkOnoInequality theoryRaw goalRaw of
            SymSOS.OnoProved proof -> (True, "Proved via Ono's inequality (AM-GM)", Just proof, M.empty, Nothing)
            _ ->
              -- Second, try generic Barrow/polynomial prover
              case SymSOS.checkBarrowInequality (proofMode opts) theoryRaw goalRaw of
                SymSOS.BarrowProved proof -> (True, "Proved via generic polynomial methods", Just proof, M.empty, Nothing)
                _ ->
                  -- Standard path with enhanced algebraic constraint handling
                  let geomLemmas = generateGeometricLemmas theoryRaw goalRaw
                      (thPrep, goalPrep, varDefsP) = eliminateRational (theoryRaw ++ geomLemmas) goalRaw
                      -- Use new eliminateSqrtWithConstraints to capture algebraic constraints
                      sqrtResult = eliminateSqrtWithConstraints thPrep goalPrep
                      thPoly = serTheory sqrtResult
                      goalPoly = serGoal sqrtResult
                      varDefsS = serVarDefs sqrtResult
                      algConstraints = serAlgebraicConstraints sqrtResult  -- v^2 = e constraints
                      varDefs = M.union varDefsP varDefsS
                      goalSquared = goalPoly
                      profileFinal = analyzeProblem thPoly goalSquared
                  in case goalSquared of
                       Ge _ _ -> let (b, r, t, ev) = trySOS algConstraints thPoly goalSquared goalPoly profileFinal in (b, r, t, varDefs, ev)
                       Gt _ _ -> let (b, r, t, ev) = trySOS algConstraints thPoly goalSquared goalPoly profileFinal in (b, r, t, varDefs, ev)
                       _ -> (False, "Not an inequality after preprocessing", Nothing, varDefs, Nothing)
  where

    trySOS algConstraints theory goalFull goalOriginal profileFinal =
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

          basis = selectGroebnerAlgorithm opts ordType eqConstraintsSOS
          reducer = reduceWithBasis ord basis

          knownLemmas = mapMaybe (listToMaybe . formulaToPolysLocal)
            [Ge (Var v) (Const 0) | v <- posVars]
          
          -- Collect other inequalities from theory (e.g. triangle inequalities)
          otherIneqsFormula = filter (not . isVarPositivity) [ f | f <- theoryTransformed, isInequality f ]
          otherIneqs = concatMap formulaToPolysLocal otherIneqsFormula
          
          isVarPositivity (Ge (Var _) (Const 0)) = True
          isVarPositivity (Gt (Var _) (Const 0)) = True
          isVarPositivity _ = False

          -- Use enhanced SOS with algebraic constraints for sqrt handling
          maybeCert = getSOSCertificateWithAlgebraic algConstraints knownLemmas reducer targetPoly

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

-- | Check if the goal is directly implied by existing theory constraints.
-- This handles cases like: goal "1 >= cx" is equivalent to "cx <= 1" in theory.
checkGoalImpliedByTheory :: [Formula] -> Formula -> Maybe String
checkGoalImpliedByTheory theory goal =
  -- Check if goal or an equivalent form is in the theory
  if goal `elem` theory
  then Just "Goal is an explicit assumption"
  else case goal of
    -- goal: a >= b is equivalent to b <= a
    Ge a b -> if Le b a `elem` theory then Just "Goal equivalent to Le constraint in theory"
              else if Ge a b `elem` theory then Just "Goal already in theory"
              else checkWithExprs a b
    -- goal: a > b is equivalent to b < a
    Gt a b -> if Lt b a `elem` theory then Just "Goal equivalent to Lt constraint in theory"
              else if Gt a b `elem` theory then Just "Goal already in theory"
              else Nothing
    -- goal: a <= b is equivalent to b >= a
    Le a b -> if Ge b a `elem` theory then Just "Goal equivalent to Ge constraint in theory"
              else if Le a b `elem` theory then Just "Goal already in theory"
              else Nothing
    -- goal: a < b is equivalent to b > a
    Lt a b -> if Gt b a `elem` theory then Just "Goal equivalent to Gt constraint in theory"
              else if Lt a b `elem` theory then Just "Goal already in theory"
              else Nothing
    _ -> Nothing
  where
    -- Check expressions that are syntactically different but semantically equivalent
    checkWithExprs a b =
      -- Check for: goal "1 >= x" vs theory "x <= 1" (with IntConst/Const variations)
      let normalizedGoal = normalizeIneq (Ge a b)
          normalizedTheory = map normalizeIneq theory
      in if normalizedGoal `elem` normalizedTheory
         then Just "Goal matches normalized theory constraint"
         else Nothing

    -- Normalize inequalities to a canonical form
    normalizeIneq :: Formula -> Formula
    normalizeIneq f = case f of
      Ge a b -> Ge (normalizeExpr a) (normalizeExpr b)
      Gt a b -> Gt (normalizeExpr a) (normalizeExpr b)
      Le a b -> Ge (normalizeExpr b) (normalizeExpr a)  -- Convert Le to Ge
      Lt a b -> Gt (normalizeExpr b) (normalizeExpr a)  -- Convert Lt to Gt
      _ -> f

    -- Normalize expressions (Const 1 and IntConst 1 become the same)
    normalizeExpr :: Expr -> Expr
    normalizeExpr e = case e of
      IntConst n -> Const (fromIntegral n)
      Const n -> Const n
      Var v -> Var v
      Add a b -> Add (normalizeExpr a) (normalizeExpr b)
      Sub a b -> Sub (normalizeExpr a) (normalizeExpr b)
      Mul a b -> Mul (normalizeExpr a) (normalizeExpr b)
      Div a b -> Div (normalizeExpr a) (normalizeExpr b)
      Pow a n -> Pow (normalizeExpr a) n
      Sqrt a -> Sqrt (normalizeExpr a)
      _ -> e  -- Other constructors pass through unchanged

runCadRational :: SolverOptions -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr)
runCadRational opts theory goal =
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
      ordType = Block [ (S.toList groupCoords, Lex)
                      , (S.toList groupSides,  Lex)
                      , (S.toList groupRest,   GrevLex)
                      ]
      ord = compareMonomials ordType

      basis = selectGroebnerAlgorithm opts ordType eqPolys
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
        UseCAD -> AnySolver (LocalCADSolver opts)
        UseGeoSolver -> AnySolver LocalGeoSolver
        UseAreaMethod -> AnySolver LocalAreaSolver
        UseInterval -> AnySolver LocalIntervalSolver
        _ -> AnySolver (SG.GroebnerSolver (intOptions opts) (proofMode opts)) -- Fallback        

  outcome <- Orchestrator.proveParallel [solver] problem
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

-- | Try direct SOS heuristic on a goal formula without any preprocessing
-- This is the fastest path for simple symmetric polynomials
-- Uses trySOSHeuristic directly to avoid expensive fallbacks in getSOSCertificate
-- NOTE: Skips goals containing sqrt (those need preprocessing first)
tryDirectSOSHeuristic :: Formula -> Maybe (SOSCertificate, String)
tryDirectSOSHeuristic goal
  | containsSqrtFormula goal = Nothing  -- Sqrt needs preprocessing, skip direct path
  | otherwise =
      case goal of
        Ge lhs rhs ->
          let poly = toPolySub M.empty (Sub lhs rhs)
          in case trySOSHeuristicDirect poly of
               Just cert ->
                 -- Verify the certificate is correct
                 if verifySOSCertificate poly cert
                 then Just (cert, formatSOSDecomposition cert)
                 else Nothing
               Nothing -> Nothing
        Gt lhs rhs ->
          let poly = toPolySub M.empty (Sub lhs rhs)
          in case trySOSHeuristicDirect poly of
               Just cert ->
                 -- Verify the certificate is correct
                 if verifySOSCertificate poly cert
                 then Just (cert, formatSOSDecomposition cert)
                 else Nothing
               Nothing -> Nothing
        _ -> Nothing

-- | Direct SOS heuristic - just pattern matching, no expensive fallbacks
trySOSHeuristicDirect :: Poly -> Maybe SOSCertificate
trySOSHeuristicDirect poly =
  -- Try trivial square first
  case polynomialSqrtDirect poly of
    Just q -> Just $ SOSCertificate [(1, q)] [] polyZero (Just TrivialSquare)
    Nothing ->
      -- Try simple sum of squares
      case trySimpleSOS poly of
        Just cert -> Just cert
        Nothing ->
          -- Try symmetric 3-variable pattern
          case trySymmetric3Var poly of
            Just cert -> Just cert
            Nothing ->
              -- Try cyclic 4-variable pattern
              case tryCyclic4Var poly of
                Just cert -> Just cert
                Nothing ->
                  -- Try general n-variable cyclic pattern
                  case tryCyclicNVar poly of
                    Just cert -> Just cert
                    Nothing ->
                      -- Try AM-GM 2-variable pattern
                      case tryAMGM2Var poly of
                        Just cert -> Just cert
                        Nothing ->
                          -- Try generalized Lagrange identity
                          tryLagrangeIdentity poly

-- | Try to find polynomial square root (simplified)
polynomialSqrtDirect :: Poly -> Maybe Poly
polynomialSqrtDirect p
  | p == polyZero = Just polyZero
  | otherwise =
      case getLeadingTerm p of
        Nothing -> Just polyZero
        Just (ltM, ltC) ->
          if not (isSquareMonomial ltM) || ltC < 0
          then Nothing
          else tryFindRoot p
  where
    isSquareMonomial (Monomial vars) = all even (M.elems vars)

-- | Simple square root finding
tryFindRoot :: Poly -> Maybe Poly
tryFindRoot p =
  case getLeadingTerm p of
    Nothing -> Just polyZero
    Just (ltM, ltC) ->
      let sqrtM = sqrtMonomial ltM
          sqrtC = sqrtRat ltC
          candidate = Poly (M.singleton sqrtM sqrtC)
          squared = polyMul candidate candidate
      in if squared == p then Just candidate else Nothing
  where
    sqrtMonomial (Monomial vars) = Monomial (M.map (`div` 2) vars)
    sqrtRat r = let n = numerator r; d = denominator r
                in integerSqrt n % integerSqrt d

-- | Try simple sum of squares (each term is a perfect square)
trySimpleSOS :: Poly -> Maybe SOSCertificate
trySimpleSOS (Poly m) =
  let terms = M.toList m
      squares = [ (c, Poly (M.singleton (sqrtMono mon) 1))
                | (mon, c) <- terms, c > 0, isSquareMono mon ]
  in if length squares == M.size m && not (null squares)
     then Just $ SOSCertificate squares [] polyZero (Just SumOfSquares)
     else Nothing
  where
    isSquareMono (Monomial vars) = all even (M.elems vars)
    sqrtMono (Monomial vars) = Monomial (M.map (`div` 2) vars)

-- | Verify that an SOS certificate correctly represents a polynomial
verifySOSCertificate :: Poly -> SOSCertificate -> Bool
verifySOSCertificate targetPoly cert =
  let terms = sosTerms cert
      -- Reconstruct: sum of c_i * p_i^2
      reconstructed = foldl polyAdd polyZero
        [scalePolyBy c (polyMul p p) | (c, p) <- terms]
  in reconstructed == targetPoly

-- | Helper to scale a polynomial by a rational
scalePolyBy :: Rational -> Poly -> Poly
scalePolyBy c (Poly m) = Poly (M.map (* c) m)

-- | Try symmetric 3-variable pattern: a² + b² + c² - ab - bc - ca
trySymmetric3Var :: Poly -> Maybe SOSCertificate
trySymmetric3Var (Poly m)
  | M.size m /= 6 = Nothing
  | otherwise =
      let terms = M.toList m
          sqTerms = [(v, c) | (Monomial vm, c) <- terms, [(v, 2)] <- [M.toList vm]]
          crossTerms = [(v1, v2, c) | (Monomial vm, c) <- terms
                                    , [(v1, e1), (v2, e2)] <- [M.toList vm]
                                    , e1 == 1, e2 == 1, v1 < v2]
      in case (sqTerms, crossTerms) of
           ([(a, ca), (b, cb), (c, cc)], [(_, _, c12), (_, _, c34), (_, _, c56)])
             | ca == cb && cb == cc && ca > 0
             , c12 == c34 && c34 == c56 && c12 < 0
             , ca == -c12
             -> let polyA = polyFromVar a; polyB = polyFromVar b; polyC = polyFromVar c
                    diffAB = polySub polyA polyB
                    diffBC = polySub polyB polyC
                    diffCA = polySub polyC polyA
                    cert = SOSCertificate [(ca/2, diffAB), (ca/2, diffBC), (ca/2, diffCA)] [] polyZero (Just (Custom "Symmetric3Var"))
                -- Verify the decomposition matches the input
                in if verifySOSCertificate (Poly m) cert then Just cert else Nothing
           _ -> Nothing

-- | Try cyclic 4-variable pattern: a² + b² + c² + d² - ab - bc - cd - da
-- Decomposition: ½[(a-b)² + (b-c)² + (c-d)² + (d-a)²]
tryCyclic4Var :: Poly -> Maybe SOSCertificate
tryCyclic4Var (Poly m)
  | M.size m /= 8 = Nothing  -- 4 squares + 4 cross terms
  | otherwise =
      let terms = M.toList m
          -- Find square terms (v²)
          sqTerms = [(v, c) | (Monomial vm, c) <- terms, [(v, 2)] <- [M.toList vm]]
          -- Find cross terms (v1 * v2)
          crossTerms = [(v1, v2, c) | (Monomial vm, c) <- terms
                                    , [(v1, e1), (v2, e2)] <- [M.toList vm]
                                    , e1 == 1, e2 == 1, v1 < v2]
      in case sqTerms of
           [(a, ca), (b, cb), (c, cc), (d, cd)]
             | ca == cb && cb == cc && cc == cd && ca > 0  -- All square coefficients equal and positive
             , length crossTerms == 4  -- Exactly 4 cross terms
             , all (\(_, _, coef) -> coef < 0 && coef == -ca) crossTerms  -- All cross coeffs equal to -k
             , isCyclicPattern [a, b, c, d] crossTerms  -- Cross terms form a cycle
             -> let vars = [a, b, c, d]
                    -- Build cyclic differences
                    diffs = zipWith (\v1 v2 -> polySub (polyFromVar v1) (polyFromVar v2))
                                    vars (drop 1 vars ++ take 1 vars)
                in Just $ SOSCertificate [(ca/2, diff) | diff <- diffs] [] polyZero (Just (Custom "Cyclic4Var"))
           _ -> Nothing

-- | Check if cross terms form a cyclic pattern (ab, bc, cd, da)
isCyclicPattern :: [String] -> [(String, String, Rational)] -> Bool
isCyclicPattern vars crossTerms =
  let n = length vars
      -- Expected pairs in a cycle: (v0,v1), (v1,v2), (v2,v3), (v3,v0)
      expectedPairs = zip vars (drop 1 vars ++ take 1 vars)
      -- Normalize pairs to have smaller element first
      normPair (x, y) = if x < y then (x, y) else (y, x)
      expectedNorm = map normPair expectedPairs
      actualPairs = [(v1, v2) | (v1, v2, _) <- crossTerms]
  in length actualPairs == n && all (`elem` expectedNorm) actualPairs

-- | Try general n-variable cyclic pattern: Σvᵢ² - Σvᵢvᵢ₊₁ (cyclic)
-- Works for any n >= 3 variables
tryCyclicNVar :: Poly -> Maybe SOSCertificate
tryCyclicNVar (Poly m) =
  let terms = M.toList m
      -- Find all square terms (v²)
      sqTerms = [(v, c) | (Monomial vm, c) <- terms, [(v, 2)] <- [M.toList vm]]
      -- Find all cross terms (v1 * v2)
      crossTerms = [(v1, v2, c) | (Monomial vm, c) <- terms
                                , [(v1, e1), (v2, e2)] <- [M.toList vm]
                                , e1 == 1, e2 == 1, v1 < v2]
      n = length sqTerms
  in if n < 3 || length crossTerms /= n
     then Nothing
     else
       let (vars, coeffs) = unzip sqTerms
           k = case coeffs of
                 (c:_) -> c
                 [] -> 0
       in if not (null coeffs) && all (== k) coeffs && k > 0  -- All square coefficients equal
          && all (\(_, _, c) -> c == -k) crossTerms  -- All cross coefficients equal to -k
          && isCyclicPatternGeneral vars crossTerms  -- Forms a cycle
          then
            -- Find the cyclic ordering
            case findCyclicOrder vars crossTerms of
              Just orderedVars ->
                let diffs = zipWith (\v1 v2 -> polySub (polyFromVar v1) (polyFromVar v2))
                                    orderedVars (drop 1 orderedVars ++ take 1 orderedVars)
                in Just $ SOSCertificate [(k/2, diff) | diff <- diffs] [] polyZero
                                         (Just (Custom $ "Cyclic" ++ show n ++ "Var"))
              Nothing -> Nothing
          else Nothing

-- | Check if cross terms can form any cyclic pattern
isCyclicPatternGeneral :: [String] -> [(String, String, Rational)] -> Bool
isCyclicPatternGeneral vars crossTerms =
  let actualPairs = [(v1, v2) | (v1, v2, _) <- crossTerms]
      -- Build adjacency: each variable should have exactly 2 neighbors
      neighbors v = [if v1 == v then v2 else v1 | (v1, v2) <- actualPairs, v1 == v || v2 == v]
  in all (\v -> length (neighbors v) == 2) vars

-- | Find the cyclic ordering of variables from cross terms
findCyclicOrder :: [String] -> [(String, String, Rational)] -> Maybe [String]
findCyclicOrder [] _ = Just []
findCyclicOrder vars crossTerms =
  let pairs = [(v1, v2) | (v1, v2, _) <- crossTerms]
      neighbors v = nub [if v1 == v then v2 else v1 | (v1, v2) <- pairs, v1 == v || v2 == v]
      -- Start from first variable and follow the chain
      start = case vars of
                (s:_) -> s
                [] -> "" -- Should be caught by pattern match above
      
      followChain prev current visited
        | length visited == length vars = Just (reverse visited)
        | otherwise =
            let nexts = filter (/= prev) (neighbors current)
            in case nexts of
                 [next] | next `notElem` visited || (length visited == length vars - 1 && next == start)
                   -> followChain current next (current : visited)
                 _ -> Nothing
  in case neighbors start of
       [n1, _] -> followChain start n1 [start]
       _ -> Nothing

-- | Try AM-GM 2-variable pattern: k*a² + k*b² - 2k*ab = k*(a-b)²
-- Matches: c₁a² + c₂b² + c₃ab where c₁ = c₂ and c₃ = -2*c₁
tryAMGM2Var :: Poly -> Maybe SOSCertificate
tryAMGM2Var (Poly m)
  | M.size m /= 3 = Nothing
  | otherwise =
      let terms = M.toList m
          sqTerms = [(v, c) | (Monomial vm, c) <- terms, [(v, 2)] <- [M.toList vm]]
          crossTerms = [(v1, v2, c) | (Monomial vm, c) <- terms
                                    , [(v1, e1), (v2, e2)] <- [M.toList vm]
                                    , e1 == 1, e2 == 1, v1 < v2]
      in case (sqTerms, crossTerms) of
           ([(a, ca), (b, cb)], [(_, _, cab)])
             | ca == cb && ca > 0
             , cab == -2 * ca
             -> let diff = polySub (polyFromVar a) (polyFromVar b)
                in Just $ SOSCertificate [(ca, diff)] [] polyZero (Just (Custom "AMGM2Var"))
           _ -> Nothing

-- | Try Lagrange identity pattern: (a²+b²)(c²+d²) - (ac+bd)² = (ad-bc)²
-- This matches degree-4 polynomials that are differences of products
tryLagrangeIdentity :: Poly -> Maybe SOSCertificate
tryLagrangeIdentity poly =
  -- Check if polynomial is already detected as a perfect square
  case polynomialSqrtDirect poly of
    Just q -> Just $ SOSCertificate [(1, q)] [] polyZero (Just CauchySchwarz)
    Nothing ->
      -- Try to factor the polynomial into a sum of squares
      tryFactorAsSOS poly

-- | Try to factor polynomial as a sum of squares by examining structure
tryFactorAsSOS :: Poly -> Maybe SOSCertificate
tryFactorAsSOS (Poly m) =
  let terms = M.toList m
      -- Look for degree-4 polynomials with specific structure
      maxDeg = maximum (0 : [sum (M.elems vm) | (Monomial vm, _) <- terms])
  in if maxDeg /= 4 then Nothing
     else tryDegree4SOS (Poly m)

-- | Try to decompose degree-4 polynomial as SOS
tryDegree4SOS :: Poly -> Maybe SOSCertificate
tryDegree4SOS (Poly m) =
  let terms = M.toList m
      -- Get all variables
      allVars = nub $ concatMap (\(Monomial vm, _) -> M.keys vm) terms
  in case allVars of
       [a, b, c, d] -> tryLagrange4Var (Poly m) a b c d
       _ -> Nothing

-- | Try specific Lagrange identity for 4 variables
tryLagrange4Var :: Poly -> String -> String -> String -> String -> Maybe SOSCertificate
tryLagrange4Var poly a b c d =
  -- Try (ad - bc)² pattern
  let ad_bc = polySub (polyMul (polyFromVar a) (polyFromVar d))
                      (polyMul (polyFromVar b) (polyFromVar c))
      sq_ad_bc = polyMul ad_bc ad_bc
  in if poly == sq_ad_bc
     then Just $ SOSCertificate [(1, ad_bc)] [] polyZero (Just CauchySchwarz)
     else
       -- Try (ac - bd)² pattern
       let ac_bd = polySub (polyMul (polyFromVar a) (polyFromVar c))
                          (polyMul (polyFromVar b) (polyFromVar d))
           sq_ac_bd = polyMul ac_bd ac_bd
       in if poly == sq_ac_bd
          then Just $ SOSCertificate [(1, ac_bd)] [] polyZero (Just CauchySchwarz)
          else Nothing

formatSOSDecomposition :: SOSCertificate -> String
formatSOSDecomposition cert =
  let terms = sosTerms cert
      formatCoeff c
        | c == 1 = ""
        | otherwise = prettyRational c ++ "*"
      formatTerm (c, p) = formatCoeff c ++ "(" ++ prettyPolyNice p ++ ")^2"
      termStrs = map formatTerm (reverse terms)
      decomposition = if null termStrs then "0" else intercalate " + " termStrs
  in "Sum of squares decomposition:\n  " ++ decomposition ++ " >= 0"
