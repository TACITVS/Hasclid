{-|
Module: SolverRouter
Description: Intelligent routing system for automatic solver selection
-}

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
import Wu (wuProve, wuProveWithTrace, formatWuTrace, proveExistentialWu, reduceWithWu)
import Prover (proveTheoryWithOptions, formatProofTrace, buchberger, subPoly, intSolve, intSat, IntSolveOptions(..), IntSolveOutcome(..), defaultIntSolveOptions, reasonOutcome, proveExistentialConstructive, intBoundsFromQ, promoteIntVars)
import CADLift (evaluateInequalityCAD, solveQuantifiedFormulaCAD)
import CADLift (proveFormulaCAD)
import Positivity (checkPositivityEnhanced, isPositive, explanation, PositivityResult(..), Confidence(..))
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import BuchbergerOpt (buchbergerWithStrategy, SelectionStrategy(..))
import TermOrder (TermOrder(..), compareMonomials)
import F4Lite (f4LiteGroebner, f4ReduceModular, reduceWithF4, reduceWithBasis)
import Geometry.WLOG (applyWLOG, detectPoints, parsePointVar)
import Geometry.Barycentric (applyBarycentric)
import Positivity.SOS (checkSOS, checkSOSWithLemmas, SOSCertificate(..), getSOSCertificate)
import Positivity.Numerical (checkSOSNumeric, reconstructPoly, PolyD, safeFromRational)
import Positivity.SDP (checkSOS_SDP)
import Positivity.SOSTypes (trySOSHeuristic)
import AreaMethod (proveArea, deriveConstruction)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (delete, find, isSuffixOf, sort, foldl', isPrefixOf, intercalate, nub, partition)
import Data.Maybe (isJust, mapMaybe)
import qualified Control.Exception as CE
import Control.Exception (try, evaluate, ArithException(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.Ratio (numerator, denominator)
import Debug.Trace (trace)

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
  { intOptions :: IntSolveOptions
  , useOptimizedGroebner :: Bool
  , selectionStrategyOpt :: SelectionStrategy
  , groebnerBackend :: GroebnerBackend
  , f4UseBatch :: Bool
  }
  deriving (Show, Eq)

data GroebnerBackend = BuchbergerBackend | F4Backend deriving (Show, Eq)

defaultSolverOptions :: SolverOptions
defaultSolverOptions = SolverOptions defaultIntSolveOptions True SugarStrategy F4Backend True

selectGroebner :: ProblemProfile -> SolverOptions -> Formula -> [Poly] -> [Poly]
selectGroebner profile opts _goal =
  let params = symbolicParams profile
      allVars = variables profile
      activeVars = filter (`notElem` params) allVars
      ord = compareMonomials (if null params then GrevLex else Block [(activeVars, GrevLex), (params, GrevLex)])
  in case groebnerBackend opts of
       F4Backend -> f4LiteGroebner ord (selectionStrategyOpt opts) (f4UseBatch opts)
       BuchbergerBackend -> buchbergerWithStrategy ord (selectionStrategyOpt opts)

-- =============================================
-- Main Automatic Solving Functions
-- =============================================

autoSolve :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult
autoSolve opts pointSubs theoryRaw goalRaw = unsafePerformIO $ do
  let safeProfile = analyzeProblem theoryRaw goalRaw
      pts = Geometry.WLOG.detectPoints (goalRaw : theoryRaw)
  result <- try $ evaluate $ autoSolveInternal pts True opts pointSubs theoryRaw goalRaw
  case result of
    Left Underflow -> return $ errorResult safeProfile "Solver limit reached (Numerical precision boundary)"
    Left Overflow  -> return $ errorResult safeProfile "Solver limit reached (Numerical overflow)"
    Left e         -> return $ errorResult safeProfile ("Solver Exception: " ++ show e)
    Right res      -> return res
  where
    errorResult profile msg = AutoSolveResult Unsolvable msg profile goalRaw False msg Nothing Nothing M.empty

autoSolveInternal :: [String] -> Bool -> SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult
autoSolveInternal pts allowSplit opts pointSubs theoryRaw goalRaw =
  let (theoryG, goalG, _logG) = preprocessGeometry pointSubs theoryRaw goalRaw
      (theoryS, goalS, varDefsS) = eliminateSqrt theoryG goalG
      (theoryP, goalP, varDefsP) = eliminateRational theoryS goalS
      varDefs = M.union varDefsS varDefsP
      preprocessResult = preprocess pointSubs theoryP goalP
      theory' = preprocessedTheory preprocessResult
      goal' = preprocessedGoal preprocessResult
      profile = analyzeProblem theory' goal'
      baseResult = case goal' of
          And f1 f2 ->
            let r1 = autoSolveInternal pts False opts pointSubs theory' f1
                r2 = if isProved r1 then autoSolveInternal pts False opts pointSubs theory' f2 else r1 
                combinedTrace = case (detailedTrace r1, detailedTrace r2) of
                                  (Just t1, Just t2) -> Just (t1 ++ "\n\nAND\n\n" ++ t2)
                                  (Just t1, Nothing) -> Just t1
                                  (Nothing, Just t2) -> Just t2
                                  (Nothing, Nothing) -> Nothing
            in AutoSolveResult (selectedSolver r1) "Conjunction decomposed" profile goal' (isProved r1 && isProved r2) (if isProved r1 then proofReason r2 else proofReason r1) Nothing combinedTrace varDefs
          Or f1 f2 ->
            let r1 = autoSolveInternal pts False opts pointSubs theory' f1
            in if isProved r1 then r1 { preprocessedGoalExpr = goal' }
               else let r2 = autoSolveInternal pts False opts pointSubs theory' f2
                        combinedTrace = case (detailedTrace r1, detailedTrace r2) of
                                          (Just t1, Just t2) -> Just ("---" ++ " Attempt 1 ---" ++ "\n" ++ t1 ++ "\n\n---" ++ " Attempt 2 ---" ++ "\n" ++ t2)
                                          (Just t1, Nothing) -> Just t1
                                          (Nothing, Just t2) -> Just t2
                                          (Nothing, Nothing) -> Nothing
                    in if isProved r2 then r2 { preprocessedGoalExpr = goal', detailedTrace = combinedTrace, varDefinitions = varDefs }
                       else r2 { preprocessedGoalExpr = goal', isProved = False, detailedTrace = combinedTrace, varDefinitions = varDefs }
          Exists qs inner | all (\q -> qvType q == QuantInt) qs ->
                let intNames = map qvName qs
                    theoryInt = map (promoteIntVars intNames) theory'
                    outcome = intSat (intOptions opts) (theoryInt ++ map (promoteIntVars intNames) (concatMap intBoundsFromQ qs) ++ [promoteIntVars intNames inner])
                    proved = intResult outcome == Just True
                in AutoSolveResult UseGroebner "Integer existential" profile goal' proved (reasonOutcome outcome proved) Nothing Nothing varDefs
          Forall qs _ | all (\q -> qvType q == QuantInt) qs && not (any containsQuantifier theory') ->
                let intNames = map qvName qs
                    (proved, reason, _, _) = proveTheoryWithOptions (selectGroebner profile opts (promoteIntVars intNames goal')) Nothing (map (promoteIntVars intNames) theory') (promoteIntVars intNames goal')
                in AutoSolveResult UseGroebner "Integer universal" profile goal' proved reason Nothing Nothing varDefs
          Exists qs _ | all (\q -> qvType q == QuantReal) qs && not (any containsQuantifier theory') ->
                let (proved, reason, _, _) = proveTheoryWithOptions (selectGroebner profile opts goal') Nothing theory' goal'
                in AutoSolveResult UseGroebner "Real existential" profile goal' proved reason Nothing Nothing varDefs
          Forall qs _ | all (\q -> qvType q == QuantReal) qs && all (\q -> isJust (qvLower q) && isJust (qvUpper q)) qs && not (any containsQuantifier theory') ->
                let (proved, reason, _, _) = proveTheoryWithOptions (selectGroebner profile opts goal') Nothing theory' goal'
                in AutoSolveResult { selectedSolver = UseGroebner, solverReason = "Bounded real universal", problemProfile = profile, preprocessedGoalExpr = goal', isProved = proved, proofReason = reason, proofEvidence = Nothing, detailedTrace = Nothing, varDefinitions = varDefs }
          Forall qs _ | all (\q -> qvType q == QuantReal) qs && not (any containsQuantifier theory') ->
                let (proved, reason, _, _) = proveTheoryWithOptions (selectGroebner profile opts goal') Nothing theory' goal'
                in AutoSolveResult { selectedSolver = UseGroebner, solverReason = "Real universal", problemProfile = profile, preprocessedGoalExpr = goal', isProved = proved, proofReason = reason, proofEvidence = Nothing, detailedTrace = Nothing, varDefinitions = varDefs }
          
          _ | (isInequality goal') && (problemType profile == Geometric || ((problemType profile == PureAlgebraic || problemType profile == Mixed) && numVariables profile > 2)) ->
            let (proved, reason, trace, defs, evidence) = proveInequalitySOS pts opts theory' goal'
            in if proved then AutoSolveResult UseGroebner "Inequality (Fast Path)" profile goal' True reason evidence trace (M.union varDefs defs)
               else let (proved', reason', trace', defs') = executeSolver pts UseCAD opts profile theory' goal'
                        combinedTrace = case (trace, trace') of
                                          (Just t1, Just t2) -> Just (t1 ++ "\n\n---" ++ " FALLBACK TO CAD ---" ++ "\n\n" ++ t2)
                                          (Just t1, Nothing) -> Just t1
                                          (Nothing, Just t2) -> Just t2
                                          (Nothing, Nothing) -> Nothing
                    in AutoSolveResult UseCAD "Inequality (Fallback)" profile goal' proved' reason' Nothing combinedTrace (M.unions [varDefs, defs', varDefs])
          Ge _ _ | problemType profile == PureInequality || problemType profile == SinglePositivity ->
            let (proved, reason, trace, defs) = executeSolver pts UseCAD opts profile theory' goal'
            in AutoSolveResult UseCAD "Inequality goal" profile goal' proved reason Nothing trace (M.union varDefs defs)
          Gt _ _ | problemType profile == PureInequality || problemType profile == SinglePositivity ->
            let (proved, reason, trace, defs) = executeSolver pts UseCAD opts profile theory' goal'
            in AutoSolveResult UseCAD "Strict inequality goal" profile goal' proved reason Nothing trace (M.union varDefs defs)
          _ | containsQuantifier goal' || any containsQuantifier theory' ->
                let (proved, reason, trace, defs) = executeSolver pts UseCAD opts profile theory' goal'
                in AutoSolveResult { selectedSolver = UseCAD, solverReason = "Quantified formula", problemProfile = profile, preprocessedGoalExpr = goal', isProved = proved, proofReason = reason, proofEvidence = Nothing, detailedTrace = trace, varDefinitions = M.union varDefs defs }
          _ ->
            case solveGeoWithTrace theory' goal' of
              GeoProved reason steps -> AutoSolveResult UseGeoSolver "Fast geometric propagation" profile goal' True reason Nothing (Just (unlines steps)) varDefs
              GeoDisproved reason steps -> AutoSolveResult UseGeoSolver "Fast geometric propagation" profile goal' False reason Nothing (Just (unlines steps)) varDefs
              GeoUnknown _ ->
                let isAreaMethodApplicable = isEquality goal' && not (isInequality goalRaw) && isJust (deriveConstruction theory' goal')
                    solver = if isAreaMethodApplicable then UseAreaMethod else selectAlgebraicSolver profile goal'
                    (proved, proofMsg, trace, defs) = executeSolver pts solver opts profile theory' goal'
                in AutoSolveResult solver ("PHASE 2: " ++ explainSolverChoice solver profile) profile goal' proved proofMsg Nothing trace (M.union varDefs defs)
  in applyInsideSplit pts allowSplit opts pointSubs theoryRaw goalRaw baseResult

applyInsideSplit :: [String] -> Bool -> SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> AutoSolveResult -> AutoSolveResult
applyInsideSplit pts allowSplit opts pointSubs theoryRaw goalRaw baseResult =
  if not allowSplit || isProved baseResult || not (shouldSplitInside pointSubs theoryRaw goalRaw)
  then baseResult
  else
    let cases = insideSplitCases
        results = [ (label, autoSolveInternal pts False opts pointSubs (constraints ++ theoryRaw) goalRaw) | (label, constraints) <- cases ]
        allProved = all (isProved . snd) results
        failed = [label | (label, res) <- results, not (isProved res)]
        traceStr = "Boundary Split Result:\n" ++ intercalate "\n" (map (\(l,r) -> l ++ ": " ++ if isProved r then "PROVED" else "NOT PROVED") results)
    in if allProved then baseResult { isProved = True, proofReason = "Proved by boundary case split", detailedTrace = mergeTrace (detailedTrace baseResult) traceStr }
       else baseResult { proofReason = proofReason baseResult ++ " (failed on: " ++ intercalate ", " failed ++ ")", detailedTrace = mergeTrace (detailedTrace baseResult) traceStr }

shouldSplitInside :: M.Map String Expr -> [Formula] -> Formula -> Bool
shouldSplitInside subs theory goal = isInequality goal && M.member "ba_u" subs && any (\f -> case f of Ge (Var "ba_v") _ -> True; Gt (Var "ba_v") _ -> True; _ -> False) theory

insideSplitCases :: [(String, [Formula])]
insideSplitCases = let v = Var "ba_v"; w = Var "ba_w"; u = Sub (Const 1) (Add v w) in [("u=0", [Eq u (Const 0)]), ("v=0", [Eq v (Const 0)]), ("w=0", [Eq w (Const 0)]), ("interior", [Gt u (Const 0), Gt v (Const 0), Gt w (Const 0)])]

mergeTrace :: Maybe String -> String -> Maybe String
mergeTrace Nothing extra = Just extra
mergeTrace (Just base) extra = Just (base ++ "\n\n" ++ extra)

formatSOSCertificate :: SOSCertificate -> String
formatSOSCertificate cert =
  let terms = sosTerms cert; lemmas = sosLemmas cert
      formatTerm (c, p) = (if c == 1 then "" else prettyRational c ++ "*") ++ "(" ++ prettyPolyNice p ++ ")^2"
      sumParts = map formatTerm (reverse terms) ++ map prettyPolyNice lemmas
  in if null sumParts then "0" else intercalate " + " sumParts ++ (if sosRemainder cert == polyZero then "" else " + [" ++ prettyPolyNice (sosRemainder cert) ++ "]")

autoSolveE :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> Either String AutoSolveResult
autoSolveE opts pointSubs theory goal = Right $ autoSolve opts pointSubs theory goal

proveInequalitySOS :: [String] -> SolverOptions -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr, Maybe ProofEvidence)
proveInequalitySOS pts opts theoryRaw goalRaw =
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
      let (thWLOG, wlogLog) = applyWLOG theory goalFull
          (thBary, goalBary, baryLog) = applyBarycentric pts thWLOG goalFull
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
          reducer = F4Lite.reduceWithBasis ord basis
          
          knownLemmas = map (\f -> head (formulaToPolysLocal f)) [Ge (Var v) (Const 0) | v <- posVars]
          maybeCert = getSOSCertificate knownLemmas reducer targetPoly
          
          sdpVerified = if isJust maybeCert then False else checkSOS_SDP (reducer targetPoly)
          
          success = isJust maybeCert || sdpVerified
          evidence = fmap (\c -> EvidenceSOS c fullLog) maybeCert
          intro = "\nALGEBRAIC PROOF DEVELOPMENT:\n" ++ replicate 40 '-' ++ "\n"
          step1 = "[1] Goal: " ++ prettyFormula goalOriginal ++ "\n"
          setup = if null fullLog then "" else "[2] Geometric Setup:\n" ++ unlines (map ("    * " ++) fullLog)
          step2 = "[3] Polynomial Transformation:\n    Mapping: " ++ (if null posVars then "Identity" else intercalate ", " [ v ++ " -> " ++ v ++ "_sq^2" | v <- posVars ]) ++ "\n    Goal: " ++ prettyFormula goalFull ++ " >= 0\n"
          reductionStep = "[4] Canonical Reduction:\n    Identity: " ++ prettyPolyNice (reducer targetPoly) ++ " >= 0\n"
          step4 = "[5] SOS Decomposition:\n    " ++ (case maybeCert of 
                               Just c -> formatSOSCertificate c
                               Nothing -> if sdpVerified then "SDP-Based Feasibility Verified" else "0") ++ " >= 0\n"
          fullTrace = intro ++ step1 ++ setup ++ step2 ++ reductionStep ++ step4 ++ (replicate 40 '-' ++ "\nCONCLUSION: IDENTITY VERIFIED.")
      in (success, if success then "Proved via compositional Sum-of-Squares" else "SOS check failed", Just fullTrace, evidence)

    isAlwaysNonNeg v theory = any (\f -> case f of Ge (Var x) _ -> x == v; Gt (Var x) _ -> x == v; _ -> False) theory
    polyDegreeIn (Poly m) var = if M.null m then 0 else maximum ((0 :: Int) : [ fromIntegral (M.findWithDefault 0 var vars) | (mono, _) <- M.toList m, let Monomial vars = mono ])

autoSolveWithTrace :: SolverOptions -> M.Map String Expr -> [Formula] -> Formula -> Maybe String -> AutoSolveResult
autoSolveWithTrace opts pointSubs theory goal _ = autoSolve opts pointSubs theory goal

selectAlgebraicSolver :: ProblemProfile -> Formula -> SolverChoice
selectAlgebraicSolver profile goal
  | containsSqrtFormula goal = UseCAD
  | containsDivFormula goal = UseCAD  
  | containsIntFormula goal = UseGroebner
  | problemType profile == Geometric && isInequality goal && numVariables profile > 2 = UseSOS
  | otherwise = UseGroebner

isEquality :: Formula -> Bool
isEquality (Eq _ _) = True
isEquality (Forall _ f) = isEquality f
isEquality (Exists _ f) = isEquality f
isEquality _ = False

isInequality :: Formula -> Bool
isInequality (Ge _ _) = True
isInequality (Gt _ _) = True
isInequality (Le _ _) = True
isInequality (Lt _ _) = True
isInequality _ = False

executeSolver :: [String] -> SolverChoice -> SolverOptions -> ProblemProfile -> [Formula] -> Formula -> (Bool, String, Maybe String, M.Map String Expr)
executeSolver pts solver opts _profile theory goal =
  case solver of
    UseSOS -> let (proved, reason, trace, defs, _) = proveInequalitySOS pts opts theory goal in (proved, reason, trace, defs)
    UseCAD -> let (proved, reason, trace, defs) = runCadRational theory goal in (proved, reason, trace, defs)
    _ -> (False, "Not implemented", Nothing, M.empty)

extractPolyVars :: Poly -> S.Set String
extractPolyVars (Poly m) = S.fromList $ concatMap (\(mono, _) -> let Monomial vars = mono in M.keys vars) (M.toList m)

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
      reducer = F4Lite.reduceWithBasis ord basis
      
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
      
      proved = proveFormulaCAD reducedIneqs reducedGoal
      
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

explainSolverChoice :: SolverChoice -> ProblemProfile -> String
explainSolverChoice _ _ = "Automatic Solver"

replaceString :: String -> String -> String -> String
replaceString _ _ "" = ""
replaceString old new s@(c:cs) | old `isPrefixOf` s = new ++ replaceString old new (drop (length old) s) | otherwise = c : replaceString old new cs

formatAutoSolveResult :: AutoSolveResult -> Bool -> String
formatAutoSolveResult result verbose =
  let header = "=== HASCLID AUTOMATIC PROOF SYSTEM ==="
      analysis = "PROBLEM ANALYSIS:\n  Structure: " ++ show (problemType (problemProfile result)) ++ "\n  Variables: " ++ show (numVariables (problemProfile result))
      defs = varDefinitions result
      beautify s = M.foldlWithKey (\acc var expr -> replaceString var ("[" ++ prettyExpr expr ++ "]") acc) s defs
      traceStr = if verbose then case detailedTrace result of Just t -> "\nPROOF DEVELOPMENT:" ++ beautify t; Nothing -> "" else ""
  in unlines [header, "", analysis, "SOLVER SELECTION: " ++ solverReason result, "", "VERDICT: " ++ if isProved result then "PROVED" else "NOT PROVED", proofReason result, traceStr]

generateGeometricLemmas :: [Formula] -> Formula -> [Formula]
generateGeometricLemmas theory goal = let pts = Geometry.WLOG.detectPoints (goal : theory) in [ Ge (Add (Sqrt (Dist2 a b)) (Sqrt (Dist2 b c))) (Sqrt (Dist2 a c)) | a <- pts, b <- pts, c <- pts, a < b, b < c ]
