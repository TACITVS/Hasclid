module Prover
  ( proveTheory
  , proveTheoryE
  , proveTheoryWithCache
  , proveTheoryWithOptions
  , buildSubMap
  , toPolySub
  , evaluatePoly
  , ProofTrace(..)
  , ProofStep(..)
  , ProofResult(..)
  , formatProofTrace
  , buchberger
  , subPoly
  , reduce
  , sPoly
  , IntSolveOptions(..)
  , IntSolveOutcome(..)
  , defaultIntSolveOptions
  , reasonOutcome
  , intSolve
  , intSat
  , proveExistentialConstructive
  , proveByInduction
  , intBoundsFromQ
  , promoteIntVars
  )
where

import Expr
import Error
import IntSolver
import Sturm (sturmSequence, rootsInInterval, samplePoints, evalPoly)
import Core.GB (buchbergerOptimized, reduce, sPoly)
import Positivity (checkPositivityEnhanced, PositivityResult(..), PositivityMethod(..), Confidence(..))
import Positivity.SDP (checkSOS_Constrained)
import Cache (GroebnerCache, lookupBasis, insertBasis)
import CADLift (proveFormulaCAD, satisfiableFormulaCAD, solveQuantifiedFormulaCAD)
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub, minimumBy, sort, findIndex, find)
import qualified Data.List as L
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
import Control.Exception (try, evaluate, ArithException(..), ArithException)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe, maybeToList, catMaybes, isJust, isNothing)
import Data.List (delete)
import Numeric.Natural (Natural)

-- =============================================
-- Proof Tracing Data Structures
-- =============================================

data ProofStep
  = UsedSubstitution String Expr                    -- Used variable substitution
  | UsedConstraint Formula                          -- Used constraint assumption
  | UsedLemma Formula                               -- Used proven lemma
  | ComputedGroebnerBasis Int                       -- Computed basis with N polynomials
  | ReducedToNormalForm Poly Poly                   -- Reduced polynomial: from -> to
  | CheckedPositivity String                        -- Checked positivity with method
  deriving (Show, Eq)

data ProofTrace = ProofTrace
  { steps :: [ProofStep]
  , usedAssumptions :: Theory
  , basisSize :: Int
  } deriving (Show, Eq)

-- | Result of a proof attempt (Either-based API)
data ProofResult = ProofResult
  { proved :: Bool          -- Was the formula proven?
  , reason :: String        -- Explanation or failure reason
  , trace :: ProofTrace     -- Proof trace (steps taken)
  } deriving (Show, Eq)

emptyTrace :: ProofTrace
emptyTrace = ProofTrace [] [] 0

formatProofTrace :: ProofTrace -> String
formatProofTrace trace = unlines $ 
  [ "\nGROEBNER BASIS ALGEBRAIC PROOF DEVELOPMENT:"
  , replicate 40 '='
  , "Used Assumptions (" ++ show (length (usedAssumptions trace)) ++ "):"
  ] ++ 
  [ "   * " ++ showFormula f | f <- usedAssumptions trace ] ++ 
  [ ""
  , "Proof Logic and Algebraic Steps:"
  ] ++ 
  [ "   " ++ show (i :: Int) ++ ". " ++ formatStep s | (i, s) <- zip [1..] (steps trace) ] ++ 
  [ ""
  , "Canonical Basis Size: " ++ show (basisSize trace) ++ " polynomials"
  , replicate 40 '='
  , "OVERALL RESULT: " ++ if provedCorrect trace then "[PROVED]" else "[NOT PROVED]"
  ]
  where
    provedCorrect t = any isComplete (steps t)
    isComplete (ReducedToNormalForm _ p) = p == polyZero
    isComplete _ = False

    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r
    showFormula (Le l r) = prettyExpr l ++ " <= " ++ prettyExpr r
    showFormula (Lt l r) = prettyExpr l ++ " < " ++ prettyExpr r
    showFormula (Divides l r) = prettyExpr l ++ " | " ++ prettyExpr r
    showFormula f        = prettyFormula f

    formatStep (UsedSubstitution v e) = "Applied variable substitution: " ++ v ++ " -> " ++ prettyExpr e
    formatStep (UsedConstraint f) = "Applied geometric constraint: " ++ prettyFormula f
    formatStep (UsedLemma f) = "Utilized proven geometric lemma: " ++ prettyFormula f
    formatStep (ComputedGroebnerBasis n) = "Constructed canonical Groebner basis using " ++ show n ++ " polynomials"
    formatStep (ReducedToNormalForm _ p2) =
      if p2 == polyZero
      then "Reduced expression to normal form: 0 (IDENTITY VERIFIED)"
      else "Reduced expression to normal form: " ++ prettyPoly p2 ++ " (NON-ZERO REMAINDER)"
    formatStep (CheckedPositivity method) = "Verified expression positivity via " ++ method

-- =============================================
-- Substitution Logic
-- =============================================

-- subPoly uses polyAdd/Neg
subPoly :: Poly -> Poly -> Poly
subPoly p1 p2 = polyAdd p1 (polyNeg p2)

-- =============================================
-- GROEBNER BASIS ENGINE (Buchberger's Algorithm)
-- =============================================

-- 3. Buchberger's Algorithm -> Uses Optimized Version
buchberger :: [Poly] -> [Poly]
buchberger = buchbergerOptimized

-- =============================================
-- Logic & Proof Engine
-- =============================================

partitionTheory :: Theory -> (Theory, Theory)
partitionTheory [] = ([], [])
partitionTheory (f:fs) =
  let (subs, constrs) = partitionTheory fs
  in case f of
       Eq (Var _) _ -> (f:subs, constrs)
       Eq (IntVar _) _ -> (f:subs, constrs)
       Eq _ _       -> (subs, f:constrs)
       _            -> (subs, constrs)

-- Substitute a map inside a formula (avoids capture for bound vars).
substFormula :: M.Map String Expr -> Formula -> Formula
substFormula sub (Eq l r) = Eq (substituteAll sub l) (substituteAll sub r)
substFormula sub (Ge l r) = Ge (substituteAll sub l) (substituteAll sub r)
substFormula sub (Gt l r) = Gt (substituteAll sub l) (substituteAll sub r)
substFormula sub (Le l r) = Le (substituteAll sub l) (substituteAll sub r)
substFormula sub (Lt l r) = Lt (substituteAll sub l) (substituteAll sub r)
substFormula sub (Divides l r) = Divides (substituteAll sub l) (substituteAll sub r)
substFormula sub (And f1 f2) = And (substFormula sub f1) (substFormula sub f2)
substFormula sub (Or f1 f2) = Or (substFormula sub f1) (substFormula sub f2)
substFormula sub (Not f) = Not (substFormula sub f)
substFormula sub (Forall qs f) =
  let blocked = foldr M.delete sub (map qvName qs)
  in Forall qs (substFormula blocked f)
substFormula sub (Exists qs f) =
  let blocked = foldr M.delete sub (map qvName qs)
  in Exists qs (substFormula blocked f)

-- Detect a linear equality and solve for the lexicographically largest variable.
solveLinearSingleVar :: Expr -> Expr -> Maybe (String, Expr)
solveLinearSingleVar l r =
  case linearCoeffs (toPolySub M.empty (Sub l r)) of
    Just (coeffs, c) | not (M.null coeffs) ->
      let (v, a) = M.findMax coeffs
          otherTerms = M.delete v coeffs
          numerator = L.foldl' (\acc (v', ai) -> Sub acc (Mul (Const ai) (Var v'))) (Const (-c)) (M.toList otherTerms)
          val = simplifyExpr (Div numerator (Const a))
      in Just (v, val)
    _ -> Nothing
  where
    linearCoeffs :: Poly -> Maybe (M.Map String Rational, Rational)
    linearCoeffs (Poly mp) =
      L.foldl' step (Just (M.empty, 0)) (M.toList mp)
      where
        step Nothing _ = Nothing
        step (Just (vs, c)) (Monomial mon, coeff) =
          case M.toList mon of 
            [] -> Just (vs, c + coeff)
            [(v,1)] -> Just (M.insertWith (+) v coeff vs, c)
            _ -> Nothing

-- Iteratively eliminates simple linear equalities to constants
preprocessSystem :: Theory -> Formula -> (Theory, Formula, [(String, Expr)])
preprocessSystem theory formula = go theory formula []
  where
    go th goal acc =
      case find substitutionCandidate th of
        Nothing -> (th, goal, reverse acc)
        Just f@(Eq l r) ->
          case solveLinearSingleVar l r of
            Nothing -> (th, goal, reverse acc)
            Just (v, val) ->
              let subMap = M.singleton v val
                  th' = map (substFormula subMap) (filter (/= f) th)
                  goal' = substFormula subMap goal
              in go th' goal' ((v, val) : acc)
        _ -> (th, goal, reverse acc)

    substitutionCandidate (Eq l r) = isJust (solveLinearSingleVar l r)
    substitutionCandidate _ = False

-- | Expand bounded integer quantifiers
expandFiniteDomain :: Formula -> Formula
expandFiniteDomain form = case form of
  Forall qs body -> expandQuantHelper Forall And (Eq (Const 1) (Const 1)) qs body
  Exists qs body -> expandQuantHelper Exists Or (Eq (Const 0) (Const 1)) qs body
  And f1 f2      -> And (expandFiniteDomain f1) (expandFiniteDomain f2)
  Or f1 f2       -> Or (expandFiniteDomain f1) (expandFiniteDomain f2)
  Not f          -> Not (expandFiniteDomain f)
  _              -> form

expandQuantHelper :: ([QuantVar] -> Formula -> Formula) 
                  -> (Formula -> Formula -> Formula) 
                  -> Formula 
                  -> [QuantVar] 
                  -> Formula 
                  -> Formula
expandQuantHelper constructor op emptyVal qs body =
  let (bounded, unbounded) = L.partition isBoundedConstantInt qs
      body' = expandFiniteDomain body
  in if null bounded
     then constructor unbounded body'
     else
       let expanded = expandVars op emptyVal bounded body'
           final = if null unbounded then expanded else constructor unbounded expanded
       in final

expandVars :: (Formula -> Formula -> Formula) -> Formula -> [QuantVar] -> Formula -> Formula
expandVars _ _ [] body = body
expandVars op emptyVal (q:qs) body =
  let v = qvName q
      lo = getConstInt (fromMaybe (IntConst 0) (qvLower q))
      hi = getConstInt (fromMaybe (IntConst 0) (qvUpper q))
  in if hi < lo
     then emptyVal
     else
        let expandedInner = expandVars op emptyVal qs body
            instances = [ substFormula (M.singleton v (IntConst i)) expandedInner | i <- [lo..hi] ]
        in foldr1 op instances

isBoundedConstantInt :: QuantVar -> Bool
isBoundedConstantInt (QuantVar _ QuantInt (Just l) (Just h)) =
  isConstInt l && isConstInt h && (getConstInt h - getConstInt l <= 100)
isBoundedConstantInt _ = False

isConstInt :: Expr -> Bool
isConstInt (IntConst _) = True
isConstInt (Const r) = denominator r == 1
isConstInt _ = False

getConstInt :: Expr -> Integer
getConstInt (IntConst i) = i
getConstInt (Const r) = numerator r
getConstInt _ = 0

-- Shared Groebner fallback
groebnerFallback
  :: ([Poly] -> [Poly])
  -> Maybe GroebnerCache
  -> Theory
  -> Formula
  -> (Bool, String, ProofTrace, Maybe GroebnerCache)
groebnerFallback customBuchberger maybeCache theory formula =
  let
    (theoryPrep, formulaPrep, subApplied) = preprocessSystem theory formula
    (substAssumptions, constraintAssumptions) = partitionTheory theoryPrep
    subM = buildSubMap substAssumptions

    substSteps = [ UsedSubstitution v e | Eq (Var v) e <- substAssumptions ]
    preSubSteps = [ UsedConstraint (Eq (Var v) e) | (v, e) <- subApplied ]
    constraintSteps = [ UsedConstraint f | f@(Eq _ _) <- constraintAssumptions ]

    idealGenerators = [ subPoly (toPolySub subM l) (toPolySub subM r)
                      | Eq l r <- constraintAssumptions ]

    (basis, updatedCache) =
      if null idealGenerators
      then ([], maybeCache)
      else case maybeCache of
             Nothing -> (customBuchberger idealGenerators, Nothing)
             Just cache ->
               let (maybeCached, cacheAfterLookup) = lookupBasis idealGenerators cache
               in case maybeCached of 
                    Just cachedBasis -> (cachedBasis, Just cacheAfterLookup)
                    Nothing ->
                      let computed = customBuchberger idealGenerators
                          cacheAfterInsert = insertBasis idealGenerators computed cacheAfterLookup
                      in (computed, Just cacheAfterInsert)

    basisStep = if null idealGenerators then [] else [ComputedGroebnerBasis (length basis)]
    (pL, pR) = case formulaPrep of
                 Eq l r -> (toPolySub subM l, toPolySub subM r)
                 Ge l r -> (toPolySub subM l, toPolySub subM r)
                 Gt l r -> (toPolySub subM l, toPolySub subM r)
                 Le l r -> (toPolySub subM r, toPolySub subM l)
                 Lt l r -> (toPolySub subM r, toPolySub subM l)
                 _      -> (polyZero, polyZero)

    difference = subPoly pL pR
    normalForm = reduce compare difference basis
    reductionStep = [ReducedToNormalForm difference normalForm]
    allSteps = substSteps ++ preSubSteps ++ constraintSteps ++ basisStep ++ reductionStep

    -- CRITICAL FIX: Reduce the theory (inequalities) using the Groebner Basis
    -- This ensures that constraints on eliminated variables are translated 
    -- to the remaining variables, allowing CAD to use them.
    reduceExpr :: Expr -> Expr
    reduceExpr e = 
       let p = toPolySub subM e
           p' = reduce compare p basis
       in polyToExpr p'

    reduceFormula :: Formula -> Formula
    reduceFormula f = case f of
      Ge l r -> Ge (reduceExpr l) (reduceExpr r)
      Gt l r -> Gt (reduceExpr l) (reduceExpr r)
      Le l r -> Le (reduceExpr l) (reduceExpr r)
      Lt l r -> Lt (reduceExpr l) (reduceExpr r)
      Eq l r -> Eq (reduceExpr l) (reduceExpr r)
      _ -> f

    reducedTheory = map reduceFormula theoryPrep

    checkPositivityWithFallback poly allowZero theory =
      let (posResult, posMsg) = checkPositivity poly allowZero
      in if posResult
         then (True, posMsg)
         else
           -- Try Numeric SDP with Constraints
           let constraints = extractInequalities theory
               sdpResult = checkSOS_Constrained poly constraints
           in if sdpResult
              then (True, "Verified Sum-of-Squares via Numeric SDP (Constrained)")
              else
               let goalExpr = unsafePerformIO $ do
                     r <- try (evaluate (polyToExpr poly)) :: IO (Either ArithException Expr)
                     case r of
                       Left e -> error ("CATCHED IN POLYTOEXPR: " ++ show e)
                       Right ex -> return ex
                   vars = S.toList (getVars poly)
                   relevantTheory = filter (\f -> all (`elem` vars) (varsInFormula f)) theory
                   goal = if allowZero then Ge goalExpr (Const 0) else Gt goalExpr (Const 0)
                   res = unsafePerformIO $ do
                     r <- try (evaluate (proveFormulaCAD relevantTheory goal)) :: IO (Either ArithException Bool)
                     case r of
                       Left e -> error ("CATCHED IN PROVER: " ++ show e)
                       Right b -> return b
               in if res
                  then (True, "Proved via CAD on Normal Form (Generic).")
                  else (False, posMsg)

  in case formulaPrep of
       Eq _ _ ->
         let result = normalForm == polyZero
             msg = if result
                   then "Equality Holds (Groebner Normal Form is 0)"
                   else "LHS /= RHS (Normal Form: " ++ prettyPoly normalForm ++ ")"
             trace = ProofTrace allSteps theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Ge _ _ ->
         let (result, msg) = checkPositivityWithFallback normalForm True reducedTheory
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Gt _ _ ->
         let (result, msg) = checkPositivityWithFallback normalForm False reducedTheory
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Le _ _ ->
         let (result, msg) = checkPositivityWithFallback normalForm True reducedTheory
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Lt _ _ ->
         let (result, msg) = checkPositivityWithFallback normalForm False reducedTheory
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       _ ->
         (False, "Goal not supported by available solvers.", ProofTrace allSteps theoryPrep (length basis), updatedCache)

-- | Prove a formula with optional cache support
proveTheoryWithCache :: Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithCache maybeCache theoryRaw formulaRaw =
  let theory = map expandFiniteDomain theoryRaw
      formula = expandFiniteDomain formulaRaw
      baseTrace = emptyTrace { usedAssumptions = theory }
  in case formula of
      And f1 f2 ->
        let (p1, r1, t1, c1) = proveTheoryWithCache maybeCache theory f1
        in if p1
           then let (p2, r2, t2, c2) = proveTheoryWithCache c1 theory f2
                in (p2, if p2 then "Both conjuncts proved: " ++ r1 ++ " AND " ++ r2 else "Failed on second conjunct: " ++ r2, t1 { steps = steps t1 ++ steps t2 }, c2)
           else (False, "Failed on first conjunct: " ++ r1, t1, c1)
      
      Or f1 f2 ->
        let (p1, r1, t1, c1) = proveTheoryWithCache maybeCache theory f1
        in if p1 
           then (True, "Proved first disjunct: " ++ r1, t1, c1)
           else let (p2, r2, t2, c2) = proveTheoryWithCache c1 theory f2
                in if p2
                   then (True, "Proved second disjunct: " ++ r2, t2, c2)
                   else (False, "Failed to prove either disjunct: (" ++ r1 ++ ") OR (" ++ r2 ++ ")", t1, c2)

      Not _ -> (False, "Top-level negation not supported yet.", baseTrace, maybeCache)

      Forall qs inner
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) -> 
            case proveBoundedRealsForall theory qs inner of
              Just (res, msg) -> (res, msg, baseTrace, maybeCache)
              Nothing -> 
                let hasIntForm = containsIntFormula inner || any containsIntFormula theory
                    hasDivForm = containsDivFormula inner || any containsDivFormula theory
                    hasSqrtForm = containsSqrtFormula inner || any containsSqrtFormula theory
                    polyOk = all isPolyFormula (inner : theory)
                in if hasIntForm || hasDivForm || hasSqrtForm || not polyOk
                                                     then (False, "Real universal not supported for non-polynomial or mixed goals.", baseTrace, maybeCache)
                                                     else 
                                                       case negateRealGoal inner of 
                                                         Nothing -> (False, "Real universal negation unsupported.", baseTrace, maybeCache)
                                                         Just negs -> 
                                                           let satAny = any (\ng -> satisfiableFormulaCAD theory ng) negs
                                                           in if satAny 
                                                                then (False, "Found counterexample branch for real universal.", baseTrace, maybeCache)
                                                                else (True, "Real universal proved by refuting negation.", baseTrace, maybeCache)

      Exists qs inner
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) -> 
            let hasIntForm = containsIntFormula inner || any containsIntFormula theory
                hasDivForm = containsDivFormula inner || any containsDivFormula theory
                hasSqrtForm = containsSqrtFormula inner || any containsSqrtFormula theory
                hasNestedQuant = containsQuantifier inner
                polyOk = all isPolyFormula (inner : theory)
            in if hasIntForm || hasDivForm || hasSqrtForm || not polyOk || hasNestedQuant
                 then fallThrough maybeCache theory formula baseTrace
                 else 
                   let (holds, msg) =
                         case traverse boundsFromQ qs of 
                           Nothing -> 
                             let ok = satisfiableFormulaCAD theory inner
                                 m = if ok 
                                       then "Real existential proved via CAD."
                                       else "Real existential refuted via CAD."
                             in (ok, m)
                           Just bnds -> 
                             let theoryWithBounds = theory ++ concat bnds
                                 ok = satisfiableFormulaCAD theoryWithBounds inner
                                 m = if ok 
                                       then "Real existential proved via CAD over bounding box."
                                       else "Real existential refuted via CAD."
                             in (ok, m)
                   in (holds, msg, baseTrace, maybeCache)

      Exists qs inner
        | all (\q -> qvType q == QuantInt) qs -> 
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                bounds = concatMap intBoundsFromQ qs
                boundsInt = map (promoteIntVars intNames) bounds
                outcome = intSat defaultIntSolveOptions (theoryInt ++ boundsInt ++ [innerInt])
                proved = intResult outcome == Just True
                msg = case intResult outcome of 
                        Just True  -> "Existential integer goal is satisfiable. " ++ reasonOutcome outcome True
                        Just False -> "Existential integer goal is unsatisfiable. " ++ reasonOutcome outcome False
                        Nothing    -> "Integer existential solver incomplete."
            in (proved, msg, baseTrace, maybeCache)

      Forall qs inner
        | all (\q -> qvType q == QuantInt) qs -> 
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                (decided, msg) = proveForallInt theoryInt innerInt
            in (decided, msg, baseTrace, maybeCache)

      Forall _ _ -> (False, "Unsupported universal quantifier.", baseTrace, maybeCache)

      _ -> fallThrough maybeCache theory formula baseTrace

fallThrough :: Maybe GroebnerCache -> Theory -> Formula -> ProofTrace -> (Bool, String, ProofTrace, Maybe GroebnerCache)
fallThrough maybeCache theory formula baseTrace =
  let hasInt = containsIntFormula formula || any containsIntFormula theory
      hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory

      tryCAD gMsg gTrace gCache =
          let cadProved = proveFormulaCAD theory formula
          in if cadProved
             then (True, "Proved via CAD (after Algebraic Solver failed).", gTrace, gCache)
             else (False, gMsg ++ " [CAD Failed]", gTrace, gCache)

  in if any containsQuantifier theory then
       (False, "Quantifiers in assumptions not supported yet.", baseTrace, maybeCache)
     else if hasInt then
       let outcome = intSolve defaultIntSolveOptions theory formula
       in case intResult outcome of
            Just True  -> (True, reasonOutcome outcome True, baseTrace, maybeCache)
            Just False -> (False, reasonOutcome outcome False, baseTrace, maybeCache)
            Nothing    -> 
              case formula of
                Eq _ _ -> 
                  let (gProved, gReason, gTrace, gCache) = groebnerFallback buchberger maybeCache theory formula
                  in if gProved 
                     then (True, "Proved by Algebraic Solver: " ++ gReason, gTrace, gCache)
                     else (False, "Int Solver Incomplete & Algebraic Solver failed: " ++ gReason, gTrace, gCache)
                _ -> (False, "Integer solver incomplete.", baseTrace, maybeCache)
     else if hasDiv then
       let (th', goal', _) = eliminateRational theory formula
           hasSqrt' = containsSqrtFormula goal' || any containsSqrtFormula th'
           (th'', goal'', _) = if hasSqrt' then eliminateSqrt th' goal' else (th', goal', M.empty)
       in proveTheoryWithCache maybeCache th'' goal''
     else if hasSqrt then
       let (th', goal', _) = eliminateSqrt theory formula
           proved = proveFormulaCAD th' goal'
           msg = if proved then "Proved via CAD with sqrt elimination." else "Not proved via CAD."
           trace = emptyTrace { usedAssumptions = th' }
       in (proved, msg, trace, maybeCache)
     else if containsQuantifier formula then
       let proved = solveQuantifiedFormulaCAD theory formula
           msg = if proved then "Proved by CAD (QE)." else "Refuted by CAD (QE)."
       in (proved, msg, baseTrace, maybeCache)
     else 
       let (gProved, gMsg, gTrace, gCache) = groebnerFallback buchberger maybeCache theory formula
       in if gProved
          then (True, gMsg, gTrace, gCache)
          else case formula of
                 Ge _ _ -> tryCAD gMsg gTrace gCache
                 Gt _ _ -> tryCAD gMsg gTrace gCache
                 Le _ _ -> tryCAD gMsg gTrace gCache
                 Lt _ _ -> tryCAD gMsg gTrace gCache
                 _      -> (False, gMsg, gTrace, gCache)

proveTheoryWithOptions :: ([Poly] -> [Poly]) -> Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithOptions customBuchberger maybeCache theoryRaw formulaRaw =
  let theory = map expandFiniteDomain theoryRaw
      formula = expandFiniteDomain formulaRaw
      baseTrace = emptyTrace { usedAssumptions = theory }
  in case formula of
      And f1 f2 ->
        let (p1, r1, t1, c1) = proveTheoryWithOptions customBuchberger maybeCache theory f1
        in if p1
           then let (p2, r2, t2, c2) = proveTheoryWithOptions customBuchberger c1 theory f2
                in (p2, if p2 then "Both conjuncts proved: " ++ r1 ++ " AND " ++ r2 else "Failed on second conjunct: " ++ r2, t1 { steps = steps t1 ++ steps t2 }, c2)
           else (False, "Failed on first conjunct: " ++ r1, t1, c1)
      
      Or f1 f2 ->
        let (p1, r1, t1, c1) = proveTheoryWithOptions customBuchberger maybeCache theory f1
        in if p1 
           then (True, "Proved first disjunct: " ++ r1, t1, c1)
           else let (p2, r2, t2, c2) = proveTheoryWithOptions customBuchberger c1 theory f2
                in if p2
                   then (True, "Proved second disjunct: " ++ r2, t2, c2)
                   else (False, "Failed to prove either disjunct.", t1, c2)

      _ -> fallThroughWithOptions customBuchberger maybeCache theory formula baseTrace

fallThroughWithOptions :: ([Poly] -> [Poly]) -> Maybe GroebnerCache -> Theory -> Formula -> ProofTrace -> (Bool, String, ProofTrace, Maybe GroebnerCache)
fallThroughWithOptions customBuchberger maybeCache theory formula _baseTrace =
  let hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory

      tryCAD gMsg gTrace gCache =
          let cadProved = proveFormulaCAD theory formula
          in if cadProved
             then (True, "Proved via CAD (after Algebraic Solver failed).", gTrace, gCache)
             else (False, gMsg ++ " [CAD Failed]", gTrace, gCache)

  in if hasDiv then
       let (th', goal', _) = eliminateRational theory formula
           hasSqrt' = containsSqrtFormula goal' || any containsSqrtFormula th'
           (th'', goal'', _) = if hasSqrt' then eliminateSqrt th' goal' else (th', goal', M.empty)
       in proveTheoryWithOptions customBuchberger maybeCache th'' goal''
     else if hasSqrt then
       let (th', goal', _) = eliminateSqrt theory formula
           proved = proveFormulaCAD th' goal'
           msg = if proved then "Proved via CAD with sqrt elimination." else "Not proved via CAD."
           trace = emptyTrace { usedAssumptions = th' }
       in (proved, msg, trace, maybeCache)
     else 
       let (gProved, gMsg, gTrace, gCache) = groebnerFallback customBuchberger maybeCache theory formula
       in if gProved
          then (True, gMsg, gTrace, gCache)
          else case formula of
                 Ge _ _ -> tryCAD gMsg gTrace gCache
                 Gt _ _ -> tryCAD gMsg gTrace gCache
                 Le _ _ -> tryCAD gMsg gTrace gCache
                 Lt _ _ -> tryCAD gMsg gTrace gCache
                 _      -> (False, gMsg, gTrace, gCache)

-- | Original proveTheory function (no caching)
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
proveTheory theory formula =
  let (isProved, reason, trace, _) = proveTheoryWithCache Nothing theory formula
  in (isProved, reason, trace)

-- | Either-based proveTheory
proveTheoryE :: Theory -> Formula -> Either ProverError ProofResult
proveTheoryE theory formula =
  let (isProved, reason, trace, _) = proveTheoryWithCache Nothing theory formula
  in Right $ ProofResult isProved reason trace

-- | Enhanced Positivity Checker
checkPositivity :: Poly -> Bool -> (Bool, String)
checkPositivity p allowZero =
  let result = checkPositivityEnhanced p allowZero
      confidenceStr = case confidence result of 
                        Proven -> "[PROVEN]"
                        HighConfidence -> "[HIGH CONFIDENCE]"
                        Heuristic -> "[HEURISTIC]"
      methodStr = show (method result)
  in (isPositive result, confidenceStr ++ " " ++ explanation result ++ " (Method: " ++ methodStr ++ ")")

-- Prove universal integer goals
proveForallInt :: Theory -> Formula -> (Bool, String)
proveForallInt theory goal =
  case negateIntGoal goal of
    Nothing -> (False, "Integer negation unsupported.")
    Just negs -> 
      let outcomes = [ intSat defaultIntSolveOptions (theory ++ [ng]) | ng <- negs ]
          anySat = any (\o -> intResult o == Just True) outcomes
          anyUnknown = any (\o -> intResult o == Nothing) outcomes
      in if anySat
           then (False, "Found counterexample.")
           else if anyUnknown
                  then (False, "Solver incomplete.")
                  else (True, "Integer universal proved.")

-- Bounded real universals
proveBoundedRealsForall :: Theory -> [QuantVar] -> Formula -> Maybe (Bool, String)
proveBoundedRealsForall theory qs goal
  | null qs = Nothing
  | any (isNothing . qvLower) qs || any (isNothing . qvUpper) qs = Nothing
  | [q] <- qs =
      case (qvLower q >>= rationalFromExpr, qvUpper q >>= rationalFromExpr) of
        (Just lo, Just hi) | lo <= hi ->
          if varInTheory (qvName q) theory then Nothing else
            case linearBoundsForVar (qvName q) goal of
              Just (coef, constTerm, strict) ->
                let minVal = if coef >= 0 then coef * lo + constTerm else coef * hi + constTerm
                in if strict
                     then Just (minVal > 0, if minVal > 0 then "Proved (linear > 0)" else "Counterexample.")
                     else Just (minVal >= 0, if minVal >= 0 then "Proved (linear >= 0)" else "Counterexample.")
              Nothing -> 
                case univariatePolyForVar (qvName q) goal of
                  Just (upoly, strict) -> proveUnivariatePolyForall lo hi upoly strict
                  Nothing -> Nothing
        _ -> Nothing
  | otherwise = Nothing

linearBoundsForVar :: String -> Formula -> Maybe (Rational, Rational, Bool)
linearBoundsForVar v (Ge l r) = linearCoeffs v (toPolySub M.empty (Sub l r)) False
linearBoundsForVar v (Gt l r) = linearCoeffs v (toPolySub M.empty (Sub l r)) True
linearBoundsForVar _ _ = Nothing

linearCoeffs :: String -> Poly -> Bool -> Maybe (Rational, Rational, Bool)
linearCoeffs v (Poly mp) strict =
  let terms = M.toList mp
      (aSum, cSum, ok) = L.foldl' (\(a,c,good) (mon, coeff) ->
                                  case mon of
                                    Monomial m -> 
                                      case M.toList m of 
                                        [] -> (a, c + coeff, good)
                                        [(v',1)] | v' == v -> (a + coeff, c, good)
                                        _ -> (a, c, False)
                                ) (0,0,True) terms
  in if ok then Just (aSum, cSum, strict) else Nothing

rationalFromExpr :: Expr -> Maybe Rational
rationalFromExpr (Const r) = Just r
rationalFromExpr (IntConst n) = Just (fromInteger n)
rationalFromExpr _ = Nothing

boundsFromQ :: QuantVar -> Maybe [Formula]
boundsFromQ q =
  case (qvLower q >>= rationalFromExpr, qvUpper q >>= rationalFromExpr) of
    (Just lo, Just hi) | lo <= hi -> 
      let v = Var (qvName q)
      in Just [Ge v (Const lo), Ge (Const hi) v]
    _ -> Nothing

intBoundsFromQ :: QuantVar -> [Formula]
intBoundsFromQ (QuantVar v QuantInt (Just lo) (Just hi)) =
  [Ge (IntVar v) lo, Le (IntVar v) hi]
intBoundsFromQ _ = []

isPolyFormula :: Formula -> Bool
isPolyFormula (Eq l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Ge l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Gt l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Le l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Lt l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (And f1 f2) = isPolyFormula f1 && isPolyFormula f2
isPolyFormula (Or f1 f2) = isPolyFormula f1 && isPolyFormula f2
isPolyFormula (Not f) = isPolyFormula f
isPolyFormula _ = False

polyToUPoly :: String -> Poly -> Maybe [Rational]
polyToUPoly v (Poly mp) =
  let addTerm acc (Monomial m, coeff) =
        case M.toList m of 
          [] -> Just (M.insertWith (+) 0 coeff acc)
          [(v', p)] | v' == v -> Just (M.insertWith (+) p coeff acc)
          _ -> Nothing
      coeffMap = foldM addTerm M.empty (M.toList mp)
  in case coeffMap of 
       Nothing -> Nothing
       Just cm -> 
         let maxPow = if M.null cm then 0 else maximum (M.keys cm)
         in Just [ M.findWithDefault 0 i cm | i <- [0..maxPow] ]

univariatePolyForVar :: String -> Formula -> Maybe ([Rational], Bool)
univariatePolyForVar v (Ge l r) =
  let p = toPolySub M.empty (Sub l r)
  in (,False) <$> polyToUPoly v p
univariatePolyForVar v (Gt l r) =
  let p = toPolySub M.empty (Sub l r)
  in (,True) <$> polyToUPoly v p
univariatePolyForVar _ _ = Nothing

proveUnivariatePolyForall :: Rational -> Rational -> [Rational] -> Bool -> Maybe (Bool, String)
proveUnivariatePolyForall lo hi upoly strict =
  let seq = sturmSequence upoly
      samplesRaw = filter (\x -> x >= lo && x <= hi) (samplePoints upoly)
      extra = if null samplesRaw then [(lo + hi) / 2] else []
      evals = map (evalPoly upoly) (samplesRaw ++ extra)
      endVals = [evalPoly upoly lo, evalPoly upoly hi]
      rootCount = rootsInInterval seq lo hi
      allVals = evals ++ endVals
  in if strict
       then if rootCount > 0 then Just (False, "Root in interval.")
            else let ok = all (> 0) allVals in Just (ok, if ok then "Proved." else "Counterexample.")
       else let ok = all (>= 0) allVals in Just (ok, if ok then "Proved." else "Counterexample.")

varInTheory :: String -> Theory -> Bool
varInTheory v = any (v `elem`) . map varsInFormula

varsInFormula :: Formula -> [String]
varsInFormula (Eq l r) = varsInExpr l ++ varsInExpr r
varsInFormula (Ge l r) = varsInExpr l ++ varsInExpr r
varsInFormula (Gt l r) = varsInExpr l ++ varsInExpr r
varsInFormula (And f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Or f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Not f) = varsInFormula f
varsInFormula _ = []

varsInExpr :: Expr -> [String]
varsInExpr (Var v) = [v]
varsInExpr (Add a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Sub a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Mul a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Div a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Pow e _) = varsInExpr e
varsInExpr (Sqrt e) = varsInExpr e
varsInExpr _ = []

negateIntGoal :: Formula -> Maybe [Formula]
negateIntGoal (Eq l r) = Just [Gt l r, Gt r l]
negateIntGoal (Ge l r) = Just [Gt r l]
negateIntGoal (Gt l r) = Just [Ge r l]
negateIntGoal _ = Nothing

negateRealGoal :: Formula -> Maybe [Formula]
negateRealGoal (Eq l r) = Just [Gt l r, Gt r l]
negateRealGoal (Ge l r) = Just [Gt r l]
negateRealGoal (Gt l r) = Just [Ge r l]
negateRealGoal _ = Nothing

promoteIntVars :: [String] -> Formula -> Formula
promoteIntVars names f = goF names f
  where
    goF ns (Eq l r) = Eq (goE ns l) (goE ns r)
    goF ns (Ge l r) = Ge (goE ns l) (goE ns r)
    goF ns (Gt l r) = Gt (goE ns l) (goE ns r)
    goF ns (And f1 f2) = And (goF ns f1) (goF ns f2)
    goF ns (Or f1 f2) = Or (goF ns f1) (goF ns f2)
    goF ns (Not f) = Not (goF ns f)
    goF _  other = other

    goE ns (Var v) | v `elem` ns = IntVar v
    goE ns (Add a b) = Add (goE ns a) (goE ns b)
    goE ns (Sub a b) = Sub (goE ns a) (goE ns b)
    goE ns (Mul a b) = Mul (goE ns a) (goE ns b)
    goE ns (Div a b) = Div (goE ns a) (goE ns b)
    goE ns (Pow e n) = Pow (goE ns e) n
    goE ns (Sqrt e) = Sqrt (goE ns e)
    goE _  other = other

proveExistentialConstructive :: Theory -> Formula -> (Bool, String, ProofTrace)
proveExistentialConstructive _theory _goal = (False, "Not implemented.", emptyTrace)

proveByInduction :: Theory -> Formula -> (Bool, String, ProofTrace)
proveByInduction _theory _formula = (False, "Not implemented.", emptyTrace)


extractInequalities :: Theory -> [Poly]
extractInequalities theory = 
  let subM = buildSubMap theory -- Reuse sub map logic? Or assume reduced?
      -- Reduced theory has simple forms.
      -- Convert Ge/Gt/Le/Lt to Poly >= 0
      toP (Ge l r) = Just (toPolySub M.empty (Sub l r))
      toP (Gt l r) = Just (toPolySub M.empty (Sub l r))
      toP (Le l r) = Just (toPolySub M.empty (Sub r l))
      toP (Lt l r) = Just (toPolySub M.empty (Sub r l))
      toP _ = Nothing
  in catMaybes (map toP theory)
