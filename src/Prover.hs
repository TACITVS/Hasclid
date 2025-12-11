{-# LANGUAGE DeriveGeneric #-}

module Prover
  ( proveTheory
  , proveTheoryWithCache
  , proveTheoryWithOptions
  , buildSubMap
  , toPolySub
  , evaluatePoly
  , ProofTrace(..)
  , ProofStep(..)
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
  ) where

import Expr
import Sturm (sturmSequence, rootsInInterval, samplePoints, evalPoly)
import Core.GB (buchbergerOptimized, reduce, sPoly)
import Positivity (checkPositivityEnhanced, PositivityResult(..), PositivityMethod(..), Confidence(..))
import Cache (GroebnerCache, lookupBasis, insertBasis)
import CADLift (proveFormulaCAD, satisfiableFormulaCAD, solveQuantifiedFormulaCAD)
import SqrtElim (eliminateSqrt)
import RationalElim (eliminateRational)
import qualified Data.Map.Strict as M
import Data.List (nub, minimumBy, sort, findIndex, find)
import qualified Data.List as L
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
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

emptyTrace :: ProofTrace
emptyTrace = ProofTrace [] [] 0

-- Format proof trace for display
formatProofTrace :: ProofTrace -> String
formatProofTrace trace = unlines $
  [ "=========================================="
  , "PROOF EXPLANATION:"
  , "=========================================="
  , ""
  , "Used Assumptions (" ++ show (length (usedAssumptions trace)) ++ "):"
  ] ++
  [ "  * " ++ showFormula f | f <- usedAssumptions trace ] ++
  [ ""
  , "Proof Steps:"
  ] ++
  [ "  " ++ show (i :: Int) ++ ". " ++ formatStep s | (i, s) <- zip [1..] (steps trace) ] ++
  [ ""
  , "Groebner Basis Size: " ++ show (basisSize trace)
  , "=========================================="
  ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r
    showFormula (Le l r) = prettyExpr l ++ " <= " ++ prettyExpr r
    showFormula (Lt l r) = prettyExpr l ++ " < " ++ prettyExpr r
    showFormula f        = prettyFormula f

    formatStep (UsedSubstitution v e) = "Used substitution: " ++ v ++ " -> " ++ prettyExpr e
    formatStep (UsedConstraint f) = "Applied constraint: " ++ prettyFormula f
    formatStep (UsedLemma f) = "Applied lemma: " ++ prettyFormula f
    formatStep (ComputedGroebnerBasis n) = "Computed Groebner basis (" ++ show n ++ " polynomials)"
    formatStep (ReducedToNormalForm _ p2) =
      if p2 == polyZero
      then "Reduced to normal form: 0 (PROOF COMPLETE)"
      else "Reduced to normal form: " ++ prettyPoly p2
    formatStep (CheckedPositivity method) = "Checked positivity using: " ++ method

-- =============================================
-- Substitution Logic (Now in Expr.hs)
-- =============================================

-- subPoly uses polyAdd/Neg
subPoly :: Poly -> Poly -> Poly
subPoly p1 p2 = polyAdd p1 (polyNeg p2)

-- =============================================
-- GROEBNER BASIS ENGINE (Buchberger's Algorithm)
-- =============================================

-- 1. Multivariate Polynomial Reduction (Division) -> Imported from BuchbergerOpt
-- 2. S-Polynomial -> Imported from BuchbergerOpt

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
       Eq _ _       -> (subs, f:constrs)
       _            -> (subs, constrs)

-- =============================================
-- Lightweight preprocessing for Groebner goals
-- =============================================

-- Substitute a map inside a formula (avoids capture for bound vars).
substFormula :: M.Map String Expr -> Formula -> Formula
substFormula sub (Eq l r) = Eq (substituteAll sub l) (substituteAll sub r)
substFormula sub (Ge l r) = Ge (substituteAll sub l) (substituteAll sub r)
substFormula sub (Gt l r) = Gt (substituteAll sub l) (substituteAll sub r)
substFormula sub (Le l r) = Le (substituteAll sub l) (substituteAll sub r)
substFormula sub (Lt l r) = Lt (substituteAll sub l) (substituteAll sub r)
substFormula sub (And f1 f2) = And (substFormula sub f1) (substFormula sub f2)
substFormula sub (Or f1 f2) = Or (substFormula sub f1) (substFormula sub f2)
substFormula sub (Not f) = Not (substFormula sub f)
substFormula sub (Forall qs f) =
  let blocked = foldr M.delete sub (map qvName qs)
  in Forall qs (substFormula blocked f)
substFormula sub (Exists qs f) =
  let blocked = foldr M.delete sub (map qvName qs)
  in Exists qs (substFormula blocked f)

-- Detect a single-variable linear equality and solve for that variable.
-- Returns (variable, value) where value is a constant expression.
solveLinearSingleVar :: Expr -> Expr -> Maybe (String, Expr)
solveLinearSingleVar l r =
  case linearSingleVar (toPolySub M.empty (Sub l r)) of
    Just (v, a, c) | a /= 0 -> Just (v, Const ((-c) / a))
    _ -> Nothing
  where
    linearSingleVar :: Poly -> Maybe (String, Rational, Rational)
    linearSingleVar (Poly mp) =
      case foldl step (Just (Nothing, 0, 0)) (M.toList mp) of
        Just (Just v, aSum, cSum) -> Just (v, aSum, cSum)
        _ -> Nothing
      where
        step Nothing _ = Nothing
        step (Just (mv, a, c)) (Monomial mon, coeff) =
          case M.toList mon of
            [] -> Just (mv, a, c + coeff)
            [(v,1)] ->
              case mv of
                Nothing   -> Just (Just v, a + coeff, c)
                Just vOld ->
                  if vOld == v
                    then Just (mv, a + coeff, c)
                    else Nothing
            _ -> Nothing

-- Iteratively eliminates simple linear equalities to constants,
-- applying substitutions to both theory and goal.
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

-- Shared Groebner fallback used by both cached and custom Buchberger flows.
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
                 Le l r -> (toPolySub subM r, toPolySub subM l)  -- Flip: l <= r becomes r >= l
                 Lt l r -> (toPolySub subM r, toPolySub subM l)  -- Flip: l < r becomes r > l
                 _      -> (polyZero, polyZero)

    difference = subPoly pL pR
    normalForm = reduce compare difference basis  -- Use Lex order for backwards compatibility
    reductionStep = [ReducedToNormalForm difference normalForm]
    allSteps = substSteps ++ preSubSteps ++ constraintSteps ++ basisStep ++ reductionStep
  in case formulaPrep of
       Eq _ _ ->
         let result = normalForm == polyZero
             msg = if result
                   then "Equality Holds (Groebner Normal Form is 0)"
                   else "LHS /= RHS (Normal Form: " ++ prettyPoly normalForm ++ ")"
             trace = ProofTrace allSteps theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Ge _ _ ->
         let (result, msg) = checkPositivity normalForm True
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Gt _ _ ->
         let (result, msg) = checkPositivity normalForm False
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Le _ _ ->
         let (result, msg) = checkPositivity normalForm True  -- l <= r becomes r >= l
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       Lt _ _ ->
         let (result, msg) = checkPositivity normalForm False  -- l < r becomes r > l
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theoryPrep (length basis)
         in (result, msg, trace, updatedCache)

       _ ->
         (False, "Goal not supported by available solvers.", ProofTrace allSteps theoryPrep (length basis), updatedCache)

-- | Prove a formula with optional cache support
-- Returns: (isProved, reason, trace, updatedCache)
proveTheoryWithCache :: Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithCache maybeCache theory formula =
  let baseTrace = emptyTrace { usedAssumptions = theory }
      hasInt = containsIntFormula formula || any containsIntFormula theory
      hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory
  in
    case formula of
      Forall qs inner
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) ->
            case proveBoundedRealsForall theory qs inner of
              Just (res, msg) -> (res, msg, baseTrace, maybeCache)
              Nothing ->
                let hasIntForm = containsIntFormula inner || any containsIntFormula theory
                    hasSqrtForm = containsSqrtFormula inner || any containsSqrtFormula theory
                    polyOk = all isPolyFormula (inner : theory)
                    hasDivForm = containsDivFormula inner || any containsDivFormula theory
                in if hasIntForm || hasDivForm || hasSqrtForm || not polyOk
                                                     then (False, "Real universal (unbounded) not supported for non-polynomial or mixed-domain goals.", baseTrace, maybeCache)
                                                     else
                                                       case negateRealGoal inner of
                                                         Nothing -> (False, "Real universal negation unsupported for this goal.", baseTrace, maybeCache)
                                                         Just negs ->
                                                           let satAny = any (\ng -> satisfiableFormulaCAD theory ng) negs
                                                           in if satAny
                                                                then (False, "Found counterexample branch for real universal (CAD satisfiable).", baseTrace, maybeCache)
                                                                else (True, "Real universal proved by refuting negation with CAD.", baseTrace, maybeCache)

      Exists qs inner
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) ->
            let hasIntForm = containsIntFormula inner || any containsIntFormula theory
                hasDivForm = containsDivFormula inner || any containsDivFormula theory
                hasSqrtForm = containsSqrtFormula inner || any containsSqrtFormula theory
                hasNestedQuant = containsQuantifier inner
                polyOk = all isPolyFormula (inner : theory)
            in if hasIntForm || hasDivForm || hasSqrtForm || not polyOk || hasNestedQuant
                 then fallThrough baseTrace hasInt hasDiv hasSqrt
                 else
                   let (holds, msg) =
                         case traverse boundsFromQ qs of
                           Nothing ->
                             let ok = satisfiableFormulaCAD theory inner
                                 m = if ok
                                       then "Real existential proved satisfiable via CAD."
                                       else "Real existential refuted via CAD (no satisfying cell)."
                             in (ok, m)
                           Just bnds ->
                             let theoryWithBounds = theory ++ concat bnds
                                 ok = satisfiableFormulaCAD theoryWithBounds inner
                                 m = if ok
                                       then "Real existential proved satisfiable via CAD over bounding box."
                                       else "Real existential refuted via CAD within bounds."
                             in (ok, m)
                   in (holds, msg, baseTrace, maybeCache)
      Exists qs inner
        | all (\q -> qvType q == QuantInt) qs ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                outcome = intSat defaultIntSolveOptions (theoryInt ++ [innerInt])
                proved = intResult outcome == Just True
                msg = case intResult outcome of
                        Just True  -> "Existential integer goal is satisfiable. " ++ reasonOutcome outcome True
                        Just False -> "Existential integer goal is unsatisfiable. " ++ reasonOutcome outcome False
                        Nothing    -> "Integer existential parsed but solver is incomplete for this goal."
            in (proved, msg, baseTrace, maybeCache)

      Forall qs inner
        | all (\q -> qvType q == QuantInt) qs ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                (decided, msg) = proveForallInt theoryInt innerInt
            in (decided, msg, baseTrace, maybeCache)

      Forall _ _ ->
        let quantMsg = "Quantifiers are not yet supported; cannot solve universally quantified goal: " ++ prettyFormula formula
        in (False, quantMsg, baseTrace, maybeCache)

      _ -> fallThrough baseTrace hasInt hasDiv hasSqrt
  where
    fallThrough baseTrace hasInt hasDiv hasSqrt
      | any containsQuantifier theory =
          (False, "Quantifiers in assumptions are not supported yet.", baseTrace, maybeCache)
      | hasInt =
          let outcome = intSolve defaultIntSolveOptions theory formula
          in case intResult outcome of
               Just True  -> (True, reasonOutcome outcome True, baseTrace, maybeCache)
               Just False -> (False, reasonOutcome outcome False, baseTrace, maybeCache)
               Nothing    ->
                 let msg = "Integer domain parsed but solver is incomplete for this goal."
                           ++ if intBruteCandidate outcome
                              then " A bounded brute-force search is available but currently disabled."
                              else ""
                 in (False, msg, baseTrace, maybeCache)
      | hasDiv =
          let (th', goal') = eliminateRational theory formula
              hasSqrt' = containsSqrtFormula goal' || any containsSqrtFormula th'
              (th'', goal'') = if hasSqrt'
                                 then eliminateSqrt th' goal'
                                 else (th', goal')
              proved = proveFormulaCAD th'' goal''
              msg = if proved
                    then "Proved via CAD with rational" ++
                         (if hasSqrt' then " and sqrt" else "") ++
                         " elimination"
                    else "Not proved via CAD with rational elimination"
              trace = emptyTrace { usedAssumptions = th'' }
          in (proved, msg, trace, maybeCache)
      | hasSqrt =
          let (th', goal') = eliminateSqrt theory formula
              proved = proveFormulaCAD th' goal'
              msg = if proved
                    then "Proved via CAD with sqrt elimination"
                    else "Not proved via CAD with sqrt elimination"
              trace = emptyTrace { usedAssumptions = th' }
          in (proved, msg, trace, maybeCache)
      | containsQuantifier formula =
          let proved = solveQuantifiedFormulaCAD theory formula
              msg = if proved then "Proved by CAD (Quantifier Elimination)" else "Refuted by CAD (Quantifier Elimination)"
          in (proved, msg, baseTrace, maybeCache)
      | otherwise = groebnerFallback buchberger maybeCache theory formula

-- | Prove a formula with custom Buchberger function and optional cache support
-- Returns: (isProved, reason, trace, updatedCache)
proveTheoryWithOptions :: ([Poly] -> [Poly]) -> Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithOptions customBuchberger maybeCache theory formula =
  let baseTrace = emptyTrace { usedAssumptions = theory }
      hasInt = containsIntFormula formula || any containsIntFormula theory
      hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory
      fallThrough baseTrace' hasInt' hasDiv' hasSqrt' =
        if any containsQuantifier theory
          then (False, "Quantifiers in assumptions are not supported yet.", baseTrace', maybeCache)
        else if hasInt'
          then
            let outcome = intSolve defaultIntSolveOptions theory formula
            in case intResult outcome of
                 Just True  -> (True, reasonOutcome outcome True, baseTrace', maybeCache)
                 Just False -> (False, reasonOutcome outcome False, baseTrace', maybeCache)
                 Nothing    ->
                   let msg = "Integer domain parsed but solver is incomplete for this goal."
                             ++ if intBruteCandidate outcome
                                then " A bounded brute-force search is available but currently disabled."
                                else ""
                   in (False, msg, baseTrace', maybeCache)
        else if hasDiv'
          then
            let (th', goal') = eliminateRational theory formula
                hasSqrt'' = containsSqrtFormula goal' || any containsSqrtFormula th'
                (th'', goal'') = if hasSqrt''
                                   then eliminateSqrt th' goal'
                                   else (th', goal')
                proved = proveFormulaCAD th'' goal''
                msg = if proved
                      then "Proved via CAD with rational" ++
                           (if hasSqrt'' then " and sqrt" else "") ++
                           " elimination"
                      else "Not proved via CAD with rational elimination"
                trace = emptyTrace { usedAssumptions = th'' }
            in (proved, msg, trace, maybeCache)
        else if hasSqrt'
          then
            let (th', goal') = eliminateSqrt theory formula
                proved = proveFormulaCAD th' goal'
                msg = if proved
                      then "Proved via CAD with sqrt elimination"
                      else "Not proved via CAD with sqrt elimination"
                trace = emptyTrace { usedAssumptions = th' }
            in (proved, msg, trace, maybeCache)
        else if containsQuantifier formula
          then
            let proved = solveQuantifiedFormulaCAD theory formula
                msg = if proved then "Proved by CAD (Quantifier Elimination)" else "Refuted by CAD (Quantifier Elimination)"
            in (proved, msg, baseTrace', maybeCache)
        else
          groebnerFallback customBuchberger maybeCache theory formula
  in
    case formula of
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
                     then fallThrough baseTrace hasInt hasDiv hasSqrt
                     else
                       case negateRealGoal inner of
                         Nothing -> fallThrough baseTrace hasInt hasDiv hasSqrt
                         Just negs ->
                           let satAny = any (\ng -> satisfiableFormulaCAD theory ng) negs
                           in if satAny
                                then (False, "Found counterexample branch for real universal (CAD satisfiable).", baseTrace, maybeCache)
                                else (True, "Real universal proved by refuting negation with CAD.", baseTrace, maybeCache)

      Exists qs inner
        | all (\q -> qvType q == QuantReal) qs
        , not (any containsQuantifier theory) ->
            let hasIntForm = containsIntFormula inner || any containsIntFormula theory
                hasDivForm = containsDivFormula inner || any containsDivFormula theory
                hasSqrtForm = containsSqrtFormula inner || any containsSqrtFormula theory
                hasNestedQuant = containsQuantifier inner
                polyOk = all isPolyFormula (inner : theory)
            in if hasIntForm || hasDivForm || hasSqrtForm || not polyOk || hasNestedQuant
                 then fallThrough baseTrace hasInt hasDiv hasSqrt
                 else
                   let (holds, msg) =
                         case traverse boundsFromQ qs of
                           Nothing ->
                             let ok = satisfiableFormulaCAD theory inner
                                 m = if ok
                                       then "Real existential proved satisfiable via CAD."
                                       else "Real existential refuted via CAD (no satisfying cell)."
                             in (ok, m)
                           Just bnds ->
                             let theoryWithBounds = theory ++ concat bnds
                                 ok = satisfiableFormulaCAD theoryWithBounds inner
                                 m = if ok
                                       then "Real existential proved satisfiable via CAD over bounding box."
                                       else "Real existential refuted via CAD within bounds."
                             in (ok, m)
                   in (holds, msg, baseTrace, maybeCache)

      Exists qs inner
        | all (\q -> qvType q == QuantInt) qs ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                outcome = intSat defaultIntSolveOptions (theoryInt ++ [innerInt])
                proved = intResult outcome == Just True
                msg = case intResult outcome of
                        Just True  -> "Existential integer goal is satisfiable. " ++ reasonOutcome outcome True
                        Just False -> "Existential integer goal is unsatisfiable. " ++ reasonOutcome outcome False
                        Nothing    -> "Integer existential parsed but solver is incomplete for this goal."
            in (proved, msg, baseTrace, maybeCache)

      Forall qs inner
        | all (\q -> qvType q == QuantInt) qs ->
            let intNames = map qvName qs
                theoryInt = map (promoteIntVars intNames) theory
                innerInt = promoteIntVars intNames inner
                (decided, msg) = proveForallInt theoryInt innerInt
            in (decided, msg, baseTrace, maybeCache)

      Forall _ _ ->
        (False, "Quantifiers are not yet supported; cannot solve universally quantified goal: " ++ prettyFormula formula, baseTrace, maybeCache)

      _ -> fallThrough baseTrace hasInt hasDiv hasSqrt

-- | Original proveTheory function (no caching)
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
proveTheory theory formula =
  let (isProved, reason, trace, _) = proveTheoryWithCache Nothing theory formula
  in (isProved, reason, trace)

-- | Enhanced Positivity Checker with multiple methods
checkPositivity :: Poly -> Bool -> (Bool, String)
checkPositivity p allowZero =
  let result = checkPositivityEnhanced p allowZero
      confidenceStr = case confidence result of
                        Proven -> "[PROVEN]"
                        HighConfidence -> "[HIGH CONFIDENCE]"
                        Heuristic -> "[HEURISTIC]"
      methodStr = show (method result)
  in (isPositive result, confidenceStr ++ " " ++ explanation result ++ " (Method: " ++ methodStr ++ ")")

-- =============================================
-- Integer Evaluation (minimal, ground/constant only)
-- =============================================

-- Try to solve integer goals by evaluation/substitution of integer constants.
-- Returns Just True/False if decided, Nothing otherwise.
data IntSolveOptions = IntSolveOptions
  { allowBruteForce :: Bool
  } deriving (Show, Eq)

-- Lightweight satisfiability check for integer conjunctions (theory only).
-- Uses the interval solver with a tautological goal to detect contradictions.
intSat :: IntSolveOptions -> Theory -> IntSolveOutcome
intSat opts th =
  let taut = Eq (IntConst 0) (IntConst 0)
      allLinear = all isLinearFormula th
      quickUnsat = any constraintUnsatQuick th
  in if quickUnsat
       then IntSolveOutcome (Just False) False False False False False
       else if null th
         then IntSolveOutcome Nothing False False False False False
         else if allLinear
                then intIntervalSolve opts True th taut
                else IntSolveOutcome Nothing False False False False False

isLinearFormula :: Formula -> Bool
isLinearFormula (Eq l r) = isJust (intLinDiff l r)
isLinearFormula (Ge l r) = isJust (intLinDiff l r)
isLinearFormula (Gt l r) = isJust (intLinDiff l r)
isLinearFormula _ = False

-- Quick contradiction detection for simple non-negative expressions
constraintUnsatQuick :: Formula -> Bool
constraintUnsatQuick (Eq l r) =
  (nonNegExpr l && constNegative r) || (nonNegExpr r && constNegative l)
constraintUnsatQuick (Gt l r) =
  constNonPositive l && nonNegExpr r
constraintUnsatQuick _ = False

constNegative :: Expr -> Bool
constNegative (IntConst n) = n < 0
constNegative (Const r) = denominator r == 1 && numerator r < 0
constNegative _ = False

constNonPositive :: Expr -> Bool
constNonPositive (IntConst n) = n <= 0
constNonPositive (Const r) = denominator r == 1 && numerator r <= 0
constNonPositive _ = False

nonNegExpr :: Expr -> Bool
nonNegExpr (IntConst n) = n >= 0
nonNegExpr (Const r) = denominator r == 1 && numerator r >= 0
nonNegExpr (Pow _ k) = even k && k > 0
nonNegExpr _ = False

-- Prove universal integer goals by refuting negation via integer solver.
-- Returns (proved?, message)
proveForallInt :: Theory -> Formula -> (Bool, String)
proveForallInt theory goal =
  case negateIntGoal goal of
    Nothing -> (False, "Integer universal parsed but negation is unsupported for this goal shape.")
    Just negs ->
      let outcomes = [ intSat defaultIntSolveOptions (theory ++ [ng]) | ng <- negs ]
          anySat = any (\o -> intResult o == Just True) outcomes
          anyUnknown = any (\o -> intResult o == Nothing) outcomes
      in if anySat
           then (False, "Found a counterexample branch for integer universal (negation satisfiable).")
           else if anyUnknown
                  then (False, "Integer universal parsed but solver is incomplete for this goal.")
                  else (True, "Integer universal proved by refuting all negation branches.")

-- Bounded real universals (single variable, linear inequality with rational bounds)
proveBoundedRealsForall :: Theory -> [QuantVar] -> Formula -> Maybe (Bool, String)
proveBoundedRealsForall theory qs goal
  | null qs = Nothing
  | not (all (\q -> qvType q == QuantReal) qs) = Nothing
  | any (isNothing . qvLower) qs || any (isNothing . qvUpper) qs = Nothing
  | [q] <- qs =
      case (qvLower q >>= rationalFromExpr, qvUpper q >>= rationalFromExpr) of
        (Just lo, Just hi) | lo <= hi ->
          if varInTheory (qvName q) theory then Nothing else
            case linearBoundsForVar (qvName q) goal of
              Just (coef, constTerm, strict) ->
                let minVal = if coef >= 0 then coef * lo + constTerm else coef * hi + constTerm
                in if strict
                     then Just (minVal > 0, if minVal > 0 then "Bounded real universal proved (linear > 0 over interval)" else "Found counterexample in bounded real universal.")
                     else Just (minVal >= 0, if minVal >= 0 then "Bounded real universal proved (linear >= 0 over interval)" else "Found counterexample in bounded real universal.")
              Nothing ->
                case univariatePolyForVar (qvName q) goal of
                  Just (upoly, strict) ->
                    proveUnivariatePolyForall lo hi upoly strict
                  Nothing -> Nothing
        _ -> Nothing
  | otherwise =
      let hasIntForm = containsIntFormula goal || any containsIntFormula theory
          hasSqrtForm = containsSqrtFormula goal || any containsSqrtFormula theory
          polyOk = all isPolyFormula (goal : theory)
      in if hasIntForm || hasSqrtForm || not polyOk
           then Nothing
           else
             case traverse boundsFromQ qs of
               Nothing -> Nothing
               Just bnds ->
                 let theoryWithBounds = theory ++ concat bnds
                     holds = proveFormulaCAD theoryWithBounds goal
                 in Just (holds, if holds
                                  then "Bounded real universal proved via CAD over bounding box."
                                  else "Counterexample exists within bounds (CAD).")
-- Extract linear coefficient for a single variable from an inequality
-- Returns (a, c, strict) meaning a*x + c >= 0 (or > 0 if strict)
linearBoundsForVar :: String -> Formula -> Maybe (Rational, Rational, Bool)
linearBoundsForVar v (Ge l r) = linearCoeffs v (toPolySub M.empty (Sub l r)) False
linearBoundsForVar v (Gt l r) = linearCoeffs v (toPolySub M.empty (Sub l r)) True
linearBoundsForVar _ _ = Nothing

linearCoeffs :: String -> Poly -> Bool -> Maybe (Rational, Rational, Bool)
linearCoeffs v (Poly mp) strict =
  let terms = M.toList mp
      (aSum, cSum, ok) = foldl (\(a,c,good) (mon, coeff) ->
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

isPolyFormula :: Formula -> Bool
isPolyFormula (Eq l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Ge l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Gt l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Forall _ f) = isPolyFormula f
isPolyFormula (Exists _ f) = isPolyFormula f
isPolyFormula (And f1 f2) = isPolyFormula f1 && isPolyFormula f2
isPolyFormula (Or f1 f2) = isPolyFormula f1 && isPolyFormula f2
isPolyFormula (Not f) = isPolyFormula f

-- Convert Poly to univariate coefficient list in variable v, if possible.
polyToUPoly :: String -> Poly -> Maybe [Rational]
polyToUPoly v (Poly mp) =
  let addTerm acc (Monomial m, coeff) =
        case M.toList m of
          [] -> Just (M.insertWith (+) 0 coeff acc) -- constant term
          [(v', p)] | v' == v -> Just (M.insertWith (+) p coeff acc)
          _ -> Nothing
      coeffMap = foldM addTerm M.empty (M.toList mp)
  in case coeffMap of
       Nothing -> Nothing
       Just cm ->
         let maxPow = if M.null cm then 0 else maximum (M.keys cm)
         in Just [ M.findWithDefault 0 i cm | i <- [0..maxPow] ]

-- Extract univariate polynomial (in target var) from a >= or > goal
univariatePolyForVar :: String -> Formula -> Maybe ([Rational], Bool)
univariatePolyForVar v (Ge l r) =
  let p = toPolySub M.empty (Sub l r)
  in (,False) <$> polyToUPoly v p
univariatePolyForVar v (Gt l r) =
  let p = toPolySub M.empty (Sub l r)
  in (,True) <$> polyToUPoly v p
univariatePolyForVar _ _ = Nothing

-- Prove that a univariate polynomial is non-negative (or positive) over [lo, hi].
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
       then
         if rootCount > 0
           then Just (False, "Found counterexample in bounded real universal (root in interval).")
           else
             let ok = all (> 0) allVals
             in Just (ok, if ok then "Bounded real universal proved (polynomial > 0 over interval)" else "Found counterexample in bounded real universal.")
       else
         let ok = all (>= 0) allVals
         in Just (ok, if ok then "Bounded real universal proved (polynomial >= 0 over interval)" else "Found counterexample in bounded real universal.")

-- Check if variable occurs in theory
varInTheory :: String -> Theory -> Bool
varInTheory v = any (v `elem`) . map varsInFormula

varsInFormula :: Formula -> [String]
varsInFormula (Eq l r) = varsInExpr l ++ varsInExpr r
varsInFormula (Ge l r) = varsInExpr l ++ varsInExpr r
varsInFormula (Gt l r) = varsInExpr l ++ varsInExpr r
varsInFormula (Forall _ f) = varsInFormula f
varsInFormula (Exists _ f) = varsInFormula f
varsInFormula (And f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Or f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Not f) = varsInFormula f

varsInExpr :: Expr -> [String]
varsInExpr (Var v) = [v]
varsInExpr (Add a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Sub a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Mul a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Div a b) = varsInExpr a ++ varsInExpr b
varsInExpr (Pow e _) = varsInExpr e
varsInExpr (Sqrt e) = varsInExpr e
varsInExpr (Determinant rows) = concatMap varsInExpr (concat rows)
varsInExpr (Circle _ _ r) = varsInExpr r
varsInExpr _ = []

-- Generate negation branches for a simple integer goal.
negateIntGoal :: Formula -> Maybe [Formula]
negateIntGoal (Eq l r) = Just [Gt l r, Gt r l]
negateIntGoal (Ge l r) = Just [Gt r l]     -- ¬(l >= r) => r > l
negateIntGoal (Gt l r) = Just [Ge r l]     -- ¬(l > r)  => r >= l
negateIntGoal _ = Nothing

-- Generate negation branches for a simple real goal (polynomial comparisons).
negateRealGoal :: Formula -> Maybe [Formula]
negateRealGoal (Eq l r) = Just [Gt l r, Gt r l]
negateRealGoal (Ge l r) = Just [Gt r l]     -- ¬(l >= r) => r > l
negateRealGoal (Gt l r) = Just [Ge r l]     -- ¬(l > r)  => r >= l
negateRealGoal _ = Nothing

-- Promote bound variable names to IntVar in a formula/expr
promoteIntVars :: [String] -> Formula -> Formula
promoteIntVars names f = goF names f
  where
    goF ns (Eq l r) = Eq (goE ns l) (goE ns r)
    goF ns (Ge l r) = Ge (goE ns l) (goE ns r)
    goF ns (Gt l r) = Gt (goE ns l) (goE ns r)
    goF ns (And f1 f2) = And (goF ns f1) (goF ns f2)
    goF ns (Or f1 f2) = Or (goF ns f1) (goF ns f2)
    goF ns (Not f) = Not (goF ns f)
    goF ns (Forall qs f') =
      let ns' = ns
      in Forall qs (goF ns' f')
    goF ns (Exists qs f') =
      let ns' = ns
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

defaultIntSolveOptions :: IntSolveOptions
defaultIntSolveOptions = IntSolveOptions { allowBruteForce = False }

data IntSolveOutcome = IntSolveOutcome
  { intResult :: Maybe Bool
  , intUsedBrute :: Bool
  , intBruteCandidate :: Bool
  , intUsedBranch :: Bool
  , intUsedLP :: Bool
  , intLPInfeasible :: Bool
  } deriving (Show, Eq)

reasonOutcome :: IntSolveOutcome -> Bool -> String
reasonOutcome outcome truth =
  let verdict = if truth then "proved" else "refuted"
  in if intUsedBrute outcome
       then "Integer goal " ++ verdict ++ " by bounded brute-force search"
       else if intUsedBranch outcome
              then "Integer goal " ++ verdict ++ " by bounded branch search"
             else if intUsedLP outcome
                    then "Integer goal " ++ verdict ++ " after LP-bound tightening"
              else "Integer goal " ++ verdict ++ " by integer evaluator"

intSolve :: IntSolveOptions -> Theory -> Formula -> IntSolveOutcome
intSolve opts theory goal =
  let subM = buildIntSubMap theory
      theorySub = map (substituteInts subM) theory
      goalSub = substituteInts subM goal
  in case intEvalFormula goalSub of
       Just b  -> IntSolveOutcome (Just b) False False False False False
       Nothing -> intIntervalSolve opts True theorySub goalSub

-- Build substitutions from Eq IntVar IntConst (or symmetric) in theory
buildIntSubMap :: Theory -> M.Map String Expr
buildIntSubMap theory = fixedPoint M.empty
  where
    intEqs = [ f | f@(Eq _ _) <- theory ]

    fixedPoint acc =
      let acc' = foldl step acc intEqs
      in if acc' == acc then acc else fixedPoint acc'

    step m (Eq (IntVar v) (IntConst n)) = M.insert v (IntConst n) m
    step m (Eq (IntConst n) (IntVar v)) = M.insert v (IntConst n) m
    step m (Eq l r) =
      case intLinDiff (substituteIntsExpr m l) (substituteIntsExpr m r) of
        Just (coeffs, c) ->
          let unknowns = Map.keys (Map.filter (/= 0) coeffs)
          in case unknowns of
               [v] ->
                 let a = coeffs Map.! v
                 in if a /= 0 && (-c) `mod` a == 0
                    then M.insert v (IntConst ((-c) `div` a)) m
                    else m
               _ -> m
        _ -> m
    step m _ = m

intEvalFormula :: Formula -> Maybe Bool
intEvalFormula (Eq l r) = do
  -- Try linear diff first
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c == 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a == b)
intEvalFormula (Ge l r) = do
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c >= 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a >= b)
intEvalFormula (Gt l r) = do
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c > 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a > b)

intEvalExpr :: Expr -> Maybe Integer
intEvalExpr (IntConst n) = Just n
intEvalExpr (IntVar _) = Nothing
intEvalExpr (Const r) =
  let n = numerator r
      d = denominator r
  in if d == 1 then Just n else Nothing
intEvalExpr (Add a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x + y)
intEvalExpr (Sub a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x - y)
intEvalExpr (Mul a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x * y)
intEvalExpr (Pow e n) = do
  x <- intEvalExpr e
  return (x ^ (fromIntegral n :: Integer))
intEvalExpr _ = Nothing

-- Linear integer form: returns (coefficients map, constant term)
-- Represents sum coeffs[var]*var + const
intLin :: Expr -> Maybe (Map.Map String Integer, Integer)
intLin (IntConst n) = Just (Map.empty, n)
intLin (Const r) =
  let n = numerator r; d = denominator r
  in if d == 1 then Just (Map.empty, n) else Nothing
intLin (IntVar v) = Just (Map.singleton v 1, 0)
intLin (Add a b) = do
  (ma, ca) <- intLin a
  (mb, cb) <- intLin b
  return (Map.unionWith (+) ma mb, ca + cb)
intLin (Sub a b) = do
  (ma, ca) <- intLin a
  (mb, cb) <- intLin b
  return (Map.unionWith (+) ma (Map.map negate mb), ca - cb)
intLin (Mul a b) =
  case (intLin a, intLin b) of
    (Just (ma, ca), Just (mb, cb))
      | Map.null ma -> Just (Map.map (* ca) mb, ca * cb)
      | Map.null mb -> Just (Map.map (* cb) ma, ca * cb)
    _ -> Nothing
intLin (Pow e 1) = intLin e
intLin _ = Nothing

-- Linearized difference l - r
intLinDiff :: Expr -> Expr -> Maybe (Map.Map String Integer, Integer)
intLinDiff l r = intLin (Sub l r)

-- Compute gcd of coefficients
gcdCoeffs :: Map.Map String Integer -> Integer
gcdCoeffs m
  | Map.null m = 0
  | otherwise = foldl1 gcd (map abs (Map.elems m))

-- Intersect an interval with a congruence class modulo g
congruenceInterval :: Integer -> Interval -> Maybe Interval
congruenceInterval g (Interval lo hi) =
  let snap n = Just (n + ((-n) `mod` g))
  in case (lo, hi) of
       (Just l, Just h) ->
         let l' = l + ((-l) `mod` g)
             h' = h - (h `mod` g)
         in if l' > h' then Nothing else Just (Interval (Just l') (Just h'))
       (Just l, Nothing) -> snap l >>= \l' -> Just (Interval (Just l') Nothing)
       (Nothing, Just h) -> Just (Interval Nothing (Just (h - (h `mod` g))))
       _ -> Just (Interval Nothing Nothing)

-- Modular feasibility check: gcd(coeffs) must divide constant for equality.
modularUnsat :: Map.Map String Integer -> Integer -> Bool
modularUnsat coeffs c =
  let g = gcdCoeffs coeffs
  in g /= 0 && (c `mod` g) /= 0

-- =============================================
-- Integer Interval Reasoner (very lightweight)
-- =============================================

data Interval = Interval { lower :: Maybe Integer, upper :: Maybe Integer } deriving (Show, Eq)

topInterval :: Interval
topInterval = Interval Nothing Nothing

singletonInterval :: Integer -> Interval
singletonInterval n = Interval (Just n) (Just n)

intersectInterval :: Interval -> Interval -> Maybe Interval
intersectInterval (Interval l1 u1) (Interval l2 u2) =
  let l = maxMaybe l1 l2
      u = minMaybe u1 u2
  in case (l, u) of
       (Just a, Just b) | a > b -> Nothing
       _ -> Just (Interval l u)

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing b = b
maxMaybe a Nothing = a
maxMaybe (Just a) (Just b) = Just (max a b)

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just (min a b)

-- Extract integer variables from expressions/formulas
intVarsExpr :: Expr -> [String]
intVarsExpr (IntVar v) = [v]
intVarsExpr (Add a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Sub a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Mul a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Div a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Pow e _) = intVarsExpr e
intVarsExpr (Sqrt e) = intVarsExpr e
intVarsExpr (Determinant rows) = concatMap intVarsExpr (concat rows)
intVarsExpr (Circle _ _ e) = intVarsExpr e
intVarsExpr _ = []

intVarsFormula :: Formula -> [String]
intVarsFormula (Eq l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Ge l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Gt l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (And f1 f2) = intVarsFormula f1 ++ intVarsFormula f2
intVarsFormula (Or f1 f2) = intVarsFormula f1 ++ intVarsFormula f2
intVarsFormula (Not f) = intVarsFormula f
intVarsFormula (Forall qs f) =
  let bound = [ qvName q | q <- qs, qvType q == QuantInt ]
  in filter (`notElem` bound) (intVarsFormula f)
intVarsFormula (Exists qs f) =
  let bound = [ qvName q | q <- qs, qvType q == QuantInt ]
  in filter (`notElem` bound) (intVarsFormula f)

-- Ceiling division for integers
ceilDiv :: Integer -> Integer -> Integer
ceilDiv _ 0 = 0
ceilDiv a b =
  let (q, r) = quotRem a b
  in if r == 0 then q else if (a > 0) == (b > 0) then q + 1 else q

-- Floor division for integers
floorDiv :: Integer -> Integer -> Integer
floorDiv _ 0 = 0
floorDiv a b =
  let (q, r) = quotRem a b
  in if r == 0 then q else if (a > 0) == (b > 0) then q else q - 1

-- Update a single variable interval from a linear inequality a*v + c >= 0 (or >0)
boundFromIneq :: Bool -> Integer -> Integer -> Interval
boundFromIneq strict a c
  | a == 0 =
      if (if strict then c > 0 else c >= 0)
         then topInterval
         else Interval (Just 1) (Just 0) -- empty, will be caught by intersect
  | a > 0 =
      let base = -c
          lb = if strict then ceilDiv (base + 1) a else ceilDiv base a
      in Interval (Just lb) Nothing
  | otherwise =
      let base = -c
          ub = if strict then floorDiv (base - 1) a else floorDiv base a
      in Interval Nothing (Just ub)

-- Decide linear goal using intervals if possible
decideWithIntervals :: Map.Map String Interval -> Formula -> Maybe Bool
decideWithIntervals env (Eq l r) =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> Just (c == 0)
      | otherwise ->
          if modularUnsat coeffs c
             then Just False -- no integer solution possible
             else
               case linearBounds coeffs c env of
                 Just (mn, mx)
                   | mn == 0 && mx == 0 -> Just True
                   | mx < 0 || mn > 0 -> Just False
                   | otherwise -> Nothing
                 _ -> Nothing
    _ -> Nothing
decideWithIntervals env (Ge l r) = checkIneq False env l r
decideWithIntervals env (Gt l r) = checkIneq True env l r

checkIneq :: Bool -> Map.Map String Interval -> Expr -> Expr -> Maybe Bool
checkIneq strict env l r =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> Just (if strict then c > 0 else c >= 0)
      | otherwise ->
          case linearBounds coeffs c env of
            Just (mn, mx)
              | strict ->
                  if mn > 0 then Just True else if mx <= 0 then Just False else Nothing
              | otherwise ->
                  if mn >= 0 then Just True else if mx < 0 then Just False else Nothing
            _ -> Nothing
    _ -> Nothing

-- Interval-based integer solver: propagates simple linear constraints and then
-- tries to decide the goal.
intIntervalSolve :: IntSolveOptions -> Bool -> Theory -> Formula -> IntSolveOutcome
intIntervalSolve opts allowBranch theory goal =
  let vars = nub (concatMap intVarsFormula (goal : theory))
      initial = Map.fromList [ (v, topInterval) | v <- vars ]
      theoryWithGoal = goal : theory
      initialWithSolution =
        case solveDiophantine theoryWithGoal of
          Left False -> Left False
          Right (Just singles) ->
            foldM (\env (v, val) ->
                     case Map.lookup v env of
                       Nothing -> Right (Map.insert v (singletonInterval val) env)
                       Just iv ->
                         case intersectInterval iv (singletonInterval val) of
                           Just iv' -> Right (Map.insert v iv' env)
                           Nothing -> Left False
                  ) initial (Map.toList singles)
          Right Nothing -> Right initial
      -- Refinement + elimination loop
      solveLoop envIn theoryIn usedLP 0 = Right (envIn, theoryIn, usedLP)
      solveLoop envIn theoryIn usedLP n =
        let step = do
              envRefined <- iterateRefineFix envIn 20 theoryIn
              theoryPruned <- pruneConstraints envRefined theoryIn
              (envLP, usedLP1) <- linearRelaxation theoryIn goal envRefined
              envRef2 <- iterateRefineFix envLP 20 theoryPruned
              theoryPruned2 <- pruneConstraints envRef2 theoryPruned
              (envCooper, theoryCooper) <- multiCooper envRef2 theoryPruned2 goal
              let changed = envCooper /= envIn || theoryCooper /= theoryIn || usedLP1
                  usedLP' = usedLP || usedLP1
              pure (changed, envCooper, theoryCooper, usedLP')
        in case step of
             Left b -> Left b
             Right (changed, envCooper, theoryCooper, usedLP') ->
               if changed
                 then solveLoop envCooper theoryCooper usedLP' (n-1)
                 else Right (envCooper, theoryCooper, usedLP')

  in case initialWithSolution of
       Left b -> IntSolveOutcome (Just b) False False False False False
       Right env0 ->
          case solveLoop env0 theory False (8 :: Int) of
           Left b -> IntSolveOutcome (Just b) False False False False False
           Right (envFinal, theoryFinal, usedLP) ->
             case decideWithIntervals envFinal goal of
               Just res -> IntSolveOutcome (Just res) False False False usedLP False
               Nothing  ->
                 if allowBranch
                   then case branchSmall opts envFinal theoryFinal goal of
                          Just res -> IntSolveOutcome (Just res) False False True usedLP False
                          Nothing -> IntSolveOutcome Nothing False False False usedLP False
                   else IntSolveOutcome Nothing False False False usedLP False

-- Refinement helpers parameterized by theory
iterateRefineFix :: Map.Map String Interval -> Int -> Theory -> Either Bool (Map.Map String Interval)
iterateRefineFix env 0 _ = Right env
iterateRefineFix env n theory =
  case refinePass env theory of
    Left b -> Left b
    Right env' ->
      if env' == env then Right env else iterateRefineFix env' (n-1) theory

refinePass :: Map.Map String Interval -> Theory -> Either Bool (Map.Map String Interval)
refinePass env theory =
  let eqRes = foldl stepEq (Right env) theory
      stepEq (Left b) _ = Left b
      stepEq (Right acc) f@(Eq _ _) = applyEq acc f
      stepEq (Right acc) _ = Right acc
  in eqRes >>= \envAfterEqs ->
       let geRes = foldl stepGe (Right envAfterEqs) theory
           stepGe (Left b) _ = Left b
           stepGe (Right acc) f = applyGe acc (case f of Gt _ _ -> True; _ -> False) f
       in geRes

applyEq :: Map.Map String Interval -> Formula -> Either Bool (Map.Map String Interval)
applyEq env (Eq l r) =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> if c == 0 then Right env else Left False
      | Map.size coeffs == 1 ->
          case Map.toList coeffs of
            [(v, a)] ->
              if a == 0
                then Left False
                else
                  let (q, r) = quotRem (-c) a
                  in if r /= 0
                     then Left False
                     else case Map.lookup v env of
                            Nothing -> Right (Map.insert v (singletonInterval q) env)
                            Just iv ->
                              case intersectInterval iv (singletonInterval q) of
                                Just newIv -> Right (Map.insert v newIv env)
                                Nothing -> Left False
            _ -> Right env
      | otherwise ->
          case refineEqIntervals env coeffs c of
            Left b -> Left b
            Right env' ->
              case linearBounds coeffs c env' of
                Just (mn, mx)
                  | mn == 0 && mx == 0 -> Right env'
                  | mx < 0 || mn > 0 -> Left False
                  | otherwise -> Right env'
                _ -> Right env'
    _ -> Right env
applyEq env _ = Right env

applyGe :: Map.Map String Interval -> Bool -> Formula -> Either Bool (Map.Map String Interval)
applyGe env strict (Ge l r) = applyIneq env strict l r
applyGe env strict (Gt l r) = applyIneq env strict l r
applyGe env _ _ = Right env

applyIneq :: Map.Map String Interval -> Bool -> Expr -> Expr -> Either Bool (Map.Map String Interval)
applyIneq env strict l r =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs ->
          let ok = if strict then c > 0 else c >= 0
          in if ok then Right env else Left False
      | Map.size coeffs == 1 ->
          case Map.toList coeffs of
            [(v, a)] ->
              let newIv = boundFromIneq strict a c
                  g = gcdCoeffs coeffs
                  newIv' = if g > 0 then fromMaybe newIv (congruenceInterval g newIv) else newIv
              in case Map.lookup v env of
                   Nothing -> Right (Map.insert v newIv' env)
                   Just iv ->
                     case intersectInterval iv newIv' of
                       Just iv' -> Right (Map.insert v iv' env)
                       Nothing -> Left False
            _ -> Right env
      | otherwise ->
          case refineIneqIntervals env coeffs c strict of
            Left b -> Left b
            Right env' -> Right env'
    _ -> Right env

-- Compute tightest finite bounds of linear expression sum a_i * v_i + c
-- using current intervals. Returns Nothing if any needed bound is infinite.
linearBounds :: Map.Map String Integer -> Integer -> Map.Map String Interval -> Maybe (Integer, Integer)
linearBounds coeffs c env = foldM step (c, c) (Map.toList coeffs)
  where
    step (mn, mx) (v, a)
      | a == 0 = Just (mn, mx)
      | otherwise =
          case Map.lookup v env of
            Nothing -> Nothing
            Just iv ->
              case boundsForCoeff a (lower iv) (upper iv) of
                Nothing -> Nothing
                Just (lTerm, uTerm) -> Just (mn + lTerm, mx + uTerm)

-- Multiply interval by coefficient; requires finite bounds
boundsForCoeff :: Integer -> Maybe Integer -> Maybe Integer -> Maybe (Integer, Integer)
boundsForCoeff coef lo hi
  | coef > 0 = do
      l <- fmap (* coef) lo
      u <- fmap (* coef) hi
      return (l, u)
  | otherwise = do
      l <- fmap (* coef) hi
      u <- fmap (* coef) lo
      return (l, u)

-- GCD of coefficient magnitudes (0 if empty)
-- Interval refinement for linear equalities with multiple variables.
-- Uses current finite bounds of other variables to tighten each variable.
refineEqIntervals :: Map.Map String Interval -> Map.Map String Integer -> Integer -> Either Bool (Map.Map String Interval)
refineEqIntervals env coeffs c =
  foldM refineOne env (Map.toList coeffs)
  where
    finite (Interval (Just _) (Just _)) = True
    finite _ = False

    -- Compute combined min/max of all variables except v
    restRange v =
      let others = Map.delete v coeffs
      in foldM (\(mn, mx) (u, a) ->
                 case Map.lookup u env of
                   Just iv | finite iv ->
                     case boundsForCoeff a (lower iv) (upper iv) of
                       Just (l, u') -> Right (mn + l, mx + u')
                       Nothing -> Left False
                   _ -> Left False
               ) (0, 0) (Map.toList others)

    refineOne acc (v, a) =
      case Map.lookup v acc of
        Nothing -> Right acc
        Just iv
          | not (finite iv) -> Right acc
          | otherwise ->
              case restRange v of
                Left False -> Left False
                Left _ -> Right acc
                Right (restMin, restMax) ->
                  let g = gcdCoeffs coeffs
                      targetMin = -restMax - c
                      targetMax = -restMin - c
                      (lo', hi') =
                        if a > 0
                          then (ceilDiv targetMin a, floorDiv targetMax a)
                          else (ceilDiv targetMax a, floorDiv targetMin a)
                      tightened = Interval (Just lo') (Just hi')
                      -- also enforce congruence mod gcd if applicable
                      tightened' =
                        if g > 0
                          then case congruenceInterval g iv of
                                 Just congr -> intersectInterval tightened congr
                                 Nothing -> Nothing
                          else Just tightened
                  in case tightened' of
                       Just newIv -> Right (Map.insert v newIv acc)
                       Nothing -> Left False

-- Interval refinement for linear inequalities with multiple variables.
-- Uses current finite bounds (when available) to tighten single-variable bounds.
refineIneqIntervals :: Map.Map String Interval -> Map.Map String Integer -> Integer -> Bool -> Either Bool (Map.Map String Interval)
refineIneqIntervals env coeffs c strict =
  foldM refineOne env (Map.toList coeffs)
  where
    refineOne acc (v, a) =
      case Map.lookup v acc of
        Nothing -> Right acc
        Just iv ->
          case restMaxSum v coeffs acc of
            Nothing -> Right acc
            Just rMax ->
              let c' = c + rMax
                  newIvBase = boundFromIneq strict a c'
                  g = gcdCoeffs coeffs
                  newIv = if g > 0 then fromMaybe newIvBase (congruenceInterval g newIvBase) else newIvBase
              in case intersectInterval iv newIv of
                   Just iv' -> Right (Map.insert v iv' acc)
                   Nothing -> Left False

-- Maximum possible contribution of all variables except v.
-- Returns Nothing if any needed bound is infinite/unknown.
restMaxSum :: String -> Map.Map String Integer -> Map.Map String Interval -> Maybe Integer
restMaxSum v coeffs env = foldM step 0 (Map.toList (Map.delete v coeffs))
  where
    step acc (u, a) =
      case Map.lookup u env of
        Just iv ->
          case maxContribution a iv of
            Just m -> Just (acc + m)
            Nothing -> Nothing
        Nothing -> Nothing

maxContribution :: Integer -> Interval -> Maybe Integer
maxContribution a (Interval lo hi)
  | a >= 0 = fmap (* a) hi
  | otherwise = fmap (* a) lo

-- Targeted small-branch search: pick the smallest finite interval and try assignments
branchSmall :: IntSolveOptions -> Map.Map String Interval -> Theory -> Formula -> Maybe Bool
branchSmall _opts env theory _goal =
  case smallestDomain env of
    Nothing -> Nothing
    Just (varName, vals)
      | length vals > 30 -> Nothing
      | otherwise -> foldl (tryAssign varName) Nothing vals
  where
    smallestDomain m =
      let finite = [ (v, [lo..hi]) | (v, Interval (Just lo) (Just hi)) <- Map.toList m, lo <= hi ]
      in if null finite then Nothing else Just (minimumBy (\a b -> compare (length (snd a)) (length (snd b))) finite)

    tryAssign vn acc val =
      case acc of
        Just res -> Just res
        Nothing ->
          let forcedEnv = Map.insert vn (singletonInterval val) env
          in case pruneConstraints forcedEnv theory of
               Left False -> Nothing
-- Remove constraints already decided by current intervals; detect contradictions early.
pruneConstraints :: Map.Map String Interval -> Theory -> Either Bool Theory
pruneConstraints env = foldr step (Right [])
  where
    step f acc =
      case acc of
        Left b -> Left b
        Right kept ->
          case decideWithIntervals env f of
            Just True -> Right kept
            Just False -> Left False
            Nothing -> Right (f : kept)

-- =============================================
-- LP relaxation (very lightweight)
-- =============================================

-- Linear relaxation for bounds using coefficient signs and current intervals.
-- For now, recompute global min/max for each variable and intersect.
linearRelaxation :: Theory -> Formula -> Map.Map String Interval -> Either Bool (Map.Map String Interval, Bool)
linearRelaxation theory goal env =
  foldM tighten (env, False) (varsInEnv env)
  where
    varsInEnv m = Map.keys m
    tighten (accEnv, usedFlag) v =
      let contribs = collectConstraints v theory goal
          merged = foldM (applyConstraint v accEnv) (accEnv, usedFlag) contribs
      in case merged of
           Left False -> Left False
           Right (acc', flag') -> Right (acc', flag')

    collectConstraints v theory goal =
      [ (coeffs, c, False)
      | Eq l r <- theory
      , Just (coeffs, c) <- [intLinDiff l r]
      , Map.member v coeffs
      ] ++
      [ (coeffs, c, strict)
      | f <- theory ++ [goal]
      , (coeffs, c, strict) <- linearIneq v f
      ]

    linearIneq v (Ge l r) = maybeToList (lin v False l r)
    linearIneq v (Gt l r) = maybeToList (lin v True l r)
    linearIneq _ _ = []

    lin v strict l r =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs -> Just (coeffs, c, strict)
        _ -> Nothing

    applyConstraint v envAcc (_currentEnv, used) (coeffs, c, strict) =
      let a = coeffs Map.! v
          (restMin, restMax) = sumBounds (Map.delete v coeffs) envAcc
          (lo, hi) =
            if a > 0
              then (ceilDiv (-c - restMax) a, floorDiv (-c - restMin - if strict then 1 else 0) a)
              else (ceilDiv (-c - restMin - if strict then 1 else 0) a, floorDiv (-c - restMax) a)
          newIv = Interval (Just lo) (Just hi)
      in case Map.lookup v envAcc of
           Just iv ->
             case intersectInterval iv newIv of
               Just iv' ->
                 if iv' == iv
                   then Right (envAcc, used)
                   else Right (Map.insert v iv' envAcc, True)
               Nothing -> Left False
           Nothing -> Right (Map.insert v newIv envAcc, True)

    sumBounds coeffs envAcc =
      Map.foldlWithKey' (\(mn,mx) var a ->
        case Map.lookup var envAcc of
          Just (Interval lo hi) ->
            let loC = fmap (* a) (if a >= 0 then lo else hi)
                hiC = fmap (* a) (if a >= 0 then hi else lo)
            in (addMaybe mn loC, addMaybe mx hiC)
          Nothing -> (mn,mx)
      ) (0,0) coeffs

    addMaybe base m = maybe base (\x -> base + x) m

-- | Prove existential goal using Constructive Gröbner Basis (Elimination)
-- Heuristic: If the system of equations is consistent (GB /= {1}), we assume generic existence.
proveExistentialConstructive :: Theory -> Formula -> (Bool, String, ProofTrace)
proveExistentialConstructive theory goal =
  case goal of
    Exists qs body ->
      let
        -- 1. Extract equations from the body
        eqsBranches = extractEqualitiesBranches body
        
        -- 2. Build ideal from equations + theory equalities
        theoryEqs = [ Sub l r | Eq l r <- theory ]
        
        -- Renaming strategy: Ensure existential vars are "largest" for Lex elimination.
        -- We prefix them with "zz_" so they sort after typical variables (starting with a-y).
        -- (Assuming Lex order compares strings)
        existentialVars = map qvName qs
        rename v | v `elem` existentialVars = "zz_" ++ v
                 | otherwise = v
        
        renamePoly :: Poly -> Poly
        renamePoly (Poly m) =
            let renameMonomial (Monomial vars) = 
                    Monomial (M.mapKeys rename vars)
            in Poly (M.mapKeys renameMonomial m)

        checkBranch eqs = 
            let rawIdeal = map (toPolySub M.empty) (eqs ++ theoryEqs)
                renamedIdeal = map renamePoly rawIdeal
                basis = buchberger renamedIdeal
                isInconsistent = any isConstantNonZero basis
            in (not isInconsistent, basis)

        -- 3. Check consistency of EACH branch
        results = map checkBranch eqsBranches
        anyConsistent = any fst results
        
        -- 4. Trace (take first consistent or first if none)
        (_, bestBasis) =
          case filter fst results of
            (r:_) -> r
            [] -> case results of
                    (r:_) -> r
                    [] -> (False, [])
        
        step = ComputedGroebnerBasis (length bestBasis)
        trace = ProofTrace [step] theory (length bestBasis)
      in
        if anyConsistent
        then (True, "Solution exists generically (Algebraic consistency proved via Groebner Basis).", trace)
        else (False, "System is inconsistent (Groebner basis contains 1). No solution exists.", trace)
    _ -> (False, "Constructive existence only supports Exists quantification.", emptyTrace)

extractEqualitiesBranches :: Formula -> [[Expr]]
extractEqualitiesBranches (Eq l r) = [[Sub l r]]
extractEqualitiesBranches (And f1 f2) = 
    [ e1 ++ e2 | e1 <- extractEqualitiesBranches f1, e2 <- extractEqualitiesBranches f2 ]
extractEqualitiesBranches (Or f1 f2) = 
    extractEqualitiesBranches f1 ++ extractEqualitiesBranches f2
extractEqualitiesBranches (Exists _ f) = extractEqualitiesBranches f
extractEqualitiesBranches _ = [[]] -- Ignore inequalities for algebraic existence check (heuristic)

isConstantNonZero :: Poly -> Bool
isConstantNonZero (Poly m) =
  case M.toList m of
    [(Monomial vars, c)] | M.null vars -> c /= 0
    _ -> False

-- =============================================
-- Diophantine solver (Smith/Hermite-lite)
-- =============================================

-- Attempts to solve a system of linear Diophantine equations represented as Theory.
-- Returns Left False on inconsistency, Right (Just assignments) if a unique integer solution
-- for some variables is found, Right Nothing if underdetermined but consistent.
solveDiophantine :: Theory -> Either Bool (Maybe (Map.Map String Integer))
solveDiophantine theory =
  let eqs = [ (coeffs, c) | Eq l r <- theory, Just (coeffs, c) <- [intLinDiff l r] ]
      vars = sort . nub $ concatMap (Map.keys . fst) eqs
  in if null eqs then Right Nothing else solveSystem vars (map simplifyEq eqs)

simplifyEq :: (Map.Map String Integer, Integer) -> (Map.Map String Integer, Integer)
simplifyEq (coeffs, c) =
  let g1 = gcdCoeffs coeffs
      g = if g1 == 0 then abs c else gcd g1 (abs c)
  in if g <= 1 then (coeffs, c) else (Map.map (`div` g) coeffs, c `div` g)

solveSystem :: [String] -> [(Map.Map String Integer, Integer)] -> Either Bool (Maybe (Map.Map String Integer))
solveSystem [] eqs =
  if any (\(m,c) -> Map.null m && c /= 0) eqs then Left False else Right Nothing
solveSystem (v:vs) eqs =
  let (withV, withoutV) = L.partition (\(m,_) -> Map.findWithDefault 0 v m /= 0) eqs
  in case withV of
       [] -> solveSystem vs eqs
       (pivot:eqsV) ->
         case eliminateVar v pivot eqsV of
           Left False -> Left False
           Right reduced ->
             case solveSystem vs (withoutV ++ reduced) of
               Left False -> Left False
               Right maybeSol ->
                 case singletonValue v (pivot:withoutV ++ reduced) of
                   Just val ->
                     let sol = Map.insert v val (fromMaybe Map.empty maybeSol)
                     in Right (Just sol)
                   Nothing -> Right maybeSol

-- Eliminate variable v from the equations using pivot as base
eliminateVar :: String -> (Map.Map String Integer, Integer) -> [(Map.Map String Integer, Integer)] -> Either Bool [(Map.Map String Integer, Integer)]
eliminateVar v pivot eqs =
  let a = Map.findWithDefault 0 v (fst pivot)
  in foldM (step a pivot) [] eqs
  where
    step a (m0,c0) acc (m,c) =
      let b = Map.findWithDefault 0 v m
      in if b == 0 then Right ((m,c):acc)
         else
           let l = lcm (abs a) (abs b)
               scale0 = l `div` a
               scale = l `div` b
               m' = Map.filter (/=0) $ Map.unionWith (-) (Map.map (* scale) m) (Map.map (* scale0) m0)
               c' = scale*c - scale0*c0
               simplified = simplifyEq (m', c')
           in if Map.null (fst simplified) && snd simplified /= 0
                 then Left False
                 else Right (simplified:acc)

-- If an equation reduces to a single variable, compute its value if integer.
singletonValue :: String -> [(Map.Map String Integer, Integer)] -> Maybe Integer
singletonValue v eqs =
  let singles = [ (a, c) | (m,c) <- eqs, Map.size m == 1, Map.member v m, let a = m Map.! v ]
  in case singles of
       [] -> Nothing
       ((a,c):_) ->
         if a /= 0 && c `mod` a == 0
           then Just ((-c) `div` a)
           else Nothing

-- =============================================
-- Cooper-style elimination (single variable)
-- =============================================

-- A simplified Cooper elimination for one variable: we try to tighten bounds using
-- inequalities mentioning a single variable, including modulus constraints derived
-- from equalities.
cooperEliminate :: Map.Map String Interval -> Theory -> Formula -> Either Bool (Map.Map String Interval, Theory)
cooperEliminate env theory goal =
  case pickFiniteVar env of
    Nothing -> Right (env, theory)
    Just v ->
      let withV = [ f | f <- theory, mentions v f ]
          eqMods = catMaybes [ modulusFromEq v f | f <- withV ]
          tightened = foldM (tightenWith v) env (withV ++ [goal])
      in case tightened of
           Left b -> Left b
           Right env' ->
             case applyModuli v eqMods env' of
               Left b -> Left b
               Right env'' -> Right (env'', theory)
  where
    mentions v (Eq l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Ge l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Gt l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Le l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Lt l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))

    modulusFromEq v (Eq l r) =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs ->
          let a = coeffs Map.! v
          in Just (abs a, (-c) `mod` abs a)
        _ -> Nothing
    modulusFromEq _ _ = Nothing

    tightenWith v envAcc f =
      case f of
        Ge l r -> tightenIneq v envAcc l r False
        Gt l r -> tightenIneq v envAcc l r True
        Le l r -> tightenIneq v envAcc r l False  -- Flip: l <= r becomes r >= l
        Lt l r -> tightenIneq v envAcc r l True   -- Flip: l < r becomes r > l
        _ -> Right envAcc

    tightenIneq v envAcc l r strict =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs ->
          let a = coeffs Map.! v
              (restMin, restMax) = sumBounds (Map.delete v coeffs) envAcc
              (lo, hi) =
                if a > 0
                  then (ceilDiv (-c - restMax) a, floorDiv (-c - restMin - if strict then 1 else 0) a)
                  else (ceilDiv (-c - restMin - if strict then 1 else 0) a, floorDiv (-c - restMax) a)
              newIv = Interval (Just lo) (Just hi)
          in case Map.lookup v envAcc of
               Just iv ->
                 case intersectInterval iv newIv of
                   Just iv' -> Right (Map.insert v iv' envAcc)
                   Nothing -> Left False
               Nothing -> Right (Map.insert v newIv envAcc)
        _ -> Right envAcc

    sumBounds coeffs envAcc =
      Map.foldlWithKey' (\(mn,mx) var a ->
        case Map.lookup var envAcc of
          Just (Interval lo hi) ->
            let loC = fmap (* a) (if a >= 0 then lo else hi)
                hiC = fmap (* a) (if a >= 0 then hi else lo)
            in (maybe mn (+mn) loC, maybe mx (+mx) hiC)
          Nothing -> (mn,mx)
      ) (0,0) coeffs

    applyModuli v mods envAcc =
      case Map.lookup v envAcc of
        Nothing -> Right envAcc
        Just iv ->
          let intersected = foldM (\ivAcc (m, _) ->
                                      case congruenceInterval m ivAcc of
                                        Just iv' -> Right iv'
                                        Nothing -> Left False
                                  ) iv mods
          in case intersected of
               Left False -> Left False
               Right iv' -> Right (Map.insert v iv' envAcc)

    pickFiniteVar envAcc =
      let finite = [ (v, iv) | (v, iv@(Interval (Just _) (Just _))) <- Map.toList envAcc ]
      in case finite of
           [] -> Nothing
           (x:_) -> Just (fst x)

pickFiniteVarCooper :: Map.Map String Interval -> Maybe String
pickFiniteVarCooper envAcc =
  let finite = [ (v, iv) | (v, iv@(Interval (Just _) (Just _))) <- Map.toList envAcc ]
  in case finite of
       [] -> Nothing
       (x:_) -> Just (fst x)

-- Repeated Cooper elimination on finite-bounded variables
multiCooper :: Map.Map String Interval -> Theory -> Formula -> Either Bool (Map.Map String Interval, Theory)
multiCooper env theory goal = cooperLoop env theory
  where
    cooperLoop envAcc theoryAcc =
      case pickFiniteVarCooper envAcc of
        Nothing -> Right (envAcc, theoryAcc)
        Just _ ->
          case cooperEliminate envAcc theoryAcc goal of
            Left b -> Left b
            Right (env', theory') ->
              if env' == envAcc then Right (envAcc, theoryAcc)
              else cooperLoop env' theory'

