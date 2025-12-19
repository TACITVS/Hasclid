{-# LANGUAGE DeriveGeneric #-}

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

-- | Result of a proof attempt (Either-based API)
data ProofResult = ProofResult
  { proved :: Bool          -- Was the formula proven?
  , reason :: String        -- Explanation or failure reason
  , trace :: ProofTrace     -- Proof trace (steps taken)
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
    showFormula (Divides l r) = prettyExpr l ++ " | " ++ prettyExpr r
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
       Eq (IntVar _) _ -> (f:subs, constrs)
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
-- This allows eliminating one variable even from multivariate linear equations
-- (e.g., alpha + beta + gamma = 1 -> gamma = 1 - alpha - beta).
solveLinearSingleVar :: Expr -> Expr -> Maybe (String, Expr)
solveLinearSingleVar l r =
  case linearCoeffs (toPolySub M.empty (Sub l r)) of
    Just (coeffs, c) | not (M.null coeffs) ->
      -- Pick the "largest" variable to solve for (best for elimination)
      let (v, a) = M.findMax coeffs
          -- a*v + sum(a_i * v_i) + c = 0  => v = (-c - sum(a_i * v_i)) / a
          otherTerms = M.delete v coeffs
          numerator = foldl' (\acc (v', ai) -> Sub acc (Mul (Const ai) (Var v'))) (Const (-c)) (M.toList otherTerms)
          val = simplifyExpr (Div numerator (Const a))
      in Just (v, val)
    _ -> Nothing
  where
    linearCoeffs :: Poly -> Maybe (M.Map String Rational, Rational)
    linearCoeffs (Poly mp) =
      foldl' step (Just (M.empty, 0)) (M.toList mp)
      where
        step Nothing _ = Nothing
        step (Just (vs, c)) (Monomial mon, coeff) =
          case M.toList mon of
            [] -> Just (vs, c + coeff)
            [(v,1)] -> Just (M.insertWith (+) v coeff vs, c)
            _ -> Nothing -- Non-linear term found

    foldl' = L.foldl'

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

-- | Expand bounded integer quantifiers (finite domain)
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
proveTheoryWithCache maybeCache theoryRaw formulaRaw =
  let theory = map expandFiniteDomain theoryRaw
      formula = expandFiniteDomain formulaRaw
      baseTrace = emptyTrace { usedAssumptions = theory }
      hasInt = containsIntFormula formula || any containsIntFormula theory
      hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory
      
      fallThrough baseTrace' hasInt' hasDiv' hasSqrt'
        | any containsQuantifier theory = 
            (False, "Quantifiers in assumptions are not supported yet.", baseTrace', maybeCache)
        | hasInt' = 
            let outcome = intSolve defaultIntSolveOptions theory formula
            in case intResult outcome of
                 Just True  -> (True, reasonOutcome outcome True, baseTrace', maybeCache)
                 Just False -> (False, reasonOutcome outcome False, baseTrace', maybeCache)
                 Nothing    -> 
                   case formula of
                     Eq _ _ -> 
                       let (gProved, gReason, gTrace, gCache) = groebnerFallback buchberger maybeCache theory formula
                       in if gProved 
                          then (True, "Proved by Algebraic Solver (valid for Integers): " ++ gReason, gTrace, gCache)
                          else (False, "Integer Solver Incomplete & Algebraic Solver failed: " ++ gReason, gTrace, gCache)
                     _ -> 
                       let msg = "Integer domain parsed but solver is incomplete for this goal."
                                 ++ if intBruteCandidate outcome
                                    then " A bounded brute-force search is available but currently disabled."
                                    else ""
                       in (False, msg, baseTrace', maybeCache)
                | hasDiv' =
                    let (th', goal') = eliminateRational theory formula
                        hasSqrt' = containsSqrtFormula goal' || any containsSqrtFormula th'
                        (th'', goal'') = if hasSqrt'
                                           then eliminateSqrt th' goal'
                                           else (th', goal')
                    in proveTheoryWithCache maybeCache th'' goal''        | hasSqrt' = 
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
            in (proved, msg, baseTrace', maybeCache)
        | otherwise = groebnerFallback buchberger maybeCache theory formula
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
                bounds = concatMap intBoundsFromQ qs
                boundsInt = map (promoteIntVars intNames) bounds
                outcome = intSat defaultIntSolveOptions (theoryInt ++ boundsInt ++ [innerInt])
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

-- | Prove a formula with custom Buchberger function and optional cache support
-- Returns: (isProved, reason, trace, updatedCache)
proveTheoryWithOptions :: ([Poly] -> [Poly]) -> Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithOptions customBuchberger maybeCache theoryRaw formulaRaw =
  let theory = map expandFiniteDomain theoryRaw
      formula = expandFiniteDomain formulaRaw
      baseTrace = emptyTrace { usedAssumptions = theory }
      hasInt = containsIntFormula formula || any containsIntFormula theory
      hasDiv = containsDivFormula formula || any containsDivFormula theory
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory
      
      fallThrough baseTrace' hasInt' hasDiv' hasSqrt'
        | any containsQuantifier theory = 
            (False, "Quantifiers in assumptions are not supported yet.", baseTrace', maybeCache)
        | hasInt' = 
            let outcome = intSolve defaultIntSolveOptions theory formula
            in case intResult outcome of
                 Just True  -> (True, reasonOutcome outcome True, baseTrace', maybeCache)
                 Just False -> (False, reasonOutcome outcome False, baseTrace', maybeCache)
                 Nothing    -> 
                   case formula of 
                     Eq _ _ -> 
                       let (gProved, gReason, gTrace, gCache) = groebnerFallback customBuchberger maybeCache theory formula
                       in if gProved 
                          then (True, "Proved by Algebraic Solver (valid for Integers): " ++ gReason, gTrace, gCache)
                          else (False, "Integer Solver Incomplete & Algebraic Solver failed: " ++ gReason, gTrace, gCache)
                     _ -> 
                       let msg = "Integer domain parsed but solver is incomplete for this goal."
                                 ++ if intBruteCandidate outcome
                                    then " A bounded brute-force search is available but currently disabled."
                                    else ""
                       in (False, msg, baseTrace', maybeCache)
                | hasDiv' =
                    let (th', goal') = eliminateRational theory formula
                        hasSqrt'' = containsSqrtFormula goal' || any containsSqrtFormula th'
                        (th'', goal'') = if hasSqrt''
                                           then eliminateSqrt th' goal'
                                           else (th', goal')
                    in proveTheoryWithOptions customBuchberger maybeCache th'' goal''        | hasSqrt' = 
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
            in (proved, msg, baseTrace', maybeCache)
        | otherwise = 
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
                bounds = concatMap intBoundsFromQ qs
                boundsInt = map (promoteIntVars intNames) bounds
                outcome = intSat defaultIntSolveOptions (theoryInt ++ boundsInt ++ [innerInt])
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
-- Legacy API - returns tuple for backward compatibility
proveTheory :: Theory -> Formula -> (Bool, String, ProofTrace)
proveTheory theory formula =
  let (isProved, reason, trace, _) = proveTheoryWithCache Nothing theory formula
  in (isProved, reason, trace)

-- | Either-based proveTheory (recommended API)
-- Returns Either ProverError ProofResult for better error handling
proveTheoryE :: Theory -> Formula -> Either ProverError ProofResult
proveTheoryE theory formula =
  let (isProved, reason, trace, _) = proveTheoryWithCache Nothing theory formula
  in Right $ ProofResult isProved reason trace

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

intBoundsFromQ :: QuantVar -> [Formula]
intBoundsFromQ (QuantVar v QuantInt (Just lo) (Just hi)) =
  [Ge (IntVar v) lo, Le (IntVar v) hi]
intBoundsFromQ (QuantVar v QuantInt (Just lo) Nothing) =
  [Ge (IntVar v) lo]
intBoundsFromQ (QuantVar v QuantInt Nothing (Just hi)) =
  [Le (IntVar v) hi]
intBoundsFromQ _ = []

isPolyFormula :: Formula -> Bool
isPolyFormula (Eq l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Ge l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Gt l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Le l r) = not (hasNonPolynomial l || hasNonPolynomial r)
isPolyFormula (Lt l r) = not (hasNonPolynomial l || hasNonPolynomial r)
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

-- | Structural Induction Proof
proveByInduction :: Theory -> Formula -> (Bool, String, ProofTrace)
proveByInduction theory formula =
  case formula of
    Forall [QuantVar n QuantInt Nothing Nothing] body ->
      let
        -- Base Case (n=0)
        baseSub = M.singleton n (Const 0)
        baseGoal = substFormula baseSub body
        (baseOk, baseMsg, baseTrace) = proveTheory theory baseGoal
        
        -- Step Case
        k = n ++ "_k"
        kExpr = Var k
        kPlus1 = Add kExpr (Const 1)
        
        hyp = substFormula (M.singleton n kExpr) body
        stepGoalRaw = substFormula (M.singleton n kPlus1) body
        stepGoal = expandSumStepFormula k kPlus1 stepGoalRaw
        
        (stepOk, stepMsg, stepTrace) = proveTheory (hyp : theory) stepGoal
        
        fullMsg = "Base Case (n=0): " ++ (if baseOk then "PROVED" else "FAILED") ++ "\n" ++
                  baseMsg ++ "\n\n" ++ 
                  "Step Case (P("++k++") -> P("++k++"+1)): " ++ (if stepOk then "PROVED" else "FAILED") ++ "\n" ++ 
                  stepMsg
                  
      in (baseOk && stepOk, fullMsg, stepTrace)
      
    _ -> (False, "Induction requires 'forall ((int n)) ...' (unbounded integer quantifier)", emptyTrace)

expandSumStepFormula :: String -> Expr -> Formula -> Formula
expandSumStepFormula kStr kPlus1 form = 
  let rw e = case e of
               Sum i lo hi body -> 
                 if hi == kPlus1 
                 then Add (Sum i lo (Var kStr) body) (substituteExpr i kPlus1 body)
                 else e
               _ -> e
      mapper = mapExpr rw
  in case form of
       Eq l r -> Eq (mapper l) (mapper r)
       Ge l r -> Ge (mapper l) (mapper r)
       Gt l r -> Gt (mapper l) (mapper r)
       Le l r -> Le (mapper l) (mapper r)
       Lt l r -> Lt (mapper l) (mapper r)
       Divides l r -> Divides (mapper l) (mapper r)
       And a b -> And (expandSumStepFormula kStr kPlus1 a) (expandSumStepFormula kStr kPlus1 b)
       Or a b -> Or (expandSumStepFormula kStr kPlus1 a) (expandSumStepFormula kStr kPlus1 b)
       Not a -> Not (expandSumStepFormula kStr kPlus1 a)
       Forall qs f -> Forall qs (expandSumStepFormula kStr kPlus1 f)
       Exists qs f -> Exists qs (expandSumStepFormula kStr kPlus1 f)