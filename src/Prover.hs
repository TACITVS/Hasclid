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
  ) where

import Expr
import Sturm (countRealRoots, isAlwaysPositive)
import Positivity (checkPositivityEnhanced, PositivityResult(..), PositivityMethod(..), Confidence(..))
import Cache (GroebnerCache, lookupBasis, insertBasis)
import qualified Data.Map.Strict as M
import Data.List (nub)

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

    formatStep (UsedSubstitution v e) = "Used substitution: " ++ v ++ " -> " ++ prettyExpr e
    formatStep (UsedConstraint f) = "Applied constraint: " ++ showFormula f
    formatStep (UsedLemma f) = "Applied lemma: " ++ showFormula f
    formatStep (ComputedGroebnerBasis n) = "Computed Groebner basis (" ++ show n ++ " polynomials)"
    formatStep (ReducedToNormalForm p1 p2) =
      if p2 == polyZero
      then "Reduced to normal form: 0 (PROOF COMPLETE)"
      else "Reduced to normal form: " ++ prettyPoly p2
    formatStep (CheckedPositivity method) = "Checked positivity using: " ++ method

-- =============================================
-- Substitution Logic
-- =============================================

buildSubMap :: Theory -> M.Map String Poly
buildSubMap theory = M.fromList [ (v, toPoly e) | Eq (Var v) e <- theory ]

evaluatePoly :: M.Map String Poly -> Poly -> Poly
evaluatePoly subM (Poly m) =
  let
      evalMono :: Monomial -> Rational -> Poly
      evalMono (Monomial vars) coeff =
        let
          termExpanded = M.foldlWithKey (\accPoly varname power ->
              let basePoly = case M.lookup varname subM of
                               Just p  -> evaluatePoly subM p
                               Nothing -> polyFromVar varname
              in polyMul accPoly (polyPow basePoly power)
            ) (polyFromConst 1) vars
        in polyMul (polyFromConst coeff) termExpanded
      results = map (\(mono, coeff) -> evalMono mono coeff) (M.toList m)
  in foldl polyAdd polyZero results

toPolySub :: M.Map String Poly -> Expr -> Poly
toPolySub subM expr = evaluatePoly subM (toPoly expr)

subPoly :: Poly -> Poly -> Poly
subPoly p1 p2 = polyAdd p1 (polyNeg p2)

-- =============================================
-- GROEBNER BASIS ENGINE (Buchberger's Algorithm)
-- =============================================

-- 1. Multivariate Polynomial Reduction (Division)
reduce :: Poly -> [Poly] -> Poly
reduce p fs
  | p == polyZero = polyZero
  | otherwise = case findDivisor p fs of
      Just (f, mQuot, cQuot) ->
          let subTerm = polyMul (polyMul f (Poly (M.singleton mQuot 1))) (polyFromConst cQuot)
          in reduce (subPoly p subTerm) fs
      Nothing ->
          let Just (ltM, ltC) = getLeadingTerm p
              rest = polySub p (Poly (M.singleton ltM ltC))
              reducedRest = reduce rest fs
          in polyAdd (Poly (M.singleton ltM ltC)) reducedRest

  where
    findDivisor :: Poly -> [Poly] -> Maybe (Poly, Monomial, Rational)
    findDivisor poly divisors =
      case getLeadingTerm poly of
        Nothing -> Nothing
        Just (ltP, cP) ->
            let candidates = [ (f, mDiv, cP / cF)
                             | f <- divisors
                             , let Just (ltF, cF) = getLeadingTerm f
                             , Just mDiv <- [monomialDiv ltP ltF]
                             ]
            in case candidates of
                 (c:_) -> Just c
                 []    -> Nothing

-- 2. S-Polynomial
sPoly :: Poly -> Poly -> Poly
sPoly f g =
  case (getLeadingTerm f, getLeadingTerm g) of
    (Just (ltF, cF), Just (ltG, cG)) ->
      let lcmM = monomialLCM ltF ltG
          Just mF = monomialDiv lcmM ltF
          Just mG = monomialDiv lcmM ltG

          factF = polyMul (Poly (M.singleton mF 1)) (polyFromConst (1/cF))
          factG = polyMul (Poly (M.singleton mG 1)) (polyFromConst (1/cG))

          term1 = polyMul factF f
          term2 = polyMul factG g
      in subPoly term1 term2
    _ -> polyZero

-- 3. Buchberger's Algorithm
buchberger :: [Poly] -> [Poly]
buchberger polys = go (filter (/= polyZero) polys)
  where
    go basis =
      let pairs = [ (f, g) | f <- basis, g <- basis, f /= g ]
          remainders = [ reduce (sPoly f g) basis | (f, g) <- pairs ]
          nonZeroRemainders = filter (/= polyZero) remainders
      in if null nonZeroRemainders
         then basis
         else go (nub (basis ++ nonZeroRemainders))

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

-- | Prove a formula with optional cache support
-- Returns: (isProved, reason, trace, updatedCache)
proveTheoryWithCache :: Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithCache maybeCache theory formula =
  let
      (substAssumptions, constraintAssumptions) = partitionTheory theory
      subM = buildSubMap substAssumptions

      -- Track substitutions used
      substSteps = [ UsedSubstitution v e | Eq (Var v) e <- substAssumptions ]

      -- Track constraint assumptions
      constraintSteps = [ UsedConstraint f | f@(Eq _ _) <- constraintAssumptions ]

      idealGenerators = [ subPoly (toPolySub subM l) (toPolySub subM r)
                        | Eq l r <- constraintAssumptions ]

      -- Compute basis with cache
      (basis, updatedCache) =
        if null idealGenerators
        then ([], maybeCache)
        else case maybeCache of
               Nothing -> (buchberger idealGenerators, Nothing)
               Just cache ->
                 let (maybeCached, cacheAfterLookup) = lookupBasis idealGenerators cache
                 in case maybeCached of
                      Just cachedBasis -> (cachedBasis, Just cacheAfterLookup)
                      Nothing ->
                        let computed = buchberger idealGenerators
                            cacheAfterInsert = insertBasis idealGenerators computed cacheAfterLookup
                        in (computed, Just cacheAfterInsert)

      basisStep = if null idealGenerators
                  then []
                  else [ComputedGroebnerBasis (length basis)]

      (pL, pR) = case formula of
                   Eq l r -> (toPolySub subM l, toPolySub subM r)
                   Ge l r -> (toPolySub subM l, toPolySub subM r)
                   Gt l r -> (toPolySub subM l, toPolySub subM r)

      difference = subPoly pL pR
      normalForm = reduce difference basis

      reductionStep = [ReducedToNormalForm difference normalForm]

      -- Build trace
      allSteps = substSteps ++ constraintSteps ++ basisStep ++ reductionStep

  in case formula of
       Eq _ _ ->
         let result = normalForm == polyZero
             msg = if result
                   then "Equality Holds (Groebner Normal Form is 0)"
                   else "LHS /= RHS (Normal Form: " ++ prettyPoly normalForm ++ ")"
             trace = ProofTrace allSteps theory (length basis)
         in (result, msg, trace, updatedCache)

       Ge _ _ ->
         let (result, msg) = checkPositivity normalForm True
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theory (length basis)
         in (result, msg, trace, updatedCache)

       Gt _ _ ->
         let (result, msg) = checkPositivity normalForm False
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theory (length basis)
         in (result, msg, trace, updatedCache)

-- | Prove a formula with custom Buchberger function and optional cache support
-- Returns: (isProved, reason, trace, updatedCache)
proveTheoryWithOptions :: ([Poly] -> [Poly]) -> Maybe GroebnerCache -> Theory -> Formula -> (Bool, String, ProofTrace, Maybe GroebnerCache)
proveTheoryWithOptions customBuchberger maybeCache theory formula =
  let
      (substAssumptions, constraintAssumptions) = partitionTheory theory
      subM = buildSubMap substAssumptions

      -- Track substitutions used
      substSteps = [ UsedSubstitution v e | Eq (Var v) e <- substAssumptions ]

      -- Track constraint assumptions
      constraintSteps = [ UsedConstraint f | f@(Eq _ _) <- constraintAssumptions ]

      idealGenerators = [ subPoly (toPolySub subM l) (toPolySub subM r)
                        | Eq l r <- constraintAssumptions ]

      -- Compute basis with cache (using custom Buchberger)
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

      basisStep = if null idealGenerators
                  then []
                  else [ComputedGroebnerBasis (length basis)]

      (pL, pR) = case formula of
                   Eq l r -> (toPolySub subM l, toPolySub subM r)
                   Ge l r -> (toPolySub subM l, toPolySub subM r)
                   Gt l r -> (toPolySub subM l, toPolySub subM r)

      difference = subPoly pL pR
      normalForm = reduce difference basis

      reductionStep = [ReducedToNormalForm difference normalForm]

      -- Build trace
      allSteps = substSteps ++ constraintSteps ++ basisStep ++ reductionStep

  in case formula of
       Eq _ _ ->
         let result = normalForm == polyZero
             msg = if result
                   then "Equality Holds (Groebner Normal Form is 0)"
                   else "LHS /= RHS (Normal Form: " ++ prettyPoly normalForm ++ ")"
             trace = ProofTrace allSteps theory (length basis)
         in (result, msg, trace, updatedCache)

       Ge _ _ ->
         let (result, msg) = checkPositivity normalForm True
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theory (length basis)
         in (result, msg, trace, updatedCache)

       Gt _ _ ->
         let (result, msg) = checkPositivity normalForm False
             positivityStep = [CheckedPositivity msg]
             trace = ProofTrace (allSteps ++ positivityStep) theory (length basis)
         in (result, msg, trace, updatedCache)

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
