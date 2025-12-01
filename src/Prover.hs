{-# LANGUAGE DeriveGeneric #-}

module Prover where

import Expr
-- FIXED: Removed invalid 'as _' syntax. 
-- We only need countRealRoots and isAlwaysPositive from Sturm.
import Sturm (countRealRoots, isAlwaysPositive) 
import qualified Data.Map.Strict as M
import Data.List (nub)

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

buchberger :: [Poly] -> [Poly]
buchberger polys = go (filter (/= polyZero) polys)
  where
    go basis =
      let pairs = [ (f, g) | f <- basis, g <- basis, f /= g ]
          remainders = [ reduce (sPoly f g) basis | (f, g) <- pairs ]
          nonZeroRemainders = filter (/= polyZero) remainders
      in if null nonZeroRemainders then basis else go (nub (basis ++ nonZeroRemainders))

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

proveTheory :: Theory -> Formula -> (Bool, String)
proveTheory theory formula = 
  let 
      (substAssumptions, constraintAssumptions) = partitionTheory theory
      subM = buildSubMap substAssumptions
      idealGenerators = [ subPoly (toPolySub subM l) (toPolySub subM r) 
                        | Eq l r <- constraintAssumptions ]
      basis = if null idealGenerators then [] else buchberger idealGenerators
      
      (pL, pR) = case formula of
                   Eq l r -> (toPolySub subM l, toPolySub subM r)
                   Ge l r -> (toPolySub subM l, toPolySub subM r)
                   Gt l r -> (toPolySub subM l, toPolySub subM r)
      
      difference = subPoly pL pR
      normalForm = reduce difference basis
      
  in case formula of
       Eq _ _ -> 
         if normalForm == polyZero
         then (True, "Equality Holds (Groebner Normal Form is 0)")
         else (False, "LHS /= RHS (Normal Form: " ++ prettyPoly normalForm ++ ")")
       
       Ge _ _ -> checkPositivity normalForm True
       Gt _ _ -> checkPositivity normalForm False

-- | Advanced Positivity Checker (Uses Sturm!)
checkPositivity :: Poly -> Bool -> (Bool, String)
checkPositivity p allowZero =
  case toUnivariate p of
    -- Case 1: Univariate Polynomial -> Use STURM!
    Just (var, coeffs) -> 
        let nRoots = countRealRoots coeffs
            lcVal  = if null coeffs then 0 else last coeffs
            -- Positive if leading coeff is positive AND no roots (meaning it never crosses zero)
            -- Note: If roots > 0, it crosses zero, so it can't be always positive.
        in if nRoots == 0 && lcVal > 0
           then (True, "Proven by Sturm's Theorem (0 real roots, LC > 0)")
           else (False, "Sturm Analysis Failed: Found " ++ show nRoots ++ " real roots.")

    -- Case 2: Constant
    Nothing | isConstant p -> 
        let c = getConst p
        in if c > 0 || (allowZero && c==0) 
           then (True, "Constant Value check")
           else (False, "Constant is negative")

    -- Case 3: Multivariate -> Fallback to Sum of Squares
    Nothing -> 
        if isTrivialSOS p 
        then (True, "Sum of Squares (Heuristic)") 
        else (False, "Multivariate polynomial (Sturm requires univariate).")

isConstant :: Poly -> Bool
isConstant (Poly m) = all (\(Monomial v) -> M.null v) (M.keys m)

getConst :: Poly -> Rational
getConst (Poly m) = sum (M.elems m)

isTrivialSOS :: Poly -> Bool
isTrivialSOS (Poly m) = all checkTerm (M.toList m)
  where checkTerm (Monomial vars, coeff) = coeff > 0 && all even (M.elems vars)

validateAssumption :: Theory -> Formula -> Bool
validateAssumption _ _ = True