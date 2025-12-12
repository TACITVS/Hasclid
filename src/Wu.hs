{-|
Module: Wu
Description: Wu's Method (Characteristic Sets) for Geometric Theorem Proving

Wu's Method with Factorization and Branching.
-}

module Wu
  ( -- * Main Wu Proof Function
    wuProve
  , wuProveWithTrace
  , proveExistentialWu

    -- * Characteristic Set Construction
  , CharSet
  , buildCharSet
  , triangularize
  , reduceWithWu

    -- * Pseudo-Division
  , pseudoRemainder
  , pseudoDivide

    -- * Degeneracy Conditions
  , DegeneracyCondition(..)
  , checkDegeneracy

    -- * Wu Proof Trace
  , WuTrace(..)
  , formatWuTrace
  ) where

import Expr
import PolynomialFactor (factorHeuristic, factorPoly)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, nub, intercalate)
import Data.Ord (comparing)
import Numeric.Natural (Natural)

-- =============================================
-- Data Types
-- =============================================

type CharSet = [Poly]

data DegeneracyCondition = DegeneracyCondition
  { leadingCoeff :: Poly
  , varName :: String
  , source :: String
  }
  deriving (Show, Eq)

data WuTrace = WuTrace
  { inputTheory :: Theory
  , inputGoal :: Formula
  , hypothesisPolys :: [Poly]
  , conclusionPoly :: Poly
  , branches :: [WuBranchTrace] -- Traces for each branch
  , isProved :: Bool
  , proofReason :: String
  }
  deriving (Show, Eq)

data WuBranchTrace = WuBranchTrace
  { branchId :: Int
  , characteristicSet :: CharSet
  , degeneracyConditions :: [DegeneracyCondition]
  , finalRemainder :: Poly
  , reductionSteps :: [(String, Poly)]
  , branchProved :: Bool
  }
  deriving (Show, Eq)

-- =============================================
-- Main Wu Proving Functions
-- =============================================

wuProve :: Theory -> Formula -> (Bool, String)
wuProve theory goal = 
  let trace = wuProveWithTrace theory goal
  in (isProved trace, proofReason trace)

wuProveWithTrace :: Theory -> Formula -> WuTrace
wuProveWithTrace theory goal = 
  let 
    hypPolys = extractHypothesisPolys theory
    concPoly = extractConclusionPoly goal

    -- Build ALL characteristic sets (branches)
    branchesList = buildCharSet hypPolys

    -- Prove for each branch
    branchTraces = zipWith (proveBranch concPoly) [1..] branchesList

    allProved = all branchProved branchTraces
    reason = if allProved 
             then "Conclusion holds for all " ++ show (length branchTraces) ++ " branches."
             else "Conclusion failed on " ++ show (length (filter (not . branchProved) branchTraces)) ++ " branch(es)."
  in 
    WuTrace 
      { inputTheory = theory
      , inputGoal = goal
      , hypothesisPolys = hypPolys
      , conclusionPoly = concPoly
      , branches = branchTraces
      , isProved = allProved
      , proofReason = reason
      }

proveBranch :: Poly -> Int -> (CharSet, [DegeneracyCondition]) -> WuBranchTrace
proveBranch concPoly bid (charSet, degConds) = 
  let 
    (finalRem, steps) = pseudoRemainderWithSteps concPoly charSet
    proved = finalRem == polyZero
  in 
    WuBranchTrace 
      { branchId = bid
      , characteristicSet = charSet
      , degeneracyConditions = degConds
      , finalRemainder = finalRem
      , reductionSteps = steps
      , branchProved = proved
      }

-- =============================================
-- Characteristic Set Construction (Branching)
-- =============================================

-- | Build characteristic sets (triangular forms) from hypothesis polynomials.
-- Returns a list of (CS, Conditions), representing different algebraic branches.
buildCharSet :: [Poly] -> [(CharSet, [DegeneracyCondition])]
buildCharSet polys = 
  let 
    nonTrivial = filter (\p -> p /= polyZero && not (isConstPoly p)) polys
  in 
    triangularize nonTrivial

-- | Reduce a polynomial using Wu's Characteristic Set method.
-- Returns a list of remainders (one for each geometric branch).
reduceWithWu :: [Poly] -> Poly -> [Poly]
reduceWithWu constraints target =
  let 
    branches = buildCharSet constraints
  in 
    [ fst (pseudoRemainderWithSteps target cs) | (cs, _) <- branches ]

-- | Triangularize with Branching
triangularize :: [Poly] -> [(CharSet, [DegeneracyCondition])]
triangularize polys = 
  let 
    sorted = sortByMainVar polys
  in 
    triangularizeStep sorted []

-- | Recursive triangularization with factorization
triangularizeStep :: [Poly] -> [DegeneracyCondition] -> [(CharSet, [DegeneracyCondition])]
triangularizeStep [] degConds = [([], degConds)]
triangularizeStep [p] degConds = 
  -- Factor the last polynomial too!
  let factors = factorHeuristic p 
  in [ ([f], degConds) | f <- factors, not (isConstPoly f) ]
triangularizeStep (p:ps) degConds = 
  let 
    -- Factor the current polynomial p = f1 * f2 * ...
    factors = factorHeuristic p
    -- Process each factor as a separate branch
  in 
    concatMap (processFactor ps degConds) (filter (not . isConstPoly) factors)

processFactor :: [Poly] -> [DegeneracyCondition] -> Poly -> [(CharSet, [DegeneracyCondition])]
processFactor ps degConds factor = 
  let 
    -- Reduce remaining polynomials modulo this factor
    (reduced, newDegConds) = reduceList ps factor
    nonZero = filter (/= polyZero) reduced
    
    -- Recursively triangularize the rest
    restBranches = triangularizeStep nonZero (degConds ++ newDegConds)
  in 
    [ (factor : restCS, allConds) | (restCS, allConds) <- restBranches ]

reduceList :: [Poly] -> Poly -> ([Poly], [DegeneracyCondition])
reduceList polys divisor = 
  let 
    results = map (\p -> pseudoDivide p divisor) polys
    remainders = map fst results
    degConds = concatMap snd results
  in 
    (remainders, degConds)

-- =============================================
-- Pseudo-Division Algorithm (Unchanged)
-- =============================================

pseudoRemainder :: Poly -> Poly -> Poly
pseudoRemainder f g = fst (pseudoDivide f g)

pseudoDivide :: Poly -> Poly -> (Poly, [DegeneracyCondition])
pseudoDivide f g
  | g == polyZero = (f, [])
  | isConstPoly g = (polyZero, [])
  | otherwise = 
      let 
        varsF = getVars f
        varsG = getVars g
        commonVars = S.toList (S.intersection varsF varsG)
      in 
        if null commonVars 
        then (f, [])
        else 
          let 
            mainVar = maximum commonVars
            degF = polyDegreeIn f mainVar
            degG = polyDegreeIn g mainVar
          in 
            if degF < degG 
            then (f, [])
            else pseudoDivideStep f g mainVar degF degG

pseudoDivideStep :: Poly -> Poly -> String -> Int -> Int -> (Poly, [DegeneracyCondition])
pseudoDivideStep f g mainVar degF degG = 
  let 
    leadCoeffG = getLeadingCoeff g mainVar
    degCond = DegeneracyCondition 
      { leadingCoeff = leadCoeffG
      , varName = mainVar
      , source = "Pseudo-division by " ++ prettyPoly g
      }
    f' = polyMul (polyPow leadCoeffG (fromIntegral (degF - degG + 1))) f
    leadCoeffF = getLeadingCoeff f' mainVar
    degDiff = degF - degG
    multiplier = polyMul leadCoeffF (polyPow (polyFromVar mainVar) (fromIntegral degDiff))
    remainder = polySub f' (polyMul multiplier g)
  in 
    if polyDegreeIn remainder mainVar < degG 
    then (remainder, [degCond])
    else 
      let (finalRem, moreConds) = pseudoDivide remainder g
      in (finalRem, degCond : moreConds)

pseudoRemainderWithSteps :: Poly -> CharSet -> (Poly, [(String, Poly)])
pseudoRemainderWithSteps f [] = (f, [("Initial polynomial", f)])
pseudoRemainderWithSteps f (g:gs) = 
  let 
    (rem1, _) = pseudoDivide f g
    step1 = ("Reduced modulo " ++ prettyPoly g, rem1)
    (finalRem, moreSteps) = pseudoRemainderWithSteps rem1 gs
  in 
    (finalRem, step1 : moreSteps)

-- =============================================
-- Polynomial Utilities (Unchanged)
-- =============================================

sortByMainVar :: [Poly] -> [Poly]
sortByMainVar = sortBy (comparing getMainVar)


polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var = 
  if M.null m 
  then 0 
  else maximum (0 : [ fromIntegral (M.findWithDefault 0 var mono) 
                    | (Monomial mono, _) <- M.toList m ])

getLeadingCoeff :: Poly -> String -> Poly
getLeadingCoeff p var = 
  let deg = polyDegreeIn p var 
  in if deg == 0 
     then p 
     else extractCoeffOfDegree p var deg

extractCoeffOfDegree :: Poly -> String -> Int -> Poly
extractCoeffOfDegree (Poly m) var deg = 
  Poly $ M.fromList 
    [ (Monomial (M.delete var mono), coeff) 
    | (Monomial mono, coeff) <- M.toList m 
    , M.findWithDefault 0 var mono == fromIntegral deg 
    ]

-- =============================================
-- Extraction Utilities
-- =============================================

extractHypothesisPolys :: Theory -> [Poly]
extractHypothesisPolys theory = 
  let subMap = buildSubMap theory 
  in [ toPolySub subMap (Sub l r) | Eq l r <- theory ]

extractConclusionPoly :: Formula -> Poly
extractConclusionPoly (Eq l r) = 
  let subMap = M.empty 
  in toPolySub subMap (Sub l r)
extractConclusionPoly _ = 
  error "Wu's method only supports equality goals (not inequalities)"

-- =============================================
-- Degeneracy Checking
-- =============================================

checkDegeneracy :: [DegeneracyCondition] -> [String]
checkDegeneracy degConds = 
  [ "WARNING: Leading coefficient must be non-zero: " ++ prettyPoly (leadingCoeff cond)
  | cond <- degConds
  , leadingCoeff cond /= polyFromConst 1 
  ]

-- =============================================
-- Existence Check (Adapted for Branching)
-- =============================================

proveExistentialWu :: Theory -> Formula -> (Bool, String, WuTrace)
proveExistentialWu theory goal = 
  case goal of 
    Exists qs body -> 
      let 
        eqsBranches = extractEqualitiesBranches body
        theoryEqs = [ Sub l r | Eq l r <- theory ]
        existentialVars = map qvName qs
        rename v | v `elem` existentialVars = "zz_" ++ v
                 | otherwise = v
        renamePoly (Poly m) = 
            let renameMonomial (Monomial vars) = Monomial (M.mapKeys rename vars)
            in Poly (M.mapKeys renameMonomial m)

        checkBranch eqs = 
            let rawPolys = map (toPolySub M.empty) (eqs ++ theoryEqs)
                renamedPolys = map renamePoly rawPolys
                -- Get ALL branches
                branches = buildCharSet renamedPolys
                
                -- Check if ANY branch is consistent
                -- A branch is consistent if its CS does not contain a non-zero constant
                consistentBranches = filter (\(cs,_) -> not (any isConstantNonZero cs)) branches
            in (not (null consistentBranches), consistentBranches)

        results = map checkBranch eqsBranches
        anyConsistent = any fst results
        
        -- Trace logic for existential is complicated with branching.
        -- We just return the first consistent branch's info for now.
        dummyTrace = case [ b | (ok, bs) <- results, ok, b <- bs ] of 
            [] -> WuTrace [] (Eq (Const 0) (Const 0)) [] polyZero [] False "No solution"
            ((cs, conds):_) -> 
                 WuTrace 
                  { inputTheory = theory
                  , inputGoal = goal
                  , hypothesisPolys = [] 
                  , conclusionPoly = polyZero
                  , branches = [WuBranchTrace 1 cs conds polyZero [] True]
                  , isProved = True
                  , proofReason = "Solution exists"
                  }

        reason = if anyConsistent 
                 then "System is triangularizable (consistent). Solution exists generically."
                 else "System is inconsistent. No solution."
      in (anyConsistent, reason, dummyTrace)
    _ -> (False, "Wu existence only supports Exists quantification.", WuTrace [] (Eq (Const 0) (Const 0)) [] polyZero [] False "")

extractEqualitiesBranches :: Formula -> [[Expr]]
extractEqualitiesBranches (Eq l r) = [[Sub l r]]
extractEqualitiesBranches (And f1 f2) = 
    [ e1 ++ e2 | e1 <- extractEqualitiesBranches f1, e2 <- extractEqualitiesBranches f2 ]
extractEqualitiesBranches (Or f1 f2) = 
    extractEqualitiesBranches f1 ++ extractEqualitiesBranches f2
extractEqualitiesBranches (Exists _ f) = extractEqualitiesBranches f
extractEqualitiesBranches _ = [[]] 

isConstantNonZero :: Poly -> Bool
isConstantNonZero (Poly m) = 
  case M.toList m of 
    [(Monomial vars, c)] | M.null vars -> c /= 0
    _ -> False

formatWuTrace :: WuTrace -> String
formatWuTrace trace = unlines $ 
  [ "=== Wu's Method Trace ==="
  , "Branching Factor: " ++ show (length (branches trace))
  , ""
  ] ++ map formatBranch (branches trace) ++ 
  [ ""
  , "Overall Result: " ++ if isProved trace then "[PROVED]" else "[NOT PROVED]"
  , "Reason: " ++ proofReason trace
  ]

formatBranch :: WuBranchTrace -> String
formatBranch b = unlines $ 
  [ "--- Branch " ++ show (branchId b) ++ " ---"
  , "Characteristic Set:"
  ] ++ 
  [ "  CS" ++ show i ++ ": " ++ prettyPoly p 
  | (i, p) <- zip [1 :: Int ..] (characteristicSet b)
  ] ++ 
  [ "Pseudo-Division:"
  ] ++ 
  [ "  " ++ desc ++ "\n    -> " ++ prettyPoly poly 
  | (desc, poly) <- reductionSteps b 
  ] ++ 
  [ "Remainder: " ++ prettyPoly (finalRemainder b)
  , "Status: " ++ if branchProved b then "Proved" else "Failed"
  ] ++ 
  (if null (degeneracyConditions b) then [] else 
    ["Degeneracy Conditions:"] ++ checkDegeneracy (degeneracyConditions b))