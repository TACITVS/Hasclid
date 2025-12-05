{-|
Module: Wu
Description: Wu's Method (Characteristic Sets) for Geometric Theorem Proving

Wu's Method is specifically designed for geometric theorem proving using
the characteristic set method. Unlike Gröbner bases which work in any
polynomial ring, Wu's method exploits the structure of geometric problems
by computing characteristic sets (triangular polynomial sets).

Key advantages for geometry:
- Much faster than Gröbner bases for geometric problems
- Natural handling of degeneracy conditions
- Efficient triangularization algorithm
- Works particularly well with coordinate geometry

Algorithm Overview:
1. Triangularization: Order polynomials by leading variable
2. Pseudo-division: Reduce polynomial modulo characteristic set
3. Zero test: Goal reduces to zero ⟹ theorem is true (under non-degeneracy)

References:
- Wu, W.-T. (1978) "On the Decision Problem and the Mechanization
  of Theorem-Proving in Elementary Geometry"
- Chou, S.-C. (1988) "Mechanical Geometry Theorem Proving"
-}

module Wu
  ( -- * Main Wu Proof Function
    wuProve
  , wuProveWithTrace
  , proveExistentialWu -- New function for existential checking

    -- * Characteristic Set Construction
  , CharSet
  , buildCharSet
  , triangularize

    -- * Pseudo-Division
  , pseudoRemainder
  , pseudoDivide

    -- * Degeneracy Conditions
  , DegeneracyCondition(..)
  , checkDegeneracy

    -- * Wu Proof Trace (for debugging/verbose mode)
  , WuTrace(..)
  , formatWuTrace
  ) where

import Expr
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import Numeric.Natural (Natural)

-- =============================================
-- Data Types
-- =============================================

-- | A characteristic set is a triangular set of polynomials
-- ordered by their main variable (class)
type CharSet = [Poly]

-- | Degeneracy conditions that must hold for the proof to be valid
data DegeneracyCondition = DegeneracyCondition
  { leadingCoeff :: Poly      -- The leading coefficient that must be non-zero
  , varName :: String          -- The variable this coefficient depends on
  , source :: String           -- Description of where this condition came from
  } deriving (Show, Eq)

-- | Trace of Wu's method execution (for verbose mode)
data WuTrace = WuTrace
  { inputTheory :: Theory
  , inputGoal :: Formula
  , hypothesisPolys :: [Poly]
  , conclusionPoly :: Poly
  , characteristicSet :: CharSet
  , degeneracyConditions :: [DegeneracyCondition]
  , finalRemainder :: Poly
  , reductionSteps :: [(String, Poly)]  -- (description, intermediate result)
  , isProved :: Bool
  , proofReason :: String
  } deriving (Show, Eq)

-- =============================================
-- Main Wu Proving Functions
-- =============================================

-- | Prove a formula using Wu's method
-- Returns: (is_proved, reason)
wuProve :: Theory -> Formula -> (Bool, String)
wuProve theory goal =
  let trace = wuProveWithTrace theory goal
  in (isProved trace, proofReason trace)

-- | Prove with full trace information (for debugging/verbose mode)
wuProveWithTrace :: Theory -> Formula -> WuTrace
wuProveWithTrace theory goal =
  let
    -- 1. Extract hypothesis polynomials (assumptions)
    hypPolys = extractHypothesisPolys theory

    -- 2. Extract conclusion polynomial (goal)
    concPoly = extractConclusionPoly goal

    -- 3. Build characteristic set from hypotheses
    (charSet, degConds) = buildCharSet hypPolys

    -- 4. Reduce conclusion modulo characteristic set
    (finalRem, steps) = pseudoRemainderWithSteps concPoly charSet

    -- 5. Check if remainder is zero
    proved = finalRem == polyZero
    reason = if proved
             then "Conclusion reduces to zero under characteristic set"
             else "Conclusion does not reduce to zero (remainder: " ++
                  prettyPoly finalRem ++ ")"
  in
    WuTrace
      { inputTheory = theory
      , inputGoal = goal
      , hypothesisPolys = hypPolys
      , conclusionPoly = concPoly
      , characteristicSet = charSet
      , degeneracyConditions = degConds
      , finalRemainder = finalRem
      , reductionSteps = steps
      , isProved = proved
      , proofReason = reason
      }

-- =============================================
-- Characteristic Set Construction
-- =============================================

-- | Build a characteristic set (triangular form) from hypothesis polynomials
buildCharSet :: [Poly] -> (CharSet, [DegeneracyCondition])
buildCharSet polys =
  let
    -- Remove zero polynomials and constants
    nonTrivial = filter (\p -> p /= polyZero && not (isConstPoly p)) polys

    -- Triangularize: reduce to triangular form
    (triPolys, degConds) = triangularize nonTrivial
  in
    (triPolys, degConds)

-- | Triangularize a set of polynomials into characteristic set form
-- Uses successive pseudo-division to eliminate variables
triangularize :: [Poly] -> (CharSet, [DegeneracyCondition])
triangularize polys =
  let
    -- Sort by main variable (lexicographic ordering)
    sorted = sortByMainVar polys

    -- Iteratively reduce to triangular form
    (triSet, degConds) = triangularizeStep sorted []
  in
    (triSet, degConds)

-- | One step of triangularization
triangularizeStep :: [Poly] -> [DegeneracyCondition] -> (CharSet, [DegeneracyCondition])
triangularizeStep [] degConds = ([], degConds)
triangularizeStep [p] degConds = ([p], degConds)
triangularizeStep (p:ps) degConds =
  let
    -- Reduce all remaining polynomials modulo p
    (reduced, newDegConds) = reduceList ps p

    -- Remove zeros after reduction
    nonZero = filter (/= polyZero) reduced

    -- Recursively triangularize the reduced set
    (restSet, allDegConds) = triangularizeStep nonZero (degConds ++ newDegConds)
  in
    (p : restSet, allDegConds)

-- | Reduce a list of polynomials modulo a given polynomial
reduceList :: [Poly] -> Poly -> ([Poly], [DegeneracyCondition])
reduceList polys divisor =
  let
    results = map (\p -> pseudoDivide p divisor) polys
    remainders = map fst results
    degConds = concatMap snd results
  in
    (remainders, degConds)

-- =============================================
-- Pseudo-Division Algorithm
-- =============================================

-- | Compute pseudo-remainder of f modulo g
-- This is the key operation in Wu's method
pseudoRemainder :: Poly -> Poly -> Poly
pseudoRemainder f g = fst (pseudoDivide f g)

-- | Pseudo-division with degeneracy condition tracking
-- Returns: (remainder, degeneracy_conditions)
pseudoDivide :: Poly -> Poly -> (Poly, [DegeneracyCondition])
pseudoDivide f g
  | g == polyZero = (f, [])  -- Division by zero: no reduction possible
  | isConstPoly g = (polyZero, [])  -- Constant divisor: reduces to zero
  | otherwise =
      let
        -- Get main variables
        varsF = getVars f
        varsG = getVars g

        -- If f and g have no common variables, no reduction possible
        commonVars = S.toList (S.intersection varsF varsG)
      in
        if null commonVars
        then (f, [])  -- No reduction possible
        else
          let
            -- Choose main variable (largest in lexicographic order)
            mainVar = maximum commonVars

            -- Get degrees
            degF = polyDegreeIn f mainVar
            degG = polyDegreeIn g mainVar
          in
            if degF < degG
            then (f, [])  -- f has lower degree: no reduction possible
            else
              -- Perform pseudo-division
              pseudoDivideStep f g mainVar degF degG

-- | Single step of pseudo-division
pseudoDivideStep :: Poly -> Poly -> String -> Int -> Int -> (Poly, [DegeneracyCondition])
pseudoDivideStep f g mainVar degF degG =
  let
    -- Get leading coefficient of g
    leadCoeffG = getLeadingCoeff g mainVar

    -- Create degeneracy condition
    degCond = DegeneracyCondition
      { leadingCoeff = leadCoeffG
      , varName = mainVar
      , source = "Pseudo-division by " ++ prettyPoly g
      }

    -- Multiply f by leadCoeffG to prepare for subtraction
    f' = polyMul (polyPow leadCoeffG (fromIntegral (degF - degG + 1))) f

    -- Compute leading term to eliminate
    leadCoeffF = getLeadingCoeff f' mainVar
    degDiff = degF - degG
    multiplier = polyMul leadCoeffF (polyPow (polyFromVar mainVar) (fromIntegral degDiff))

    -- Subtract: f' - multiplier * g
    remainder = polySub f' (polyMul multiplier g)
  in
    -- Check if we need to continue reducing
    if polyDegreeIn remainder mainVar < degG
    then (remainder, [degCond])
    else
      let (finalRem, moreConds) = pseudoDivide remainder g
      in (finalRem, degCond : moreConds)

-- | Pseudo-remainder with step-by-step trace
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
-- Polynomial Utilities for Wu's Method
-- =============================================

-- | Sort polynomials by their main variable (lexicographic)
sortByMainVar :: [Poly] -> [Poly]
sortByMainVar = sortBy (comparing getMainVar)

-- | Get the main variable of a polynomial (largest in lex order)
getMainVar :: Poly -> String
getMainVar p =
  let vars = S.toList (getVars p)
  in if null vars then "" else maximum vars

-- | Get all variables in a polynomial
getVars :: Poly -> S.Set String
getVars (Poly m) =
  S.fromList [ v | (Monomial mono, _) <- M.toList m, (v, _) <- M.toList mono ]

-- | Check if polynomial is constant
isConstPoly :: Poly -> Bool
isConstPoly p = p == polyZero || S.null (getVars p)

-- | Get degree of polynomial in a specific variable
polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  if M.null m
  then 0
  else maximum (0 : [ fromIntegral (M.findWithDefault 0 var mono)
                    | (Monomial mono, _) <- M.toList m ])

-- | Get leading coefficient with respect to a variable
-- This returns the coefficient of the highest power of var
getLeadingCoeff :: Poly -> String -> Poly
getLeadingCoeff p var =
  let deg = polyDegreeIn p var
  in if deg == 0
     then p  -- No occurrence of var, entire poly is the "leading coeff"
     else extractCoeffOfDegree p var deg

-- | Extract coefficient of a specific degree of a variable
extractCoeffOfDegree :: Poly -> String -> Int -> Poly
extractCoeffOfDegree (Poly m) var deg =
  Poly $ M.fromList
    [ (Monomial (M.delete var mono), coeff)
    | (Monomial mono, coeff) <- M.toList m
    , M.findWithDefault 0 var mono == fromIntegral deg
    ]

-- =============================================
-- Hypothesis and Conclusion Extraction
-- =============================================

-- | Extract hypothesis polynomials from theory (assumptions)
extractHypothesisPolys :: Theory -> [Poly]
extractHypothesisPolys theory =
  let subMap = buildSubMap theory
  in [ toPolySub subMap (Sub l r) | Eq l r <- theory ]

-- | Extract conclusion polynomial from goal formula
extractConclusionPoly :: Formula -> Poly
extractConclusionPoly (Eq l r) =
  let subMap = M.empty  -- No substitutions for goal
  in toPolySub subMap (Sub l r)
extractConclusionPoly _ =
  error "Wu's method only supports equality goals (not inequalities)"

-- =============================================
-- Degeneracy Checking
-- =============================================

-- | Check if degeneracy conditions might be violated
checkDegeneracy :: [DegeneracyCondition] -> [String]
checkDegeneracy degConds =
  [ "WARNING: Leading coefficient must be non-zero: " ++ prettyPoly (leadingCoeff cond)
  | cond <- degConds
  , leadingCoeff cond /= polyFromConst 1  -- Only warn for non-trivial conditions
  ]

-- | Check existential goal using Wu's Method (Triangularization)
-- Checks if the system of equations defined by the existential body + theory is consistent.
-- Renames existential variables to be "main" variables.
proveExistentialWu :: Theory -> Formula -> (Bool, String, WuTrace)
proveExistentialWu theory goal =
  case goal of
    Exists qs body ->
      let
        -- 1. Extract equations
        eqsBranches = extractEqualitiesBranches body
        
        -- 2. Theory equations
        theoryEqs = [ Sub l r | Eq l r <- theory ]
        
        -- 3. Variable renaming (qs > free vars)
        existentialVars = map qvName qs
        rename v | v `elem` existentialVars = "zz_" ++ v
                 | otherwise = v
        
        renamePoly :: Poly -> Poly
        renamePoly (Poly m) =
            let renameMonomial (Monomial vars) = 
                    Monomial (M.mapKeys rename vars)
            in Poly (M.mapKeys renameMonomial m)

        checkBranch eqs = 
            let rawPolys = map (toPolySub M.empty) (eqs ++ theoryEqs)
                renamedPolys = map renamePoly rawPolys
                
                -- Triangularize
                (charSet, degConds) = buildCharSet renamedPolys
                
                -- Check consistency (is constant non-zero in CS?)
                isInconsistent = any isConstantNonZero charSet
            in (not isInconsistent, charSet, degConds)

        -- 4. Check consistency of EACH branch
        results = map checkBranch eqsBranches
        anyConsistent = any (\(c,_,_) -> c) results
        
        -- 5. Trace (take first consistent or first if none)
        (consistent, bestCS, bestConds) = 
            case filter (\(c,_,_) -> c) results of
                (r:_) -> r
                [] -> head results
        
        reason = if anyConsistent
                 then "System is triangularizable (consistent). Solution exists generically."
                 else "System is inconsistent (characteristic set contains constant). No solution."
                 
        -- Dummy trace object for compatibility
        trace = WuTrace
          { inputTheory = theory
          , inputGoal = goal
          , hypothesisPolys = [] -- We abstracted this
          , conclusionPoly = polyZero
          , characteristicSet = bestCS
          , degeneracyConditions = bestConds
          , finalRemainder = polyZero
          , reductionSteps = []
          , isProved = anyConsistent
          , proofReason = reason
          }
      in (anyConsistent, reason, trace)
    _ -> (False, "Wu existence only supports Exists quantification.", emptyTrace)

-- Helper (duplicated from Prover.hs to avoid cycle, or move to Expr)
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

emptyTrace :: WuTrace
emptyTrace = WuTrace [] (Eq (Const 0) (Const 0)) [] polyZero [] [] polyZero [] False ""

-- | Format Wu's method trace for display
formatWuTrace :: WuTrace -> String
formatWuTrace trace = unlines $
  [ "=== Wu's Method Trace ==="
  , ""
  , "Hypothesis Polynomials:"
  ] ++
  [ "  " ++ show i ++ ". " ++ prettyPoly p
  | (i, p) <- zip [1..] (hypothesisPolys trace)
  ] ++
  [ ""
  , "Conclusion Polynomial:"
  , "  " ++ prettyPoly (conclusionPoly trace)
  , ""
  , "Characteristic Set (Triangular Form):"
  ] ++
  [ "  CS" ++ show i ++ ": " ++ prettyPoly p
  | (i, p) <- zip [1..] (characteristicSet trace)
  ] ++
  [ ""
  , "Pseudo-Division Steps:"
  ] ++
  [ "  " ++ desc ++ "\n    → " ++ prettyPoly poly
  | (desc, poly) <- reductionSteps trace
  ] ++
  [ ""
  , "Final Remainder: " ++ prettyPoly (finalRemainder trace)
  , ""
  , "Result: " ++ if isProved trace then "✓ PROVED" else "✗ NOT PROVED"
  , "Reason: " ++ proofReason trace
  ] ++
  (if null (degeneracyConditions trace)
   then []
   else [ ""
        , "Degeneracy Conditions:"
        ] ++ checkDegeneracy (degeneracyConditions trace))
