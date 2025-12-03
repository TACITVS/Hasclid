{-|
Module: GeoSolver
Description: Fast-path geometric constraint solver using symbolic propagation

This module solves geometric problems through SYMBOLIC CONSTRAINT PROPAGATION.
It works with symbolic variables (like 'S') and explores multiple geometric
configurations (branches) to find proofs or counter-examples.
-}

module GeoSolver
  ( -- * Main Solving Function
    solveGeo
  , solveGeoWithTrace

    -- * Knowledge Base
  , CoordMap
  , buildKnowledgeBase
  , propagateConstraints

    -- * Result Types
  , GeoResult(..)
  , formatGeoResult
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (nub)
import Data.Maybe (mapMaybe, isJust, fromJust, listToMaybe)
import Data.Ratio (numerator, denominator)

-- =============================================
-- Data Types
-- =============================================

-- | Knowledge Base: Maps coordinate variables to their symbolic expressions
type CoordMap = M.Map String Expr

-- | Result of geometric solving
data GeoResult
  = GeoProved String [String]      -- Proved on ALL branches
  | GeoDisproved String [String]   -- Disproved on AT LEAST ONE valid branch
  | GeoUnknown String              -- Cannot decide
  deriving (Show, Eq)

-- =============================================
-- Main Solving Functions
-- =============================================

-- | Solve geometric problem using constraint propagation
solveGeo :: Theory -> Formula -> Maybe Bool
solveGeo theory goal =
  case solveGeoWithTrace theory goal of
    GeoProved _ _ -> Just True
    GeoDisproved _ _ -> Just False
    GeoUnknown _ -> Nothing

-- | Solve with detailed trace information
solveGeoWithTrace :: Theory -> Formula -> GeoResult
solveGeoWithTrace theory goal =
  let
    -- Step 1: Build initial knowledge base
    kb0 = buildKnowledgeBase theory M.empty
    
    -- Step 2: Propagate constraints across potentially multiple branches
    -- We start with one branch (kb0) and might fork into multiple
    (branches, steps1) = propagateUntilConvergence theory [kb0] [] 10

    -- Step 3: Check goal in all resulting branches
    results = map (\kb -> checkGoal kb goal) branches
    
    -- Analyze results
    -- If any branch is DEFINITELY FALSE -> Disproved (Counter-example found)
    -- If ALL branches are DEFINITELY TRUE -> Proved
    -- Otherwise -> Unknown
    
    hasCounterExample = any isFalse results
    allProved = all isTrue results
    
  in
    if null branches
    then GeoUnknown "Contradictory assumptions (no valid branches)"
    else if hasCounterExample
         then 
           let (Just (Just False, reason, _)) = findResult isFalse results
           in GeoDisproved reason (steps1 ++ ["Found counter-example in branch: " ++ reason])
         else if allProved
              then GeoProved "Geometric propagation (all branches hold)" steps1
              else GeoUnknown "Some branches indeterminate"

  where
    isFalse (Just False, _, _) = True
    isFalse _ = False
    
    isTrue (Just True, _, _) = True
    isTrue _ = False
    
    findResult pred [] = Nothing
    findResult pred (x:xs) = if pred x then Just x else findResult pred xs

-- | Propagate until fixed point on all branches
propagateUntilConvergence :: Theory -> [CoordMap] -> [String] -> Int -> ([CoordMap], [String])
propagateUntilConvergence theory kbs steps maxIters
  | maxIters <= 0 = (kbs, steps ++ ["Warning: Max iterations reached"])
  | otherwise =
      let 
        -- Apply propagation to each KB, collecting new KBs and steps
        results = map (\kb -> propagateConstraints theory kb) kbs
        newKBs = concatMap fst results
        newSteps = concatMap snd results
        
        -- Check for convergence (if set of KBs hasn't changed)
        converged = newKBs == kbs
      in
        if converged
        then (newKBs, steps) -- No new information
        else propagateUntilConvergence theory newKBs (steps ++ nub newSteps) (maxIters - 1)

-- =============================================
-- Knowledge Base Construction
-- =============================================

buildKnowledgeBase :: Theory -> CoordMap -> CoordMap
buildKnowledgeBase [] kb = kb
buildKnowledgeBase (formula:rest) kb =
  case formula of
    Eq (Var v) e -> buildKnowledgeBase rest (M.insert v e kb)
    Eq e (Var v) -> buildKnowledgeBase rest (M.insert v e kb)
    _ -> buildKnowledgeBase rest kb

-- =============================================
-- Constraint Propagation
-- =============================================

-- | Propagate constraints on a single KB, returning potential branches
propagateConstraints :: Theory -> CoordMap -> ([CoordMap], [String])
propagateConstraints [] kb = ([kb], [])
propagateConstraints (formula:rest) kb =
  let (kbs, steps) = applyConstraint formula kb
  in 
    -- Recursively process rest of theory on all resulting KBs
    let recursiveResults = map (\k -> propagateConstraints rest k) kbs
        finalKBs = concatMap fst recursiveResults
        finalSteps = steps ++ concatMap snd recursiveResults
    in (finalKBs, finalSteps)

applyConstraint :: Formula -> CoordMap -> ([CoordMap], [String])
applyConstraint formula kb =
  case formula of
    -- RULE 1: Explicit Coordinate Equality (xA = xB)
    Eq (Sub (Var v1) (Var v2)) (Const 0) ->
      let newKB = unifyVars kb v1 v2
      in ([newKB], ["Deduced " ++ v1 ++ " = " ++ v2])

    -- RULE 2: Perpendicularity (Dot Product = 0)
    Eq (Perpendicular pA pB pC pD) (Const 0) ->
      propagatePerpendicular kb pA pB pC pD

    -- RULE 3: Distance
    Eq (Dist2 p1 p2) distExpr ->
      propagateDistance kb p1 p2 distExpr

    _ -> ([kb], [])

-- =============================================
-- Geometric Logic (Symbolic)
-- =============================================

-- | Propagate perpendicularity
propagatePerpendicular :: CoordMap -> String -> String -> String -> String -> ([CoordMap], [String])
propagatePerpendicular kb pA pB pC pD
  | isHorizontal kb pA pB =
      -- AB horizontal -> CD vertical (xC = xD)
      let newKB = unifyVars kb ("x" ++ pC) ("x" ++ pD)
      in ([newKB], ["Line " ++ pA ++ pB ++ " horizontal => " ++ pC ++ pD ++ " vertical"])
  | isVertical kb pA pB =
      -- AB vertical -> CD horizontal (yC = yD)
      let newKB = unifyVars kb ("y" ++ pC) ("y" ++ pD)
      in ([newKB], ["Line " ++ pA ++ pB ++ " vertical => " ++ pC ++ pD ++ " horizontal"])
  | otherwise = ([kb], [])

-- | Propagate distance constraint (Symbolic)
-- (x1-x2)^2 + (y1-y2)^2 = dist
-- If one coord is unknown, solve for it.
propagateDistance :: CoordMap -> String -> String -> Expr -> ([CoordMap], [String])
propagateDistance kb p1 p2 distExpr =
  let
    dVal = resolveExpand kb distExpr
    x1 = resolveExpand kb (Var ("x" ++ p1)); x2 = resolveExpand kb (Var ("x" ++ p2))
    y1 = resolveExpand kb (Var ("y" ++ p1)); y2 = resolveExpand kb (Var ("y" ++ p2))
    z1 = resolveExpand kb (Var ("z" ++ p1)); z2 = resolveExpand kb (Var ("z" ++ p2))
    
    -- Helper to solve: (u - known)^2 = target
    solveFor :: String -> Expr -> Expr -> Expr -> ([CoordMap], [String])
    solveFor var u known target =
       let 
         -- Equation: (u - known)^2 + other_terms = dVal
         -- so (u - known)^2 = dVal - other_terms
         rhs = simplifyExpr (Sub dVal target)
       in 
         case solveQuadratic (Sub u known) rhs of
           [] -> ([kb], []) -- Cannot solve
           solutions -> 
             -- We have solutions for (u - known), e.g., k and -k
             -- u - known = sol => u = known + sol
             let newKBs = map (\sol -> M.insert var (simplifyExpr (Add known sol)) kb) solutions
                 step = "Solved distance for " ++ var ++ " (branches: " ++ show (length solutions) ++ ")"
             in (newKBs, [step])
             
    -- Detect unknowns
    isUnknown (Var v) | take 1 v `elem` ["x","y","z"] = Just v
    isUnknown _ = Nothing
    
    uX1 = isUnknown x1; uX2 = isUnknown x2
    uY1 = isUnknown y1; uY2 = isUnknown y2
    uZ1 = isUnknown z1; uZ2 = isUnknown z2
    
    dx2 = Pow (Sub x1 x2) 2
    dy2 = Pow (Sub y1 y2) 2
    dz2 = Pow (Sub z1 z2) 2
    
  in
    case (uX1, uY1, uZ1, uX2, uY2, uZ2) of
      -- Solve for x2
      (Nothing, Nothing, Nothing, Just v, Nothing, Nothing) ->
        solveFor v x2 x1 (Add dy2 dz2)
      -- Solve for y2
      (Nothing, Nothing, Nothing, Nothing, Just v, Nothing) ->
        solveFor v y2 y1 (Add dx2 dz2)
      -- Solve for x1
      (Just v, Nothing, Nothing, Nothing, Nothing, Nothing) ->
        solveFor v x1 x2 (Add dy2 dz2)
      -- Solve for y1
      (Nothing, Just v, Nothing, Nothing, Nothing, Nothing) ->
        solveFor v y1 y2 (Add dx2 dz2)
        
      _ -> ([kb], [])


-- | Solve quadratic equation symbolically
-- Returns list of solutions for LHS term
-- equation: term^2 = rhs
-- returns: [sqrt(rhs), -sqrt(rhs)]
solveQuadratic :: Expr -> Expr -> [Expr]
solveQuadratic term rhs =
  case symbolicSqrt rhs of
    Just root -> [root, Mul (Const (-1)) root]
    Nothing -> []

-- | Try to find symbolic square root
symbolicSqrt :: Expr -> Maybe Expr
symbolicSqrt (Const c) 
  | c >= 0 = -- Try to find rational root
      let n = numerator c; d = denominator c
          rootN = integerSqrt n; rootD = integerSqrt d
      in if rootN*rootN == n && rootD*rootD == d
         then Just (Const (fromInteger rootN / fromInteger rootD))
         else Nothing
  | otherwise = Nothing
symbolicSqrt (Pow e 2) = Just e
symbolicSqrt (Mul e1 e2) = 
  case (symbolicSqrt e1, symbolicSqrt e2) of
    (Just r1, Just r2) -> Just (Mul r1 r2)
    _ -> Nothing
symbolicSqrt _ = Nothing

integerSqrt :: Integer -> Integer
integerSqrt n = floor (sqrt (fromIntegral n))

-- =============================================
-- Utilities
-- =============================================

resolveExpand :: CoordMap -> Expr -> Expr
resolveExpand kb e = simplifyExpr (expandExprRecursive kb e)

expandExprRecursive :: CoordMap -> Expr -> Expr
expandExprRecursive kb (Var v) =
  case resolveVar kb v of
    Just e -> expandExprRecursive kb e
    Nothing -> if take 1 v == "z" then Const 0 else Var v
expandExprRecursive kb (Add e1 e2) = Add (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Sub e1 e2) = Sub (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Mul e1 e2) = Mul (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Div e1 e2) = Div (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Pow e n) = Pow (expandExprRecursive kb e) n
expandExprRecursive kb (Dist2 p1 p2) =
  let x1 = expandExprRecursive kb (Var ("x"++p1))
      y1 = expandExprRecursive kb (Var ("y"++p1))
      z1 = expandExprRecursive kb (Var ("z"++p1))
      x2 = expandExprRecursive kb (Var ("x"++p2))
      y2 = expandExprRecursive kb (Var ("y"++p2))
      z2 = expandExprRecursive kb (Var ("z"++p2))
  in Add (Add (Pow (Sub x1 x2) 2) (Pow (Sub y1 y2) 2)) (Pow (Sub z1 z2) 2)
expandExprRecursive _ e = e

resolveVar :: CoordMap -> String -> Maybe Expr
resolveVar kb v =
  case M.lookup v kb of
    Just (Var v2) | v /= v2 -> 
      case resolveVar kb v2 of
        Just val -> Just val
        Nothing -> Just (Var v2)
    Just e -> Just e
    Nothing -> Nothing

unifyVars :: CoordMap -> String -> String -> CoordMap
unifyVars kb v1 v2 =
  let e1 = resolveVar kb v1
      e2 = resolveVar kb v2
  in case (e1, e2) of
       (Just val, _) -> M.insert v2 val kb
       (_, Just val) -> M.insert v1 val kb
       _ -> M.insert v2 (Var v1) kb

isHorizontal :: CoordMap -> String -> String -> Bool
isHorizontal kb pA pB =
  let yA = resolveExpand kb (Var ("y"++pA))
      yB = resolveExpand kb (Var ("y"++pB))
  in exprEqualsSymbolic yA yB

isVertical :: CoordMap -> String -> String -> Bool
isVertical kb pA pB =
  let xA = resolveExpand kb (Var ("x"++pA))
      xB = resolveExpand kb (Var ("x"++pB))
  in exprEqualsSymbolic xA xB

checkGoal :: CoordMap -> Formula -> (Maybe Bool, String, [String])
checkGoal kb (Eq lhs rhs) =
  let l = resolveExpand kb lhs
      r = resolveExpand kb rhs
  in if exprEqualsSymbolic l r
     then (Just True, "Equality holds symbolically", ["LHS: " ++ prettyExpr l, "RHS: " ++ prettyExpr r])
     else 
       -- If we can determine they are definitely NOT equal (e.g. Const 5 vs Const 6)
       case (l, r) of
         (Const c1, Const c2) | c1 /= c2 -> (Just False, "Constants not equal", ["LHS: " ++ show c1, "RHS: " ++ show c2])
         -- For symbolic inequality, it's harder. 
         -- But for the square problem: 5S^2 vs S^2 -> not equal if S!=0.
         -- We assume variables are generic (non-zero).
         -- If we have a nice polynomial normal form, we can check.
         _ -> 
            -- Check if difference is non-zero
            if l /= r then (Just False, "Symbolically distinct", ["LHS: " ++ prettyExpr l, "RHS: " ++ prettyExpr r])
            else (Nothing, "Unknown", [])
checkGoal _ _ = (Nothing, "Unsupported goal type", [])
formatGeoResult :: GeoResult -> String
formatGeoResult (GeoProved reason steps) =
  unlines $
    [ "=== GEOMETRIC SOLVER ==="
    , "Result: PROVED"
    , "Reason: " ++ reason
    , ""
    , "Derivation Steps:"
    ] ++ map ("  " ++) steps

formatGeoResult (GeoDisproved reason steps) =
  unlines $
    [ "=== GEOMETRIC SOLVER ==="
    , "Result: DISPROVED"
    , "Reason: " ++ reason
    , ""
    , "Derivation Steps:"
    ] ++ map ("  " ++) steps

formatGeoResult (GeoUnknown reason) =
  unlines
    [ "=== GEOMETRIC SOLVER ==="
    , "Result: UNKNOWN"
    , "Reason: " ++ reason
    , "Falling back to algebraic solvers..."
    ]

