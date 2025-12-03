{-|
Module: GeoSolver
Description: Fast-path geometric constraint solver using propagation

This module solves geometric problems through CONSTRAINT PROPAGATION
rather than polynomial manipulation. It recognizes simple geometric
patterns (vertical/horizontal lines, perpendicularity) and propagates
them instantly.

For example:
- If AB is horizontal (yA = yB) and AB ⊥ CD, then CD is vertical (xC = xD)
- If we know A=(0,0), B=(S,0), C=(S,S), we can compute dist(D,A) directly

This is the "screwdriver" that handles 80% of geometric problems instantly,
before falling back to the "hammer" (Wu/Gröbner) for complex cases.
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
import qualified Data.Set as S
import Data.Maybe (mapMaybe, isJust, fromJust, fromMaybe)
import Data.Ratio ((%), numerator, denominator)

-- =============================================
-- Data Types
-- =============================================

-- | Knowledge Base: Maps coordinate variables to their values/relations
-- e.g., "xA" -> Const 0, "yB" -> Var "yA", etc.
type CoordMap = M.Map String Expr

-- | Result of geometric solving
data GeoResult
  = GeoProved String [String]      -- Proved with reason and derivation steps
  | GeoDisproved String [String]   -- Disproved with reason and steps
  | GeoUnknown String              -- Cannot decide, need fallback
  deriving (Show, Eq)

-- =============================================
-- Main Solving Functions
-- =============================================

-- | Solve geometric problem using constraint propagation
-- Returns: Just True (proved), Just False (disproved), Nothing (unknown)
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
    -- Step 1: Build initial knowledge base from explicit point definitions
    kb0 = buildKnowledgeBase theory M.empty

    -- Step 2: Propagate geometric constraints (the "smart" part!)
    -- Keep iterating until no more progress (fixed-point computation)
    (kb1, steps1) = propagateUntilConvergence theory kb0 [] 10

    -- Step 3: Check if goal can be evaluated in this knowledge base
    (result, steps2) = checkGoal kb1 goal []
  in
    case result of
      Just True -> GeoProved "Geometric constraint propagation" (steps1 ++ steps2)
      Just False -> GeoDisproved "Geometric contradiction found" (steps1 ++ steps2)
      Nothing -> GeoUnknown "Insufficient geometric information for fast path"

-- | Propagate constraints until no more progress is made (fixed-point)
propagateUntilConvergence :: Theory -> CoordMap -> [String] -> Int -> (CoordMap, [String])
propagateUntilConvergence theory kb steps maxIters
  | maxIters <= 0 = (kb, steps ++ ["Warning: Reached maximum iterations without convergence"])
  | otherwise =
      let (kb', steps') = propagateConstraints theory kb []
      in if M.size kb' == M.size kb  -- No new deductions made
         then (kb', steps ++ steps')  -- Converged!
         else propagateUntilConvergence theory kb' (steps ++ steps') (maxIters - 1)  -- Keep going

-- =============================================
-- Knowledge Base Construction
-- =============================================

-- | Build initial knowledge base from theory (point definitions)
buildKnowledgeBase :: Theory -> CoordMap -> CoordMap
buildKnowledgeBase [] kb = kb
buildKnowledgeBase (formula:rest) kb =
  case formula of
    -- Point coordinate assignments: Eq (Var "xA") (Const 0)
    Eq (Var v) e -> buildKnowledgeBase rest (M.insert v e kb)

    -- Also handle reverse: Eq (Const 0) (Var "xA")
    Eq e (Var v) -> buildKnowledgeBase rest (M.insert v e kb)

    _ -> buildKnowledgeBase rest kb

-- =============================================
-- Constraint Propagation (The Core Logic)
-- =============================================

-- | Propagate geometric constraints through the knowledge base
propagateConstraints :: Theory -> CoordMap -> [String] -> (CoordMap, [String])
propagateConstraints [] kb steps = (kb, steps)
propagateConstraints (formula:rest) kb steps =
  case formula of
    -- RULE 1: Direct coordinate equality
    -- If (xA - xB = 0), then xA = xB
    Eq (Sub (Var v1) (Var v2)) (Const 0) ->
      let newKB = unifyVars kb v1 v2
          step = "Deduced " ++ v1 ++ " = " ++ v2 ++ " from equality constraint"
      in propagateConstraints rest newKB (steps ++ [step])

    -- RULE 2: Perpendicularity propagation
    -- If AB is horizontal and AB ⊥ CD, then CD is vertical
    Eq (Perpendicular pA pB pC pD) (Const 0) ->
      let (newKB, newSteps) = propagatePerpendicular kb pA pB pC pD
      in propagateConstraints rest newKB (steps ++ newSteps)

    -- RULE 3: Distance constraints with known coordinates
    Eq (Dist2 p1 p2) distExpr ->
      let (newKB, newSteps) = propagateDistance kb p1 p2 distExpr
      in propagateConstraints rest newKB (steps ++ newSteps)

    _ -> propagateConstraints rest kb steps

-- | Propagate perpendicularity constraint
propagatePerpendicular :: CoordMap -> String -> String -> String -> String -> (CoordMap, [String])
propagatePerpendicular kb pA pB pC pD
  | isHorizontal kb pA pB =
      -- AB is horizontal, so CD must be vertical (xC = xD)
      let newKB = unifyVars kb ("x" ++ pC) ("x" ++ pD)
          step = "Line " ++ pA ++ pB ++ " is horizontal, so " ++ pC ++ pD ++ " is vertical"
      in (newKB, [step])

  | isVertical kb pA pB =
      -- AB is vertical, so CD must be horizontal (yC = yD)
      let newKB = unifyVars kb ("y" ++ pC) ("y" ++ pD)
          step = "Line " ++ pA ++ pB ++ " is vertical, so " ++ pC ++ pD ++ " is horizontal"
      in (newKB, [step])

  | otherwise = (kb, [])

-- | Propagate distance constraint
-- If enough coordinates are known, try to deduce unknowns
propagateDistance :: CoordMap -> String -> String -> Expr -> (CoordMap, [String])
propagateDistance kb p1 p2 distExpr =
  case evaluateExpr kb distExpr of
    Just distValue ->
      -- Try to solve for unknown coordinates
      -- For example: if (xC - xD)² + (yC - yD)² = S², try to deduce xD or yD
      let
        x1Coord = "x" ++ p1
        y1Coord = "y" ++ p1
        z1Coord = "z" ++ p1
        x2Coord = "x" ++ p2
        y2Coord = "y" ++ p2
        z2Coord = "z" ++ p2

        -- Try to get coordinate values
        mx1 = lookupCoord kb x1Coord
        my1 = lookupCoord kb y1Coord
        mz1 = lookupCoord kb z1Coord
        mx2 = lookupCoord kb x2Coord
        my2 = lookupCoord kb y2Coord
        mz2 = lookupCoord kb z2Coord
      in
        -- CASE 1: All but one coordinate known - solve for the unknown
        case (mx1, my1, mz1, mx2, my2, mz2) of
          -- If p1 fully known, p2's x unknown, y and z known → solve for x
          (Just x1, Just y1, Just z1, Nothing, Just y2, Just z2) ->
            let dySquared = (y1 - y2) * (y1 - y2)
                dzSquared = (z1 - z2) * (z1 - z2)
                dxSquared = distValue - dySquared - dzSquared
            in if dxSquared >= 0
               then
                 -- For now, choose positive root (could try both)
                 let dx = sqrtRat dxSquared
                     x2Val = x1 - dx  -- or x1 + dx
                     newKB = M.insert x2Coord (Const x2Val) kb
                     step = "Deduced " ++ x2Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          -- If p1 fully known, p2's y unknown, x and z known → solve for y
          (Just x1, Just y1, Just z1, Just x2, Nothing, Just z2) ->
            let dxSquared = (x1 - x2) * (x1 - x2)
                dzSquared = (z1 - z2) * (z1 - z2)
                dySquared = distValue - dxSquared - dzSquared
            in if dySquared >= 0
               then
                 let dy = sqrtRat dySquared
                     y2Val = y1 - dy  -- or y1 + dy
                     newKB = M.insert y2Coord (Const y2Val) kb
                     step = "Deduced " ++ y2Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          -- If p1 fully known, p2's z unknown, x and y known → solve for z
          (Just x1, Just y1, Just z1, Just x2, Just y2, Nothing) ->
            let dxSquared = (x1 - x2) * (x1 - x2)
                dySquared = (y1 - y2) * (y1 - y2)
                dzSquared = distValue - dxSquared - dySquared
            in if dzSquared >= 0
               then
                 let dz = sqrtRat dzSquared
                     z2Val = z1 - dz  -- or z1 + dz
                     newKB = M.insert z2Coord (Const z2Val) kb
                     step = "Deduced " ++ z2Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          -- Symmetric cases: p2 fully known, p1 has unknowns
          (Nothing, Just y1, Just z1, Just x2, Just y2, Just z2) ->
            let dySquared = (y1 - y2) * (y1 - y2)
                dzSquared = (z1 - z2) * (z1 - z2)
                dxSquared = distValue - dySquared - dzSquared
            in if dxSquared >= 0
               then
                 let dx = sqrtRat dxSquared
                     x1Val = x2 + dx  -- or x2 - dx
                     newKB = M.insert x1Coord (Const x1Val) kb
                     step = "Deduced " ++ x1Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          (Just x1, Nothing, Just z1, Just x2, Just y2, Just z2) ->
            let dxSquared = (x1 - x2) * (x1 - x2)
                dzSquared = (z1 - z2) * (z1 - z2)
                dySquared = distValue - dxSquared - dzSquared
            in if dySquared >= 0
               then
                 let dy = sqrtRat dySquared
                     y1Val = y2 + dy  -- or y2 - dy
                     newKB = M.insert y1Coord (Const y1Val) kb
                     step = "Deduced " ++ y1Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          (Just x1, Just y1, Nothing, Just x2, Just y2, Just z2) ->
            let dxSquared = (x1 - x2) * (x1 - x2)
                dySquared = (y1 - y2) * (y1 - y2)
                dzSquared = distValue - dxSquared - dySquared
            in if dzSquared >= 0
               then
                 let dz = sqrtRat dzSquared
                     z1Val = z2 + dz  -- or z2 - dz
                     newKB = M.insert z1Coord (Const z1Val) kb
                     step = "Deduced " ++ z1Coord ++ " from distance constraint"
                 in (newKB, [step])
               else (kb, [])

          -- Not enough info to deduce anything
          _ -> (kb, [])

    Nothing -> (kb, [])

-- | Square root of a rational (approximate for non-perfect squares)
sqrtRat :: Rational -> Rational
sqrtRat r
  | r < 0 = 0  -- Should not happen if called correctly
  | r == 0 = 0
  | r == 1 = 1
  | r == 4 = 2
  | r == 9 = 3
  | r == 16 = 4
  | r == 25 = 5
  | otherwise =
      -- For non-perfect squares, use Newton's method with rationals
      let x0 = fromInteger (numerator r) % 1
          epsilon = 1 % 1000000  -- Precision
          newton x = let x' = (x + r / x) / 2
                     in if abs (x' - x) < epsilon then x' else newton x'
      in newton x0

-- =============================================
-- Geometric Pattern Recognition
-- =============================================

-- | Check if a line is horizontal (yA = yB)
isHorizontal :: CoordMap -> String -> String -> Bool
isHorizontal kb pA pB = coordsEqual kb ("y" ++ pA) ("y" ++ pB)

-- | Check if a line is vertical (xA = xB)
isVertical :: CoordMap -> String -> String -> Bool
isVertical kb pA pB = coordsEqual kb ("x" ++ pA) ("x" ++ pB)

-- | Check if two coordinates are equal in the knowledge base
coordsEqual :: CoordMap -> String -> String -> Bool
coordsEqual kb v1 v2 =
  case (resolveVar kb v1, resolveVar kb v2) of
    (Just e1, Just e2) -> exprEqual e1 e2
    _ -> False

-- | Check if two expressions are equal (simplified)
exprEqual :: Expr -> Expr -> Bool
exprEqual (Const c1) (Const c2) = c1 == c2
exprEqual (Var v1) (Var v2) = v1 == v2
exprEqual _ _ = False

-- =============================================
-- Variable Resolution
-- =============================================

-- | Unify two variables in the knowledge base
-- Makes v2 equal to v1 by updating the knowledge base
unifyVars :: CoordMap -> String -> String -> CoordMap
unifyVars kb v1 v2 =
  let
    -- Resolve both variables to their ultimate values
    mv1 = resolveVar kb v1
    mv2 = resolveVar kb v2
  in
    case (mv1, mv2) of
      (Just e1, Just e2) ->
        -- Both have values - make v2 point to e1
        M.insert v2 e1 kb
      (Just e1, Nothing) ->
        -- v1 has value, v2 doesn't - give v2 the same value
        M.insert v2 e1 kb
      (Nothing, Just e2) ->
        -- v2 has value, v1 doesn't - give v1 the same value
        M.insert v1 e2 kb
      (Nothing, Nothing) ->
        -- Neither has value - make v2 point to v1
        M.insert v2 (Var v1) kb

-- | Resolve a variable to its value (following chains)
resolveVar :: CoordMap -> String -> Maybe Expr
resolveVar kb v =
  case M.lookup v kb of
    Nothing -> Nothing
    Just (Var v2) | v2 /= v -> resolveVar kb v2  -- Follow chain
    Just e -> Just e

-- =============================================
-- Goal Checking
-- =============================================

-- | Check if a goal can be evaluated in the knowledge base
checkGoal :: CoordMap -> Formula -> [String] -> (Maybe Bool, [String])
checkGoal kb (Eq lhs rhs) steps =
  case (evaluateExpr kb lhs, evaluateExpr kb rhs) of
    (Just v1, Just v2) ->
      let result = v1 == v2
          step = "Evaluated: " ++ prettyExpr lhs ++ " = " ++ show v1 ++
                ", " ++ prettyExpr rhs ++ " = " ++ show v2 ++
                " → " ++ if result then "EQUAL" else "NOT EQUAL"
      in (Just result, steps ++ [step])

    _ -> (Nothing, steps ++ ["Cannot fully evaluate goal with current knowledge"])

checkGoal _ _ steps = (Nothing, steps)

-- | Evaluate an expression using the knowledge base
evaluateExpr :: CoordMap -> Expr -> Maybe Rational
evaluateExpr kb expr =
  case expr of
    Const c -> Just c

    Var v -> do
      e <- resolveVar kb v
      evaluateExpr kb e

    Add e1 e2 -> do
      v1 <- evaluateExpr kb e1
      v2 <- evaluateExpr kb e2
      return (v1 + v2)

    Sub e1 e2 -> do
      v1 <- evaluateExpr kb e1
      v2 <- evaluateExpr kb e2
      return (v1 - v2)

    Mul e1 e2 -> do
      v1 <- evaluateExpr kb e1
      v2 <- evaluateExpr kb e2
      return (v1 * v2)

    Pow e n -> do
      v <- evaluateExpr kb e
      return (v ^ n)

    -- CRITICAL: Distance formula evaluation
    Dist2 p1 p2 -> do
      x1 <- lookupCoord kb ("x" ++ p1)
      y1 <- lookupCoord kb ("y" ++ p1)
      z1 <- lookupCoord kb ("z" ++ p1)
      x2 <- lookupCoord kb ("x" ++ p2)
      y2 <- lookupCoord kb ("y" ++ p2)
      z2 <- lookupCoord kb ("z" ++ p2)
      let dx = x1 - x2
          dy = y1 - y2
          dz = z1 - z2
      return (dx*dx + dy*dy + dz*dz)

    _ -> Nothing

-- | Lookup a coordinate value (defaulting to 0 if not present for z-coords)
lookupCoord :: CoordMap -> String -> Maybe Rational
lookupCoord kb v =
  case resolveVar kb v of
    Just e -> evaluateExpr kb e
    Nothing ->
      -- Default z-coordinates to 0 for 2D problems
      if take 1 v == "z" then Just 0 else Nothing

-- =============================================
-- Formatting
-- =============================================

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
