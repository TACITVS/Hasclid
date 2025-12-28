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
import Data.Maybe (mapMaybe)
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

solveGeoWithTrace :: Theory -> Formula -> GeoResult
solveGeoWithTrace theory goal =
  -- Phase 1: Simple syntactic check
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
           case findResult isFalse results of
             Just (Just False, reason, _) ->
               GeoDisproved reason (steps1 ++ ["Found counter-example in branch: " ++ reason])
             _ -> GeoUnknown "Counter-example indicated but not retrievable"
         else if allProved
              then GeoProved "Geometric propagation (all branches hold)" steps1
              else GeoUnknown "Some branches indeterminate"

  where
    isFalse (Just False, _, _) = True
    isFalse _ = False
    
    isTrue (Just True, _, _) = True
    isTrue _ = False
    
    findResult _ [] = Nothing
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

-- | Propagate perpendicularity (BIDIRECTIONAL - checks both lines)
propagatePerpendicular :: CoordMap -> String -> String -> String -> String -> ([CoordMap], [String])
propagatePerpendicular kb pA pB pC pD
  -- Case 1: AB horizontal -> CD vertical (xC = xD)
  | isHorizontal kb pA pB =
      let newKB = unifyVars kb ("x" ++ pC) ("x" ++ pD)
      in ([newKB], ["Line " ++ pA ++ pB ++ " horizontal => " ++ pC ++ pD ++ " vertical"])

  -- Case 2: AB vertical -> CD horizontal (yC = yD)
  | isVertical kb pA pB =
      let newKB = unifyVars kb ("y" ++ pC) ("y" ++ pD)
      in ([newKB], ["Line " ++ pA ++ pB ++ " vertical => " ++ pC ++ pD ++ " horizontal"])

  -- Case 3: CD horizontal -> AB vertical (xA = xB) [BIDIRECTIONAL]
  | isHorizontal kb pC pD =
      let newKB = unifyVars kb ("x" ++ pA) ("x" ++ pB)
      in ([newKB], ["Line " ++ pC ++ pD ++ " horizontal => " ++ pA ++ pB ++ " vertical"])

  -- Case 4: CD vertical -> AB horizontal (yA = yB) [BIDIRECTIONAL]
  | isVertical kb pC pD =
      let newKB = unifyVars kb ("y" ++ pA) ("y" ++ pB)
      in ([newKB], ["Line " ++ pC ++ pD ++ " vertical => " ++ pA ++ pB ++ " horizontal"])

  -- Case 5: General case - try to extract linear constraint from dot product
  -- Perpendicularity: (xB - xA)(xD - xC) + (yB - yA)(yD - yC) = 0
  | otherwise = tryGeneralPerpendicular kb pA pB pC pD

-- | Try to derive constraints from general perpendicularity
-- When neither line is axis-aligned, expand the dot product and solve
tryGeneralPerpendicular :: CoordMap -> String -> String -> String -> String -> ([CoordMap], [String])
tryGeneralPerpendicular kb pA pB pC pD =
  let
      -- Unexpanded (keeps coordinate variable names like xD)
      xA' = Var ("x" ++ pA); yA' = Var ("y" ++ pA)
      xB' = Var ("x" ++ pB); yB' = Var ("y" ++ pB)
      xC' = Var ("x" ++ pC); yC' = Var ("y" ++ pC)
      xD' = Var ("x" ++ pD); yD' = Var ("y" ++ pD)

      dotRaw =
        simplifyExpr $
          Add
            (Mul (Sub xB' xA') (Sub xD' xC'))
            (Mul (Sub yB' yA') (Sub yD' yC'))

      -- Resolve coordinates with existing substitutions
      xA = resolveExpand kb (Var ("x" ++ pA)); yA = resolveExpand kb (Var ("y" ++ pA))
      xB = resolveExpand kb (Var ("x" ++ pB)); yB = resolveExpand kb (Var ("y" ++ pB))
      xC = resolveExpand kb (Var ("x" ++ pC)); yC = resolveExpand kb (Var ("y" ++ pC))
      xD = resolveExpand kb (Var ("x" ++ pD)); yD = resolveExpand kb (Var ("y" ++ pD))

      -- Dot product equation = 0 (2D)
      dotExpr = simplifyExpr $
                  Add
                    (Mul (Sub xB xA) (Sub xD xC))
                    (Mul (Sub yB yA) (Sub yD yC))

      -- Prefer variables on the D line (including resolved names)
      resolveName v = case resolveVar kb v of
                        Just (Var v2) -> Just v2
                        _ -> Nothing
      preferredVars = mapMaybe id
        [ Just ("y" ++ pD)
        , resolveName ("y" ++ pD)
        , Just ("x" ++ pD)
        , resolveName ("x" ++ pD)
        , Just ("y" ++ pC)
        , resolveName ("y" ++ pC)
        , Just ("x" ++ pC)
        , resolveName ("x" ++ pC)
        ]

      candidateVars = nub (preferredVars ++ varsInExpr dotExpr ++ varsInExpr dotRaw)

      trySolveExpr _ [] = Nothing
      trySolveExpr expr (v:vs) =
        if containsVar v expr
        then case solveSymbolicLinear v expr (Const 0) of
               Just sol ->
                 let simplified = simplifyExpr sol
                     newKB = M.insert v simplified kb
                 in Just (newKB, "Derived from perpendicular (general): " ++ v ++ " = " ++ prettyExpr simplified)
               Nothing -> trySolveExpr expr vs
        else trySolveExpr expr vs

      trySolve [] = Nothing
      trySolve (expr:rest) =
        case trySolveExpr expr candidateVars of
          Just r -> Just r
          Nothing -> trySolve rest
  in
      case trySolve [dotExpr, dotRaw] of
        Just (newKB, msg) -> ([newKB], [msg])
        Nothing -> ([kb], []) -- Could not derive a linear constraint

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
    isUnknown (Var v) | isCoordVarName v = Just v
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
solveQuadratic _ rhs =
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
symbolicSqrt (Sqrt e) = Just (Sqrt e)
symbolicSqrt _ = Nothing

-- | Solve a linear equation symbolically: solve for 'var' in 'lhs = rhs'
-- Returns Just solution if var appears linearly, Nothing otherwise
-- Example: solveSymbolicLinear "x" (Add (Var "x") (Var "S")) (Mul (Const 2) (Var "S"))
--          => Just (Var "S")   [because x + S = 2*S implies x = S]
solveSymbolicLinear :: String -> Expr -> Expr -> Maybe Expr
solveSymbolicLinear var lhs rhs =
  let
    -- Move everything to left side: lhs - rhs = 0
    equation = simplifyExpr (Sub lhs rhs)

    -- Try to extract linear form: a*var + b = 0
    (coeff, constant) = extractLinearCoeffs var equation
  in
    case (coeff, constant) of
      -- If coefficient of var is zero, equation doesn't involve var
      (Const 0, _) -> Nothing

      -- If we have a*var + b = 0, solve for var: var = -b/a
      (a, b) -> Just (simplifyExpr (Div (Mul (Const (-1)) b) a))

-- | Extract coefficients from a linear expression in given variable
-- Returns (coefficient_of_var, constant_term)
-- Example: extractLinearCoeffs "x" (Add (Mul (Const 2) (Var "x")) (Const 3))
--          => (Const 2, Const 3)
extractLinearCoeffs :: String -> Expr -> (Expr, Expr)
extractLinearCoeffs var expr =
  case simplifyExpr expr of
    -- Just the variable: x => (1, 0)
    Var v | v == var -> (Const 1, Const 0)

    -- Constant: c => (0, c)
    Const c -> (Const 0, Const c)

    -- Addition: split into var-dependent and independent parts
    Add e1 e2 ->
      let (c1, k1) = extractLinearCoeffs var e1
          (c2, k2) = extractLinearCoeffs var e2
      in (simplifyExpr (Add c1 c2), simplifyExpr (Add k1 k2))

    -- Subtraction: same as addition but negate second term
    Sub e1 e2 ->
      let (c1, k1) = extractLinearCoeffs var e1
          (c2, k2) = extractLinearCoeffs var e2
      in (simplifyExpr (Sub c1 c2), simplifyExpr (Sub k1 k2))

    -- Multiplication: check if one operand is var-free
    Mul e1 e2 ->
      let (c1, k1) = extractLinearCoeffs var e1
          (c2, k2) = extractLinearCoeffs var e2
      in case (c1, c2) of
           -- e1 is constant, e2 has var: c*x => (c*c2, c*k2)
           (Const 0, _) -> (simplifyExpr (Mul e1 c2), simplifyExpr (Mul e1 k2))
           -- e2 is constant, e1 has var: x*c => (c1*c, k1*c)
           (_, Const 0) -> (simplifyExpr (Mul c1 e2), simplifyExpr (Mul k1 e2))
           -- Both have var - nonlinear, give up
           _ -> (Const 0, expr)

    -- Division, power, etc.: treat as non-linear (give up)
    _ | containsVar var expr -> (Const 0, expr) -- Nonlinear
      | otherwise -> (Const 0, expr) -- Constant

-- | Check if expression contains a variable
containsVar :: String -> Expr -> Bool
containsVar v (Var x) = v == x
containsVar v (Add e1 e2) = containsVar v e1 || containsVar v e2
containsVar v (Sub e1 e2) = containsVar v e1 || containsVar v e2
containsVar v (Mul e1 e2) = containsVar v e1 || containsVar v e2
containsVar v (Div e1 e2) = containsVar v e1 || containsVar v e2
containsVar v (Pow e _) = containsVar v e
containsVar v (Sqrt e) = containsVar v e
containsVar v (IntVar x) = v == x
containsVar _ _ = False

-- =============================================
-- Utilities
-- =============================================

resolveExpand :: CoordMap -> Expr -> Expr
resolveExpand kb e = simplifyExpr (expandExprRecursive kb e)

expandExprRecursive :: CoordMap -> Expr -> Expr
expandExprRecursive kb (Var v) =
  case resolveVar kb v of
    Just e -> expandExprRecursive kb e
    Nothing -> Var v
expandExprRecursive kb (Add e1 e2) = Add (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Sub e1 e2) = Sub (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Mul e1 e2) = Mul (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Div e1 e2) = Div (expandExprRecursive kb e1) (expandExprRecursive kb e2)
expandExprRecursive kb (Pow e n) = Pow (expandExprRecursive kb e) n
expandExprRecursive kb (Sqrt e) = Sqrt (expandExprRecursive kb e)
expandExprRecursive kb (Dist2 p1 p2) =
  let x1 = expandExprRecursive kb (Var ("x"++p1))
      y1 = expandExprRecursive kb (Var ("y"++p1))
      z1 = expandExprRecursive kb (Var ("z"++p1))
      x2 = expandExprRecursive kb (Var ("x"++p2))
      y2 = expandExprRecursive kb (Var ("y"++p2))
      z2 = expandExprRecursive kb (Var ("z"++p2))
  in Add (Add (Pow (Sub x1 x2) 2) (Pow (Sub y1 y2) 2)) (Pow (Sub z1 z2) 2)
expandExprRecursive _ e = e

isCoordVarName :: String -> Bool
isCoordVarName ('z':'z':'_':_) = False
isCoordVarName ('x':rest) = not (null rest)
isCoordVarName ('y':rest) = not (null rest)
isCoordVarName ('z':rest) = not (null rest)
isCoordVarName _ = False

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
       case (l, r) of
         (Const c1, Const c2) | c1 /= c2 -> (Just False, "Constants not equal", ["LHS: " ++ show c1, "RHS: " ++ show c2])
         -- For symbolic expressions that don't match structurally, we cannot be sure they are mathematically distinct
         -- without a canonical form or stronger algebra.
         -- Return Nothing (Unknown) to allow fallback to Wu/Groebner which handle this correctly.
         _ -> (Nothing, "Symbolic mismatch (needs algebraic proof)", ["LHS: " ++ prettyExpr l, "RHS: " ++ prettyExpr r])
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

