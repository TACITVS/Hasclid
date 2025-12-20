module Geometry.WLOG
  ( applyWLOG
  , detectPoints
  , isTranslationInvariant
  ) where

import Expr
import Data.List (nub, sort, isPrefixOf)

-- | Apply Without Loss of Generality (WLOG) simplifications
-- Returns (New Theory, Description of changes)
applyWLOG :: Theory -> Formula -> (Theory, [String])
applyWLOG theory goal =
  let
    -- 1. Identify Points
    points = detectPoints (goal : theory)
    
    -- 2. Detect Invariances (Conservative)
    -- For now, we assume if we haven't seen explicit constants assigned to these vars, we can fix them.
    -- Better: check if problem is "Geometric" type from ProblemAnalyzer? 
    -- But we don't want circular deps.
    -- We'll assume if points are detected, we can try to fix the first one if it's free.
    
    (th1, log1) = applyTranslationWLOG points theory
    (th2, log2) = applyRotationWLOG points th1
    
    -- Scaling WLOG: Fix one length to 1 (assumes homogeneity)
    (th3, log3) = applyScalingWLOG points th2
    
  in (th3, log1 ++ log2 ++ log3)

-- | Fix second point to (1,0) if not constrained (fixes scale)
applyScalingWLOG :: [String] -> Theory -> (Theory, [String])
applyScalingWLOG [] theory = (theory, [])
applyScalingWLOG (_:[]) theory = (theory, [])
applyScalingWLOG (_:p2:_) theory =
  let x2 = "x" ++ p2
      isConstrained v = any (isExplicitConstraint v) theory
  in if isConstrained x2
     then (theory, [])
     else 
       let newConstraints = [ Eq (Var x2) (Const 1) ]
       in (theory ++ newConstraints, ["WLOG Scaling: Fixed " ++ p2 ++ " at x=1"])

-- | Detect points based on variable naming convention (xA, yA, etc.)
detectPoints :: [Formula] -> [String]
detectPoints formulas =
  let vars = concatMap extractVarsFromFormula formulas
      pointNames = nub $ [ p | v <- vars, let (p, axis) = parsePointVar v, axis /= "" ]
  in sort pointNames

parsePointVar :: String -> (String, String)
parsePointVar v
  | "zz_" `isPrefixOf` v = ("", "") -- Ignore auxiliary variables
  | "x" `isPrefixOf` v && length v > 1 = (drop 1 v, "x")
  | "y" `isPrefixOf` v && length v > 1 = (drop 1 v, "y")
  | "z" `isPrefixOf` v && length v > 1 = (drop 1 v, "z")
  | otherwise = ("", "")

-- | Fix first point to (0,0) if not already constrained
applyTranslationWLOG :: [String] -> Theory -> (Theory, [String])
applyTranslationWLOG [] theory = (theory, [])
applyTranslationWLOG (p1:_) theory =
  let x1 = "x" ++ p1
      y1 = "y" ++ p1
      
      -- Check if x1 or y1 are already constrained to a constant
      isConstrained v = any (isExplicitConstraint v) theory
      
  in if isConstrained x1 || isConstrained y1
     then (theory, []) -- Already fixed
     else 
       let newConstraints = 
             [ Eq (Var x1) (Const 0)
             , Eq (Var y1) (Const 0)
             ]
       in (theory ++ newConstraints, ["WLOG Translation: Fixed " ++ p1 ++ " at (0,0)"])

-- | Fix second point to x-axis (y=0) if not constrained
applyRotationWLOG :: [String] -> Theory -> (Theory, [String])
applyRotationWLOG [] theory = (theory, [])
applyRotationWLOG (_:[]) theory = (theory, [])
applyRotationWLOG (_:p2:_) theory =
  let y2 = "y" ++ p2
      
      isConstrained v = any (isExplicitConstraint v) theory
      
  in if isConstrained y2
     then (theory, [])
     else
       -- Also ensure x2 is not 0 (degenerate) if we fix y2=0? 
       -- Usually we just add y2=0. 
       -- But if x2 is also 0, then p1=p2.
       -- For generic triangle, p1 != p2.
       -- We assume user didn't constrain y2.
       let newConstraints = [ Eq (Var y2) (Const 0) ]
           -- Optional: Assume x2 > 0 to fix reflection? Algebraic solver doesn't care about > 0 usually for Equality.
           -- But for Inequality (CAD), x2 > 0 helps.
           -- Let's add x2 >= 0? No, let's keep it simple.
       in (theory ++ newConstraints, ["WLOG Rotation: Fixed " ++ p2 ++ " on x-axis (y" ++ p2 ++ "=0)"])

-- | Check if variable is explicitly constrained to a value
isExplicitConstraint :: String -> Formula -> Bool
isExplicitConstraint v (Eq (Var v') (Const _)) = v == v'
isExplicitConstraint v (Eq (Const _) (Var v')) = v == v'
isExplicitConstraint _ _ = False

-- Helpers (duplicated from ProblemAnalyzer for independence)
extractVarsFromFormula :: Formula -> [String]
extractVarsFromFormula formula =
  case formula of
    Forall qs f -> filterBound qs (extractVarsFromFormula f)
    Exists qs f -> filterBound qs (extractVarsFromFormula f)
    Eq l r -> varsE l ++ varsE r
    Ge l r -> varsE l ++ varsE r
    Gt l r -> varsE l ++ varsE r
    Le l r -> varsE l ++ varsE r
    Lt l r -> varsE l ++ varsE r
    And f1 f2 -> extractVarsFromFormula f1 ++ extractVarsFromFormula f2
    Or f1 f2 -> extractVarsFromFormula f1 ++ extractVarsFromFormula f2
    Not f -> extractVarsFromFormula f
  where
    filterBound qs vs = filter (\v -> not (any (\q -> qvName q == v) qs)) vs

varsE :: Expr -> [String]
varsE (Var v) = [v]
varsE (Add a b) = varsE a ++ varsE b
varsE (Sub a b) = varsE a ++ varsE b
varsE (Mul a b) = varsE a ++ varsE b
varsE (Div a b) = varsE a ++ varsE b
varsE (Pow a _) = varsE a
varsE (Sqrt a) = varsE a
varsE (Dist2 p1 p2) = [axis ++ p | p <- [p1,p2], axis <- ["x","y"]]
-- ... (other geometric primitives omitted for brevity, they usually don't appear in raw WLOG input unless parsed into Vars)
varsE _ = []

-- Placeholder for invariance checks
isTranslationInvariant :: Theory -> Bool
isTranslationInvariant _ = True -- TODO: Real check
