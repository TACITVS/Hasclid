{-# LANGUAGE DeriveGeneric #-}

module AreaMethod
  ( proveArea
  , proveAreaE
  , AreaResult(..)
  , GeoExpr(..)
  , Construction
  , ConstructStep(..)
  , deriveConstruction
  , exprToGeoExpr
  , geoToExpr
  , reduceArea
  , checkConstruction
  ) where

import Expr
import Error
import Data.Ratio
import qualified Data.Map.Strict as M
import Data.List (nub, isPrefixOf, find, (\\))
import Data.Maybe (mapMaybe)
import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Modular

-- =============================================
-- 1. Geometric Quantities (Invariants)
-- =============================================

data GeoExpr
  = S_Area String String String
  | P_Pyth String String String
  | G_Dist2 String String
  | G_Sqrt GeoExpr
  | G_Const Rational
  | G_Param String
  | G_Add GeoExpr GeoExpr
  | G_Sub GeoExpr GeoExpr
  | G_Mul GeoExpr GeoExpr
  | G_Div GeoExpr GeoExpr
  deriving (Show, Eq, Ord)

data ConstructStep
  = PointFree String
  | PointInter String String String String String
  | PointMid String String String
  | PointFoot String String String String
  | PointOnLine String String String GeoExpr
  | PointInterAng String String String GeoExpr String String GeoExpr
  deriving (Show, Eq)

type Construction = [ConstructStep]
type AreaM = Writer [GeoExpr]

-- =============================================
-- 2. Basic Evaluation / Simplification
-- =============================================

simplifyGeo :: GeoExpr -> GeoExpr
simplifyGeo (G_Add a b) =
  case (simplifyGeo a, simplifyGeo b) of
    (G_Const x, G_Const y) -> G_Const (x + y)
    (G_Const 0, y) -> y
    (x, G_Const 0) -> x
    (x, y) -> G_Add x y
simplifyGeo (G_Sub a b) =
  case (simplifyGeo a, simplifyGeo b) of
    (G_Const x, G_Const y) -> G_Const (x - y)
    (x, G_Const 0) -> x
    (x, y) | x == y -> G_Const 0
    (x, y) -> G_Sub x y
simplifyGeo (G_Mul a b) =
  case (simplifyGeo a, simplifyGeo b) of
    (G_Const x, G_Const y) -> G_Const (x * y)
    (G_Const 0, _) -> G_Const 0
    (_, G_Const 0) -> G_Const 0
    (G_Const 1, y) -> y
    (x, G_Const 1) -> x
    (x, y) -> G_Mul x y
simplifyGeo (G_Div a b) =
  case (simplifyGeo a, simplifyGeo b) of
    (G_Const x, G_Const y) | y /= 0 -> G_Const (x / y)
    (x, G_Const 1) -> x
    (x, y) -> G_Div x y
simplifyGeo (G_Sqrt a) =
  case simplifyGeo a of
    G_Const x | x >= 0 -> 
      let n = numerator x
          d = denominator x
          rn = integerSqrt n
          rd = integerSqrt d
      in if rn*rn == n && rd*rd == d
         then G_Const (toRational rn / toRational rd)
         else G_Sqrt (G_Const x)
    x -> G_Sqrt x
simplifyGeo (S_Area a b c) = normalizeArea a b c
simplifyGeo (P_Pyth a b c) = normalizePyth a b c
simplifyGeo (G_Dist2 a b) = normalizeDist2 a b
simplifyGeo (G_Param s) = G_Param s
simplifyGeo x = x

normalizeArea :: String -> String -> String -> GeoExpr
normalizeArea a b c
  | a == b || b == c || a == c = G_Const 0
  | otherwise =
      let minPt = minimum [a,b,c]
      in if a == minPt then S_Area a b c
         else if b == minPt then S_Area b c a
         else S_Area c a b

normalizePyth :: String -> String -> String -> GeoExpr
normalizePyth a b c =
  if a > c then P_Pyth c b a else P_Pyth a b c

normalizeDist2 :: String -> String -> GeoExpr
normalizeDist2 a b
  | a == b = G_Const 0
  | a > b = G_Dist2 b a
  | otherwise = G_Dist2 a b

-- =============================================
-- 3. Elimination Lemmas
-- =============================================

eliminate :: ConstructStep -> GeoExpr -> AreaM GeoExpr
eliminate (PointMid m a b) expr = eliminate (PointOnLine m a b (G_Const (1%2))) expr
eliminate step expr = do
  res <- elimRec step expr
  return (simplifyGeo res)

elimRec :: ConstructStep -> GeoExpr -> AreaM GeoExpr
elimRec step (G_Add a b) = G_Add <$> elimRec step a <*> elimRec step b
elimRec step (G_Sub a b) = G_Sub <$> elimRec step a <*> elimRec step b
elimRec step (G_Mul a b) = G_Mul <$> elimRec step a <*> elimRec step b
elimRec step (G_Div a b) = G_Div <$> elimRec step a <*> elimRec step b
elimRec step (G_Sqrt a)  = G_Sqrt <$> elimRec step a
elimRec _    (G_Const c) = return (G_Const c)
elimRec _    (G_Param s) = return (G_Param s)
elimRec step (S_Area a b c) = elimArea step a b c
elimRec step (P_Pyth a b c) = elimPyth step a b c
elimRec step (G_Dist2 a b)  = elimDist step a b

-- Area Elimination
elimArea :: ConstructStep -> String -> String -> String -> AreaM GeoExpr
elimArea (PointOnLine y u v r) a b c
  | y == a = elimAreaOnLine y u v r b c
  | y == b = elimAreaOnLine y u v r c a
  | y == c = elimAreaOnLine y u v r a b
  | otherwise = return (S_Area a b c)
elimArea (PointInter y u v p q) a b c
  | y == a = elimAreaInter y u v p q b c
  | y == b = elimAreaInter y u v p q c a
  | y == c = elimAreaInter y u v p q a b
  | otherwise = return (S_Area a b c)
elimArea (PointInterAng y u v t1 p q t2) a b c
  | y == a = elimAreaInterAng y u v t1 p q t2 b c
  | y == b = elimAreaInterAng y u v t1 p q t2 c a
  | y == c = elimAreaInterAng y u v t1 p q t2 a b
  | otherwise = return (S_Area a b c)
elimArea _ a b c = return (S_Area a b c)

-- Pythagoras Elimination
elimPyth :: ConstructStep -> String -> String -> String -> AreaM GeoExpr
elimPyth (PointOnLine y u v r) a b c
  | y == b = return $
      let pU = P_Pyth a u c
          pV = P_Pyth a v c
          uv2 = G_Dist2 u v
          coeff = G_Mul (G_Mul (G_Sub (G_Const 1) r) r) (G_Const 2)
          corr = G_Mul coeff uv2
          term1 = G_Mul (G_Sub (G_Const 1) r) pU
          term2 = G_Mul r pV
      in G_Sub (G_Add term1 term2) corr
  | y == a = return $
      let pU = P_Pyth u b c
          pV = P_Pyth v b c
          term1 = G_Mul (G_Sub (G_Const 1) r) pU
          term2 = G_Mul r pV
      in G_Add term1 term2
  | y == c = return $
      let pU = P_Pyth a b u
          pV = P_Pyth a b v
          term1 = G_Mul (G_Sub (G_Const 1) r) pU
          term2 = G_Mul r pV
      in G_Add term1 term2
  | otherwise = return (P_Pyth a b c)
elimPyth _ a b c = return (P_Pyth a b c)

-- Distance Elimination
elimDist :: ConstructStep -> String -> String -> AreaM GeoExpr
elimDist (PointOnLine y u v r) a b
  | y == a = return (elimDistOnLine y u v r b)
  | y == b = return (elimDistOnLine y u v r a)
  | otherwise = return (G_Dist2 a b)
elimDist _ a b = return (G_Dist2 a b)

-- =============================================
-- Helpers
-- =============================================

elimAreaOnLine :: String -> String -> String -> GeoExpr -> String -> String -> AreaM GeoExpr
elimAreaOnLine _ u v r a b = return $
  let sU = S_Area u a b
      sV = S_Area v a b
      term1 = G_Mul (G_Sub (G_Const 1) r) sU
      term2 = G_Mul r sV
  in G_Add term1 term2

elimAreaInter :: String -> String -> String -> String -> String -> String -> String -> AreaM GeoExpr
elimAreaInter _ u v p q a b = do
  let sPQU = S_Area p q u
      sPQV = S_Area p q v
      sVAB = S_Area v a b
      sUAB = S_Area u a b
      num = G_Sub (G_Mul sPQU sVAB) (G_Mul sPQV sUAB)
      den = G_Sub sPQU sPQV
  tell [den] -- Denominator condition: S_PQU != S_PQV (lines not parallel)
  return (G_Div num den)

elimAreaInterAng :: String -> String -> String -> GeoExpr -> String -> String -> GeoExpr -> String -> String -> AreaM GeoExpr
elimAreaInterAng _ u v t1 p q t2 a b = do
  let
    k1 = G_Div t1 (G_Const 4)
    k2 = G_Div t2 (G_Const 4)
    
    evalF1 z = G_Sub (S_Area v u z) (G_Mul k1 (P_Pyth v u z))
    evalF2 z = G_Sub (S_Area q p z) (G_Mul k2 (P_Pyth q p z))
    
    f1A = evalF1 a
    f1B = evalF1 b
    f2A = evalF2 a
    f2B = evalF2 b
    f2U = evalF2 u
    
    dAB = G_Sub (G_Mul f1A f2B) (G_Mul f1B f2A)
    
    term1 = G_Mul (G_Sub f1B f1A) f2U
    den = G_Add term1 dAB
    num = G_Mul (S_Area a b u) dAB
  
  tell [den] -- Denominator condition
  return (G_Div num den)

elimDistOnLine :: String -> String -> String -> GeoExpr -> String -> GeoExpr
elimDistOnLine _ u v r b =
  let dU = G_Dist2 u b
      dV = G_Dist2 v b
      dUV = G_Dist2 u v
      term1 = G_Mul (G_Sub (G_Const 1) r) dU
      term2 = G_Mul r dV
      coeff = G_Mul (G_Sub (G_Const 1) r) r
      corr  = G_Mul coeff dUV
  in G_Sub (G_Add term1 term2) corr

-- =============================================
-- 4. Proof Engine
-- =============================================

geoToExpr :: GeoExpr -> Expr
geoToExpr (S_Area a b c) = Collinear a b c
geoToExpr (P_Pyth a b c) = 
  Sub (Add (Dist2 a b) (Dist2 b c)) (Dist2 a c)
geoToExpr (G_Dist2 a b)  = Dist2 a b
geoToExpr (G_Sqrt e)     = Sqrt (geoToExpr e)
geoToExpr (G_Const r)    = Const r
geoToExpr (G_Param s)    = Var s
geoToExpr (G_Add a b)    = Add (geoToExpr a) (geoToExpr b)
geoToExpr (G_Sub a b)    = Sub (geoToExpr a) (geoToExpr b)
geoToExpr (G_Mul a b)    = Mul (geoToExpr a) (geoToExpr b)
geoToExpr (G_Div a b)    = Div (geoToExpr a) (geoToExpr b)

-- | Result of Area Method proof attempt (Either-based API)
data AreaResult = AreaResult
  { areaProved :: Bool      -- Was the theorem proved?
  , areaReason :: String    -- Explanation
  , areaReduced :: GeoExpr  -- Reduced expression
  , ndgConditions :: [GeoExpr] -- Non-degeneracy conditions
  , isDegenerate :: Bool    -- Is the construction degenerate?
  } deriving (Show, Eq)

-- | Legacy tuple-based API for backward compatibility
proveArea :: Construction -> GeoExpr -> (Bool, String)
proveArea steps goal =
  let
    (reduced, ndgs) = runWriter (eliminateAll steps goal)
    simplified = simplifyGeo reduced
    degenerate = checkDegeneracy ndgs
  in
    if degenerate
    then (False, "Construction is DEGENERATE (e.g. parallel lines intersected)")
    else case simplified of
           G_Const c | c == 0 -> (True, "Reduced to 0")
           _ -> (False, "Reduced to: " ++ show simplified)

-- | Check if any non-degeneracy condition is violated (i.e. denominator can be zero)
checkDegeneracy :: [GeoExpr] -> Bool
checkDegeneracy ndgs = 
  let checkOne den = 
        let poly = geoToExpr den
            -- Check if den = 0 is consistent (Satisfiable)
            -- If it is satisfiable, then there exists a configuration where it is zero -> Degenerate
            (consistent, _) = Modular.probSolve [] (Eq poly (Const 0))
        in consistent
  in any checkOne ndgs

eliminateAll :: Construction -> GeoExpr -> AreaM GeoExpr
eliminateAll steps goal = foldr (\step acc -> acc >>= eliminate step) (return goal) steps

-- | Get the reduced expression from Area Method
reduceArea :: Construction -> GeoExpr -> GeoExpr
reduceArea steps goal = 
  let (res, _) = runWriter (eliminateAll steps goal)
  in simplifyGeo res

-- | Either-based version of proveArea (recommended API)
-- Returns Either ProverError AreaResult for better error handling
proveAreaE :: Construction -> GeoExpr -> Either ProverError AreaResult
proveAreaE steps goal =
  let (reduced, ndgs) = runWriter (eliminateAll steps goal)
      simplified = simplifyGeo reduced
      degenerate = checkDegeneracy ndgs
      proved = case simplified of
                 G_Const c | c == 0 -> True
                 _ -> False
      reason = if degenerate then "Construction is Degenerate" 
               else if proved then "Reduced to 0" 
               else "Reduced to complex expression"
  in Right $ AreaResult (proved && not degenerate) reason simplified ndgs degenerate

-- | Check validity of a construction (Genericity Checker)
-- Verifies that all denominators implied by construction steps are non-zero.
checkConstruction :: Construction -> (Bool, String)
checkConstruction steps =
  let 
    go _ [] = (True, "Construction Valid")
    go prevReversed (step:rest) =
      let ndgs = getStepNDGs step
          -- Check if any NDG reduces to 0 using previous steps
          isZero ndg = 
            let reduced = reduceArea prevReversed ndg
            in reduced == G_Const 0
          
          failures = filter isZero ndgs
      in if null failures
         then go (step:prevReversed) rest
         else (False, "Degenerate step: " ++ show step)
  in go [] steps

getStepNDGs :: ConstructStep -> [GeoExpr]
getStepNDGs (PointInter _ u v p q) = 
  -- Intersection of UV and PQ. Denom is S_PQU - S_PQV.
  [G_Sub (S_Area p q u) (S_Area p q v)]
getStepNDGs (PointInterAng _ u v t1 p q t2) =
  -- More complex, but similar structure. Denom from elimAreaInterAng.
  [] 
getStepNDGs (PointOnLine _ _ _ r) =
  []
getStepNDGs _ = []
-- =============================================
-- 5. Theory -> Construction Bridge
-- =============================================

deriveConstruction :: Theory -> Formula -> Maybe (Construction, GeoExpr)
deriveConstruction theory goal = do
  geoGoal <- exprToGeoExpr goal
  let points = collectPoints theory
      steps = mapMaybe (theoryToStep theory) points
      
      -- Any point in 'points' NOT constructed is a Free point
      constructedPoints = map getConstructedPoint steps
      freePoints = points \\ constructedPoints
      freeSteps = map PointFree freePoints
      
      -- Rudimentary topological sort:
      -- Free points first, then constructed ones.
      -- A real impl needs full topological sort based on dependencies.
      -- For now, we assume implicit order or simple cases.
      fullConstruction = freeSteps ++ steps
      
  return (fullConstruction, geoGoal)

getConstructedPoint :: ConstructStep -> String
getConstructedPoint (PointFree p) = p
getConstructedPoint (PointInter p _ _ _ _) = p
getConstructedPoint (PointMid p _ _) = p
getConstructedPoint (PointFoot p _ _ _) = p
getConstructedPoint (PointOnLine p _ _ _) = p
getConstructedPoint (PointInterAng p _ _ _ _ _ _) = p

theoryToStep :: Theory -> String -> Maybe ConstructStep
theoryToStep theory p =
  let relevant = filter (mentionsPoint p) theory
  in case findMidpoint p relevant of
       Just step -> Just step
       Nothing ->
         case findFoot p relevant of
           Just step -> Just step
           Nothing -> findInter p relevant

mentionsPoint :: String -> Formula -> Bool
mentionsPoint p f = p `elem` getPointsInFormula f

findMidpoint :: String -> [Formula] -> Maybe ConstructStep
findMidpoint p formulas =
  case find (isMidpointDef p) formulas of
    Just (Eq (Midpoint a b _) _) -> Just (PointMid p a b)
    _ -> Nothing

isMidpointDef :: String -> Formula -> Bool
isMidpointDef p (Eq (Midpoint _ _ m) _) = m == p
isMidpointDef _ _ = False

findFoot :: String -> [Formula] -> Maybe ConstructStep
findFoot p formulas =
  -- Need (Perpendicular C P A B) AND (Collinear A B P)
  -- P is the foot of perpendicular from C to AB
  let perps = [ (c, a, b) | Eq (Perpendicular c p' a b) _ <- formulas, p' == p ] ++
              [ (c, a, b) | Eq (Perpendicular p' c a b) _ <- formulas, p' == p ]
      colls = [ (u, v) | Eq (Collinear u v p') _ <- formulas, p' == p ]
  in case [ (c, a, b) | (c, a, b) <- perps, any (\(u,v) -> (u==a && v==b) || (u==b && v==a)) colls ] of
       ((c, a, b):_) -> Just (PointFoot p c a b)
       _ -> Nothing

findInter :: String -> [Formula] -> Maybe ConstructStep
findInter p formulas =
  -- Need (Collinear A B P) AND (Collinear C D P)
  -- P is intersection of AB and CD
  let colls = [ (a, b) | Eq (Collinear a b p') _ <- formulas, p' == p ]
      pairs = [ ((a,b), (c,d)) | (a,b) <- colls, (c,d) <- colls, a /= c || b /= d, a /= d || b /= c ]
  in case pairs of
       (((a,b), (c,d)):_) -> Just (PointInter p a b c d)
       _ -> Nothing

_definesPoint :: String -> Formula -> Bool
_definesPoint p f = mentionsPoint p f -- Simplified for now, relying on theoryToStep logic

collectPoints :: Theory -> [String]
collectPoints = nub . concatMap getPointsInFormula

getPointsInFormula :: Formula -> [String]
getPointsInFormula (Eq l r) = getPointsInExpr l ++ getPointsInExpr r
getPointsInFormula _ = []

getPointsInExpr :: Expr -> [String]
getPointsInExpr (Midpoint a b m) = [a,b,m]
getPointsInExpr (Dist2 a b) = [a,b]
getPointsInExpr (Collinear a b c) = [a,b,c]
getPointsInExpr (Perpendicular a b c d) = [a,b,c,d]
getPointsInExpr (Parallel a b c d) = [a,b,c,d]
getPointsInExpr (Var v) = 
  if "x" `isPrefixOf` v then [drop 1 v]
  else if "y" `isPrefixOf` v then [drop 1 v]
  else []
getPointsInExpr _ = []

-- Convert goal Formula to GeoExpr (Difference = 0 or >= 0)
exprToGeoExpr :: Formula -> Maybe GeoExpr
exprToGeoExpr (Eq l r) = G_Sub <$> exprToGeo l <*> exprToGeo r
exprToGeoExpr (Ge l r) = G_Sub <$> exprToGeo l <*> exprToGeo r
exprToGeoExpr (Gt l r) = G_Sub <$> exprToGeo l <*> exprToGeo r
exprToGeoExpr (Le l r) = G_Sub <$> exprToGeo r <*> exprToGeo l
exprToGeoExpr (Lt l r) = G_Sub <$> exprToGeo r <*> exprToGeo l
exprToGeoExpr _ = Nothing

exprToGeo :: Expr -> Maybe GeoExpr
exprToGeo (Dist2 a b) = Just (G_Dist2 a b)
exprToGeo (Sqrt e)    = G_Sqrt <$> exprToGeo e
exprToGeo (Add a b)   = G_Add <$> exprToGeo a <*> exprToGeo b
exprToGeo (Sub a b)   = G_Sub <$> exprToGeo a <*> exprToGeo b
exprToGeo (Mul a b)   = G_Mul <$> exprToGeo a <*> exprToGeo b
exprToGeo (Div a b)   = G_Div <$> exprToGeo a <*> exprToGeo b
exprToGeo (Const r)   = Just (G_Const r)
exprToGeo (Var v)     = Just (G_Param v)
exprToGeo (Collinear a b c) = Just (S_Area a b c)
exprToGeo (Perpendicular a b c d) =
  -- dot(AB, CD) = (P_{BCD} - P_{ACD}) / 2
  -- We omit factor 2 for zero-check
  Just (G_Sub (P_Pyth b c d) (P_Pyth a c d))
exprToGeo _ = Nothing

