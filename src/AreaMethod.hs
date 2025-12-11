{-# LANGUAGE DeriveGeneric #-}

module AreaMethod where

import Expr
import Data.Ratio
import qualified Data.Map.Strict as M

-- =============================================
-- 1. Geometric Quantities (Invariants)
-- =============================================

data GeoExpr
  = S_Area String String String
  | P_Pyth String String String
  | G_Dist2 String String
  | G_Const Rational
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
  | PointOnLine String String String Rational
  deriving (Show, Eq)

type Construction = [ConstructStep]

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
simplifyGeo (S_Area a b c) = normalizeArea a b c
simplifyGeo (P_Pyth a b c) = normalizePyth a b c
simplifyGeo (G_Dist2 a b) = normalizeDist2 a b
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

eliminate :: ConstructStep -> GeoExpr -> GeoExpr
eliminate (PointMid m a b) expr = eliminate (PointOnLine m a b (1%2)) expr
eliminate step expr = simplifyGeo (elimRec step expr)

elimRec :: ConstructStep -> GeoExpr -> GeoExpr
elimRec step (G_Add a b) = G_Add (elimRec step a) (elimRec step b)
elimRec step (G_Sub a b) = G_Sub (elimRec step a) (elimRec step b)
elimRec step (G_Mul a b) = G_Mul (elimRec step a) (elimRec step b)
elimRec step (G_Div a b) = G_Div (elimRec step a) (elimRec step b)
elimRec step (G_Const c) = G_Const c
elimRec step (S_Area a b c) = elimArea step a b c
elimRec step (P_Pyth a b c) = elimPyth step a b c
elimRec step (G_Dist2 a b)  = elimDist step a b

-- Area Elimination
elimArea :: ConstructStep -> String -> String -> String -> GeoExpr
elimArea (PointOnLine y u v r) a b c
  | y == a = elimAreaOnLine y u v r b c
  | y == b = elimAreaOnLine y u v r c a
  | y == c = elimAreaOnLine y u v r a b
  | otherwise = S_Area a b c
elimArea (PointInter y u v p q) a b c
  | y == a = elimAreaInter y u v p q b c
  | y == b = elimAreaInter y u v p q c a
  | y == c = elimAreaInter y u v p q a b
  | otherwise = S_Area a b c
elimArea _ a b c = S_Area a b c

-- Area Helpers
elimAreaOnLine :: String -> String -> String -> Rational -> String -> String -> GeoExpr
elimAreaOnLine _ u v r a b =
  let sU = S_Area u a b
      sV = S_Area v a b
      term1 = G_Mul (G_Const (1-r)) sU
      term2 = G_Mul (G_Const r) sV
  in G_Add term1 term2

elimAreaInter :: String -> String -> String -> String -> String -> String -> String -> GeoExpr
elimAreaInter _ u v p q a b =
  let sPQU = S_Area p q u
      sPQV = S_Area p q v
      sVAB = S_Area v a b
      sUAB = S_Area u a b
      num = G_Sub (G_Mul sPQU sVAB) (G_Mul sPQV sUAB)
      den = G_Sub sPQU sPQV
  in G_Div num den

-- Pythagoras Elimination
elimPyth :: ConstructStep -> String -> String -> String -> GeoExpr
elimPyth (PointOnLine y u v r) a b c
  | y == b =
      let pU = P_Pyth a u c
          pV = P_Pyth a v c
          uv2 = G_Dist2 u v
          corr = G_Mul (G_Const ((1-r)*r*2)) uv2
          term1 = G_Mul (G_Const (1-r)) pU
          term2 = G_Mul (G_Const r) pV
      in G_Sub (G_Add term1 term2) corr
  | y == a =
      let pU = P_Pyth u b c
          pV = P_Pyth v b c
          term1 = G_Mul (G_Const (1-r)) pU
          term2 = G_Mul (G_Const r) pV
      in G_Add term1 term2
  | y == c =
      let pU = P_Pyth a b u
          pV = P_Pyth a b v
          term1 = G_Mul (G_Const (1-r)) pU
          term2 = G_Mul (G_Const r) pV
      in G_Add term1 term2
  | otherwise = P_Pyth a b c
elimPyth _ a b c = P_Pyth a b c

-- Distance Elimination
elimDist :: ConstructStep -> String -> String -> GeoExpr
elimDist (PointOnLine y u v r) a b
  | y == a = elimDistOnLine y u v r b
  | y == b = elimDistOnLine y u v r a
  | otherwise = G_Dist2 a b
elimDist _ a b = G_Dist2 a b

elimDistOnLine :: String -> String -> String -> Rational -> String -> GeoExpr
elimDistOnLine _ u v r b =
  let dU = G_Dist2 u b
      dV = G_Dist2 v b
      dUV = G_Dist2 u v
      term1 = G_Mul (G_Const (1-r)) dU
      term2 = G_Mul (G_Const r) dV
      corr  = G_Mul (G_Const ((1-r)*r)) dUV
  in G_Sub (G_Add term1 term2) corr

-- =============================================
-- 4. Proof Engine
-- =============================================

proveArea :: Construction -> GeoExpr -> (Bool, String)
proveArea steps goal =
  let
    reduced = foldr eliminate goal steps
    simplified = simplifyGeo reduced
  in
    case simplified of
      G_Const c | c == 0 -> (True, "Reduced to 0")
      _ -> (False, "Reduced to: " ++ show simplified)