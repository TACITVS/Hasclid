{-# LANGUAGE DeriveGeneric #-}

module Expr where

import Data.List (intercalate, sortBy, dropWhileEnd)
import qualified Data.Map.Strict as M
import Numeric.Natural (Natural)
import Data.Ratio ((%), numerator, denominator)
import Data.Maybe (mapMaybe)

-- =============================================
-- Symbolic Expressions (AST)
-- =============================================

data Expr
  = Var String              
  | Const Rational          
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Natural
  | Dist2 String String             
  | Collinear String String String  
  | Dot String String String String 
  | Circle String String Expr       
  deriving (Eq, Show)

prettyExpr :: Expr -> String
prettyExpr (Var x)      = x
prettyExpr (Const r)    = prettyRational r
prettyExpr (Add e1 e2)  = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2)  = "(- " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Mul e1 e2)  = "(* " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Div e1 e2)  = "(/ " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Pow e n)    = "(^ " ++ prettyExpr e ++ " " ++ show n ++ ")"
prettyExpr (Dist2 p1 p2) = "(dist2 " ++ p1 ++ " " ++ p2 ++ ")"
prettyExpr (Collinear p1 p2 p3) = "(collinear " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ ")"
prettyExpr (Dot a b c d) = "(dot " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (Circle p c r) = "(circle " ++ p ++ " " ++ c ++ " " ++ prettyExpr r ++ ")"

prettyRational :: Rational -> String
prettyRational r
  | d == 1    = show n
  | otherwise = show n ++ "/" ++ show d
  where n = numerator r
        d = denominator r

-- =============================================
-- Polynomial Engine
-- =============================================

newtype Monomial = Monomial (M.Map String Natural) deriving (Eq, Ord, Show)
newtype Poly = Poly (M.Map Monomial Rational) deriving (Eq, Show)

monomialOne :: Monomial
monomialOne = Monomial M.empty

monomialMul :: Monomial -> Monomial -> Monomial
monomialMul (Monomial m1) (Monomial m2) = Monomial (M.unionWith (+) m1 m2)

-- Monomial Division
monomialDiv :: Monomial -> Monomial -> Maybe Monomial
monomialDiv (Monomial m1) (Monomial m2)
  | M.isSubmapOfBy (<=) m2 m1 = Just $ Monomial (M.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just (v1 - v2)) m1 m2)
  | otherwise = Nothing

monomialLCM :: Monomial -> Monomial -> Monomial
monomialLCM (Monomial m1) (Monomial m2) = Monomial (M.unionWith max m1 m2)

polyZero :: Poly
polyZero = Poly M.empty

polyFromConst :: Rational -> Poly
polyFromConst r | r == 0 = polyZero | otherwise = Poly (M.singleton monomialOne r)

polyFromVar :: String -> Poly
polyFromVar x = Poly (M.singleton (Monomial (M.singleton x 1)) 1)

polyAdd :: Poly -> Poly -> Poly
polyAdd (Poly p1) (Poly p2) = Poly (M.filter (/= 0) (M.unionWith (+) p1 p2))

polyNeg :: Poly -> Poly
polyNeg (Poly p) = Poly (M.map negate p)

polySub :: Poly -> Poly -> Poly
polySub p1 p2 = polyAdd p1 (polyNeg p2)

polyMul :: Poly -> Poly -> Poly
polyMul (Poly p1) (Poly p2) =
  Poly . M.filter (/= 0) $ M.fromListWith (+)
    [ (monomialMul m1 m2, c1 * c2) | (m1, c1) <- M.toList p1, (m2, c2) <- M.toList p2 ]

polyPow :: Poly -> Natural -> Poly
polyPow _ 0 = polyFromConst 1
polyPow p 1 = p
polyPow p n | even n = let h = polyPow p (n `div` 2) in polyMul h h
            | otherwise = polyMul p (polyPow p (n - 1))

getLeadingTerm :: Poly -> Maybe (Monomial, Rational)
getLeadingTerm (Poly m)
  | M.null m = Nothing
  | otherwise = M.lookupMax m

prettyMonomial :: Monomial -> String
prettyMonomial (Monomial m)
  | M.null m = ""
  | otherwise = intercalate "*" [ if e==1 then v else v++"^"++show e | (v,e)<-M.toList m ]

prettyPoly :: Poly -> String
prettyPoly (Poly m)
  | M.null m = "0"
  | otherwise =
      let terms = [ (c, mono) | (mono, c) <- M.toDescList m, c /= 0 ]
          str (c, mo) = (if c < 0 then " - " else " + ") ++
                        (if abs c == 1 && not (M.null (let Monomial x = mo in x)) then "" else prettyRational (abs c)) ++
                        prettyMonomial mo
      in case concatMap str terms of
           (' ':'+':' ':rest) -> rest
           (' ':'-':' ':rest) -> "-" ++ rest
           s -> s

-- =============================================
-- Univariate Support (New for Sturm)
-- =============================================

-- Converts a Poly to a list of coefficients [c0, c1, c2...] 
-- Returns Nothing if the Poly has more than one variable.
toUnivariate :: Poly -> Maybe (String, [Rational])
toUnivariate (Poly m) = 
  let 
      -- Get all variables used
      vars = concatMap (\(Monomial vm) -> M.keys vm) (M.keys m)
      
      -- FIXED: Use safe pattern matching instead of 'head'
      uniqueVars = case vars of
                     (v:_) -> [v]
                     []    -> []
      
      -- Check if truly univariate
      isValid = all (\(Monomial vm) -> length (M.keys vm) <= 1) (M.keys m)
      
  in if not isValid then Nothing else
     case uniqueVars of
       [] -> Just ("x", [M.findWithDefault 0 monomialOne m]) -- Constant poly
       (v:_) -> 
         let maxDeg = maximum (0 : map (\(Monomial vm) -> M.findWithDefault 0 v vm) (M.keys m))
             coeffs = [ M.findWithDefault 0 (Monomial (if i==0 then M.empty else M.singleton v (fromIntegral i))) m 
                      | i <- [0..maxDeg] ]
         in Just (v, coeffs)

-- Convert back
fromUnivariate :: String -> [Rational] -> Poly
fromUnivariate v coeffs = 
  foldl polyAdd polyZero
    [ polyMul (polyFromConst c) (polyPow (polyFromVar v) (fromIntegral i)) 
    | (i, c) <- zip [0..] coeffs, c /= 0 ]

-- =============================================
-- Conversion: Expr -> Poly
-- =============================================

toPoly :: Expr -> Poly
toPoly (Var x)     = polyFromVar x
toPoly (Const r)   = polyFromConst r
toPoly (Add e1 e2) = polyAdd (toPoly e1) (toPoly e2)
toPoly (Sub e1 e2) = polySub (toPoly e1) (toPoly e2)
toPoly (Mul e1 e2) = polyMul (toPoly e1) (toPoly e2)
toPoly (Div _ _)   = error "Division not supported"
toPoly (Pow e n)   = polyPow (toPoly e) n

toPoly (Dist2 p1 p2) =
  let x1 = polyFromVar ("x" ++ p1); y1 = polyFromVar ("y" ++ p1); z1 = polyFromVar ("z" ++ p1)
      x2 = polyFromVar ("x" ++ p2); y2 = polyFromVar ("y" ++ p2); z2 = polyFromVar ("z" ++ p2)
      dx = polySub x1 x2; dy = polySub y1 y2; dz = polySub z1 z2
  in polyAdd (polyAdd (polyMul dx dx) (polyMul dy dy)) (polyMul dz dz)

toPoly (Dot a b c d) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a); za = polyFromVar ("z" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b); zb = polyFromVar ("z" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c); zc = polyFromVar ("z" ++ c)
      xd = polyFromVar ("x" ++ d); yd = polyFromVar ("y" ++ d); zd = polyFromVar ("z" ++ d)
      vABx = polySub xb xa; vABy = polySub yb ya; vABz = polySub zb za
      vCDx = polySub xd xc; vCDy = polySub yd yc; vCDz = polySub zd zc
  in polyAdd (polyAdd (polyMul vABx vCDx) (polyMul vABy vCDy)) (polyMul vABz vCDz)

toPoly (Collinear a b c) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c)
      vABx = polySub xb xa; vABy = polySub yb ya
      vACx = polySub xc xa; vACy = polySub yc ya
  in polySub (polyMul vABx vACy) (polyMul vABy vACx)

toPoly (Circle p c r) =
  let distSq = toPoly (Dist2 p c)
      rad    = toPoly r
  in polySub distSq (polyPow rad 2)

-- =============================================
-- Logic
-- =============================================

data Formula 
  = Eq Expr Expr 
  | Ge Expr Expr 
  | Gt Expr Expr 
  deriving (Eq, Show)

type Theory = [Formula]