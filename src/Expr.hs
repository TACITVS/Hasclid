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
  -- Geometric primitives
  | Dist2 String String                    -- Squared distance between two points
  | Collinear String String String         -- Three points are collinear
  | Dot String String String String        -- Dot product of vectors AB and CD
  | Circle String String Expr              -- Point on circle with center and radius
  -- New geometry helpers
  | Midpoint String String String          -- M is midpoint of AB: (xM = (xA+xB)/2, etc.)
  | Perpendicular String String String String  -- AB ⊥ CD (dot product = 0)
  | Parallel String String String String       -- AB ∥ CD (cross product = 0)
  -- High-level algebraic primitives
  | Determinant [[Expr]]                   -- Lazy determinant of a matrix
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
prettyExpr (Midpoint a b m) = "(midpoint " ++ a ++ " " ++ b ++ " " ++ m ++ ")"
prettyExpr (Perpendicular a b c d) = "(perpendicular " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (Parallel a b c d) = "(parallel " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (Determinant rows) = "(det " ++ show (length rows) ++ "x" ++ show (length (head rows)) ++ ")"

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
newtype Poly = Poly (M.Map Monomial Rational) deriving (Eq, Ord, Show)

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

-- Enhanced pretty printing with mathematical notation
prettyPolyNice :: Poly -> String
prettyPolyNice (Poly m)
  | M.null m = "0"
  | otherwise =
      let terms = sortTerms [ (c, mono) | (mono, c) <- M.toList m, c /= 0 ]
      in formatTerms terms
  where
    -- Sort terms by total degree (descending), then lexicographically
    sortTerms = sortBy (\(c1, m1) (c2, m2) ->
      let deg1 = totalDegree m1
          deg2 = totalDegree m2
      in compare deg2 deg1 <> compare m2 m1)

    totalDegree (Monomial vars) = sum (M.elems vars)

    formatTerms [] = "0"
    formatTerms [(c, mono)] = formatFirstTerm c mono
    formatTerms ((c, mono):rest) = formatFirstTerm c mono ++ concatMap (uncurry formatOtherTerm) rest

    formatFirstTerm c mono
      | M.null (let Monomial m = mono in m) = prettyRational c
      | c == 1 = formatMonomial mono
      | c == -1 = "-" ++ formatMonomial mono
      | otherwise = prettyRational c ++ formatMonomial mono

    formatOtherTerm c mono
      | c < 0 = " - " ++ formatCoeff (abs c) mono
      | otherwise = " + " ++ formatCoeff c mono

    formatCoeff c mono
      | M.null (let Monomial m = mono in m) = prettyRational c
      | c == 1 = formatMonomial mono
      | otherwise = prettyRational c ++ formatMonomial mono

    formatMonomial (Monomial m)
      | M.null m = ""
      | otherwise = concatMap formatVar (M.toAscList m)

    formatVar (v, 1) = v
    formatVar (v, e) = v ++ "^" ++ show e

-- =============================================
-- Expression Simplification
-- =============================================

-- | Substitute a variable with an expression
substituteExpr :: String -> Expr -> Expr -> Expr
substituteExpr var val (Var v) | v == var = val
substituteExpr _ _ (Var v) = Var v
substituteExpr _ _ (Const c) = Const c
substituteExpr var val (Add e1 e2) = Add (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Sub e1 e2) = Sub (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Mul e1 e2) = Mul (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Div e1 e2) = Div (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Pow e n) = Pow (substituteExpr var val e) n
substituteExpr _ _ e = e

-- | Substitute multiple variables
substituteAll :: M.Map String Expr -> Expr -> Expr
substituteAll subMap (Var v) = M.findWithDefault (Var v) v subMap
substituteAll _ (Const c) = Const c
substituteAll subMap (Add e1 e2) = Add (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Sub e1 e2) = Sub (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Mul e1 e2) = Mul (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Div e1 e2) = Div (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Pow e n) = Pow (substituteAll subMap e) n
substituteAll _ e = e

-- | Check if two expressions are symbolically equal
-- Tries to convert to polynomials for robust equality checking
-- Falls back to structural equality (after simplification) if conversion fails (e.g. division)
exprEqualsSymbolic :: Expr -> Expr -> Bool
exprEqualsSymbolic e1 e2 = 
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in if s1 == s2 then True -- Fast path: identical structure
     else 
       -- Try polynomial equivalence
       -- Catch errors from toPoly (like Division)
       if hasDivision s1 || hasDivision s2 
       then False -- Cannot prove equality with Division easily yet
       else toPoly s1 == toPoly s2

hasDivision :: Expr -> Bool
hasDivision (Div _ _) = True
hasDivision (Add e1 e2) = hasDivision e1 || hasDivision e2
hasDivision (Sub e1 e2) = hasDivision e1 || hasDivision e2
hasDivision (Mul e1 e2) = hasDivision e1 || hasDivision e2
hasDivision (Pow e _) = hasDivision e
hasDivision _ = False

-- Simplify an Expr by applying algebraic rules
simplifyExpr :: Expr -> Expr
simplifyExpr (Add e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, e) -> e                    -- 0 + e = e
       (e, Const 0) -> e                    -- e + 0 = e
       (Const r1, Const r2) -> Const (r1 + r2)  -- Fold constants
       _ -> Add s1 s2

simplifyExpr (Sub e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (e, Const 0) -> e                    -- e - 0 = e
       (Const 0, e) -> Mul (Const (-1)) e   -- 0 - e = -e
       (Const r1, Const r2) -> Const (r1 - r2)
       _ | s1 == s2 -> Const 0              -- e - e = 0
       _ -> Sub s1 s2

simplifyExpr (Mul e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, _) -> Const 0              -- 0 * e = 0
       (_, Const 0) -> Const 0              -- e * 0 = 0
       (Const 1, e) -> e                    -- 1 * e = e
       (e, Const 1) -> e                    -- e * 1 = e
       (Const (-1), Const (-1)) -> Const 1  -- -1 * -1 = 1
       (Const r1, Const r2) -> Const (r1 * r2)
       _ -> Mul s1 s2

simplifyExpr (Div e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, _) -> Const 0              -- 0 / e = 0
       (e, Const 1) -> e                    -- e / 1 = e
       (Const r1, Const r2) | r2 /= 0 -> Const (r1 / r2)
       _ | s1 == s2 -> Const 1              -- e / e = 1
       _ -> Div s1 s2

simplifyExpr (Pow e 0) = Const 1            -- e^0 = 1
simplifyExpr (Pow e 1) = simplifyExpr e     -- e^1 = e
simplifyExpr (Pow e n) = 
  let s = simplifyExpr e 
  in case s of
       Const r -> Const (r ^ n)
       _ -> Pow s n

simplifyExpr (Determinant rows) =
  let 
      -- 1. Simplify all elements
      simpRows = map (map simplifyExpr) rows
      
      -- 2. Check for zero rows or zero columns
      hasZeroRow = any (all (== Const 0)) simpRows
      
      -- Transpose to check columns
      cols = if null simpRows then [] else transpose simpRows
      hasZeroCol = any (all (== Const 0)) cols
      
      -- 3. Check for dependent rows (identical rows)
      hasIdenticalRows = hasDuplicates simpRows
      
  in if hasZeroRow || hasZeroCol || hasIdenticalRows
     then Const 0
     else 
       -- 4. Symbolic Gaussian Elimination (Bareiss Algorithm - Simplified Step)
       -- If we find a row starting with 0, we can't easily pivot without division (which we avoid).
       -- But if we find triangular form, it's just product of diagonals.
       -- For now, just return the simplified matrix structure to allow lazy expansion later.
       -- Full Bareiss is complex to implement on Expr tree without a proper polynomial type here.
       -- We settle for "Zero/Identity Detection" which handles many geometric cases (coplanarity).
       Determinant simpRows

-- Geometric primitives don't simplify further at Expr level
simplifyExpr e@(Dist2 _ _) = e
simplifyExpr e@(Collinear _ _ _) = e
simplifyExpr e@(Dot _ _ _ _) = e
simplifyExpr e@(Circle _ _ _) = e
simplifyExpr e@(Midpoint _ _ _) = e
simplifyExpr e@(Perpendicular _ _ _ _) = e
simplifyExpr e@(Parallel _ _ _ _) = e

-- Base cases
simplifyExpr e@(Var _) = e
simplifyExpr e@(Const _) = e

-- Helper: Matrix transposition
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- Helper: Check for duplicate rows
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

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
toPoly (Div _ _)   = error "Division Error: Division is not supported in polynomial expressions.\nNote: Rational constants like 1/2 are supported, but division of variables is not.\nContext: Attempting to convert Div expression to polynomial."
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

-- Midpoint: M is midpoint of AB means 2*xM = xA + xB, 2*yM = yA + yB, 2*zM = zA + zB
-- We return the constraint for x-coordinate: 2*xM - xA - xB = 0
-- User should add similar constraints for y and z coordinates
toPoly (Midpoint a b m) =
  let xA = polyFromVar ("x" ++ a); xB = polyFromVar ("x" ++ b); xM = polyFromVar ("x" ++ m)
      yA = polyFromVar ("y" ++ a); yB = polyFromVar ("y" ++ b); yM = polyFromVar ("y" ++ m)
      zA = polyFromVar ("z" ++ a); zB = polyFromVar ("z" ++ b); zM = polyFromVar ("z" ++ m)
      -- Combined constraint: (2xM - xA - xB)² + (2yM - yA - yB)² + (2zM - zA - zB)² = 0
      xConstraint = polySub (polySub (polyMul (polyFromConst 2) xM) xA) xB
      yConstraint = polySub (polySub (polyMul (polyFromConst 2) yM) yA) yB
      zConstraint = polySub (polySub (polyMul (polyFromConst 2) zM) zA) zB
  in polyAdd (polyAdd (polyMul xConstraint xConstraint) (polyMul yConstraint yConstraint)) (polyMul zConstraint zConstraint)

-- Perpendicular: AB ⊥ CD means AB · CD = 0
toPoly (Perpendicular a b c d) = toPoly (Dot a b c d)

-- Parallel: AB ∥ CD means AB × CD = 0 (cross product = 0)
-- In 3D: (AB × CD)_x² + (AB × CD)_y² + (AB × CD)_z² = 0
-- Cross product: u × v = (u_y*v_z - u_z*v_y, u_z*v_x - u_x*v_z, u_x*v_y - u_y*v_x)
toPoly (Parallel a b c d) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a); za = polyFromVar ("z" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b); zb = polyFromVar ("z" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c); zc = polyFromVar ("z" ++ c)
      xd = polyFromVar ("x" ++ d); yd = polyFromVar ("y" ++ d); zd = polyFromVar ("z" ++ d)
      -- Vector AB
      vABx = polySub xb xa; vABy = polySub yb ya; vABz = polySub zb za
      -- Vector CD
      vCDx = polySub xd xc; vCDy = polySub yd yc; vCDz = polySub zd zc
      -- Cross product components
      crossX = polySub (polyMul vABy vCDz) (polyMul vABz vCDy)
      crossY = polySub (polyMul vABz vCDx) (polyMul vABx vCDz)
      crossZ = polySub (polyMul vABx vCDy) (polyMul vABy vCDx)
  in polyAdd (polyAdd (polyMul crossX crossX) (polyMul crossY crossY)) (polyMul crossZ crossZ)

-- Determinant: Recursive expansion (Laplace expansion along first row)
-- For a matrix M, det(M) = sum_{j=1..n} (-1)^(1+j) * M_{1,j} * det(M_{1,j})
-- Base case: 1x1 matrix [a] -> a
toPoly (Determinant rows) = detPoly rows
  where
    detPoly [[x]] = toPoly x
    detPoly m = 
        let 
            firstRow = head m
            restRows = tail m
            n = length firstRow
            
            -- Terms of expansion
            terms = [ let element = firstRow !! colIndex
                          subMatrix = [ removeAt colIndex row | row <- restRows ]
                          sign = if even colIndex then 1 else -1
                          term = polyMul (toPoly element) (detPoly subMatrix)
                      in if sign == 1 then term else polyNeg term
                    | colIndex <- [0..n-1] ]
        in foldl polyAdd polyZero terms

    removeAt i xs = take i xs ++ drop (i+1) xs

-- =============================================
-- Logic
-- =============================================

data Formula 
  = Eq Expr Expr 
  | Ge Expr Expr 
  | Gt Expr Expr 
  deriving (Eq, Show)

type Theory = [Formula]