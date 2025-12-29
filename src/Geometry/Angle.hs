module Geometry.Angle
  ( eliminateAngles
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (nub)

-- | Eliminate Angle expressions by converting to algebraic constraints
eliminateAngles :: Theory -> Formula -> (Theory, Formula)
eliminateAngles theory goal =
  let
    -- Simple pass: rewrite Angle equations
    goal' = rewriteAngleFormula goal
    theory' = map rewriteAngleFormula theory
  in (theory', goal')

rewriteAngleFormula :: Formula -> Formula
rewriteAngleFormula f =
  case f of
    -- angle(A,B,C) + angle(D,E,F) = angle(G,H,I)
    Eq (Add (Angle a b c) (Angle d e f)) (Angle g h i) ->
      complexAngleSum [ (a,b,c), (d,e,f) ] (g,h,i)
      
    -- angle(A,B,C) = angle(D,E,F)
    Eq (Angle a b c) (Angle d e f) ->
      complexAngleSum [ (a,b,c) ] (d,e,f)
      
    -- Generic: sum of angles = sum of angles
    Eq lhs rhs ->
      case (collectAngles lhs, collectAngles rhs) of
        (Just leftAngles, Just rightAngles) ->
           complexAngleEq leftAngles rightAngles
        _ -> f -- Fallback
        
    _ -> f

-- Collect list of angle triplets from expression: angle(A)+angle(B)...
collectAngles :: Expr -> Maybe [(String, String, String)]
collectAngles (Angle a b c) = Just [(a,b,c)]
collectAngles (Add e1 e2) = (++) <$> collectAngles e1 <*> collectAngles e2
collectAngles _ = Nothing

-- Generate constraint for Sum(Angles L) = Sum(Angles R)
-- Product(Rotations L) = Product(Rotations R) * RealPositive
complexAngleEq :: [(String, String, String)] -> [(String, String, String)] -> Formula
complexAngleEq leftAngles rightAngles =
  let
    -- Rotation for ABC is (C-B)/(A-B)
    -- We work with vectors: u = A-B, v = C-B.
    -- Rotation is v/u.
    -- Product L = (v1/u1) * (v2/u2) ...
    -- Product R = (w1/t1) * ...
    -- L = R * K (K > 0)
    -- L/R is Real Positive.
    -- Let Num/Den be the rational complex function.
    -- We need Im(L/R) = 0 and Re(L/R) > 0.
    
    (lnum, lden) = productRotations leftAngles
    (rnum, rden) = productRotations rightAngles
    
    -- L = lnum/lden. R = rnum/rden.
    -- L/R = (lnum * rden) / (lden * rnum) = Z
    
    zNum = complexMul lnum rden
    zDen = complexMul lden rnum
    
    -- Z is real positive means Z = k > 0.
    -- Z = zNum / zDen.
    -- zNum / zDen = k. zNum = k * zDen.
    -- Treat as vectors: zNum is parallel to zDen and same direction.
    -- Im(zNum * conj(zDen)) = 0  (Collinear)
    -- Re(zNum * conj(zDen)) > 0  (Same direction)
    
    cross = complexCross zNum zDen
    dot   = complexDot zNum zDen
    
  in And (Eq cross (Const 0)) (Gt dot (Const 0))

complexAngleSum :: [(String, String, String)] -> (String, String, String) -> Formula
complexAngleSum inputs output = complexAngleEq inputs [output]

-- Complex Number Representation: (Real Expr, Imag Expr)
type ComplexExpr = (Expr, Expr)

-- Vector from P1 to P2: (x2-x1) + i(y2-y1)
vectorC :: String -> String -> ComplexExpr
vectorC p1 p2 =
  ( Sub (Var ("x"++p2)) (Var ("x"++p1))
  , Sub (Var ("y"++p2)) (Var ("y"++p1))
  )

-- Rotation u->v is v/u
-- Product of rotations: Numerators product / Denominators product
productRotations :: [(String, String, String)] -> (ComplexExpr, ComplexExpr)
productRotations triplets =
  let
    -- for ABC: u=BA (A-B), v=BC (C-B). rot = v/u.
    vecs = [ (vectorC b c, vectorC b a) | (a,b,c) <- triplets ]
    nums = map fst vecs
    dens = map snd vecs
    
    prodNum = foldr complexMul (Const 1, Const 0) nums
    prodDen = foldr complexMul (Const 1, Const 0) dens
  in (prodNum, prodDen)

complexMul :: ComplexExpr -> ComplexExpr -> ComplexExpr
complexMul (r1, i1) (r2, i2) =
  ( Sub (Mul r1 r2) (Mul i1 i2)
  , Add (Mul r1 i2) (Mul r2 i1)
  )

-- Im(z1 * conj(z2)) = r1*i2 - i1*r2 (Cross product)
complexCross :: ComplexExpr -> ComplexExpr -> Expr
complexCross (r1, i1) (r2, i2) = Sub (Mul r1 i2) (Mul i1 r2)

-- Re(z1 * conj(z2)) = r1*r2 + i1*i2 (Dot product)
complexDot :: ComplexExpr -> ComplexExpr -> Expr
complexDot (r1, i1) (r2, i2) = Add (Mul r1 r2) (Mul i1 i2)
