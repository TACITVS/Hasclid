module Geometry.Barycentric
  ( applyBarycentric
  ) where

import Expr
import qualified Data.Map.Strict as M

-- | Apply Barycentric Coordinate transformation to simplify geometric inequalities.
-- PA^2 = c^2 v^2 + b^2 w^2 + (b^2+c^2-a^2)vw
-- PB^2 = c^2 u^2 + a^2 w^2 + (c^2+a^2-b^2)uw
-- PC^2 = b^2 u^2 + a^2 v^2 + (b^2+a^2-c^2)uv
-- subject to u+v+w=1
applyBarycentric :: [String] -> Theory -> Formula -> (Theory, Formula, [String])
applyBarycentric points theory goal =
  case points of
    (p1:p2:p3:p4:_) -> 
      let 
          -- Triangle: p1, p2, p3. Point: p4.
          a2 = Dist2 p2 p3; b2 = Dist2 p1 p3; c2 = Dist2 p1 p2
          r1s = Dist2 p1 p4; r2s = Dist2 p2 p4; r3s = Dist2 p3 p4
          
          -- Barycentric coords for p4: u, v, w
          u = Var "ba_u"; v = Var "ba_v"; w = Var "ba_w"
          
          -- Substitution rules
          barySub = M.fromList
            [ (prettyExpr r1s, Add (Add (Mul c2 (Mul v v)) (Mul b2 (Mul w w))) (Mul (Sub (Add b2 c2) a2) (Mul v w)))
            , (prettyExpr r2s, Add (Add (Mul c2 (Mul u u)) (Mul a2 (Mul w w))) (Mul (Sub (Add a2 c2) b2) (Mul u w)))
            , (prettyExpr r3s, Add (Add (Mul b2 (Mul u u)) (Mul a2 (Mul v v))) (Mul (Sub (Add a2 b2) c2) (Mul u v)))
            ]
            
          -- Constraint: u + v + w = 1
          constraint = Eq (Add u (Add v w)) (Const 1)
          
          -- New theory and goal
          theory' = constraint : map (substBary barySub) theory
          goal' = substBary barySub goal
          
      in (theory', goal', ["Barycentric: Point " ++ p4 ++ " relative to " ++ p1 ++ p2 ++ p3])
    _ -> (theory, goal, [])

substBary :: M.Map String Expr -> Formula -> Formula
substBary sub (Eq l r) = Eq (substE sub l) (substE sub r)
substBary sub (Ge l r) = Ge (substE sub l) (substE sub r)
substBary sub (Gt l r) = Gt (substE sub l) (substE sub r)
substBary sub (And f1 f2) = And (substBary sub f1) (substBary sub f2)
substBary _ f = f

substE :: M.Map String Expr -> Expr -> Expr
substE sub (Dist2 a b) = 
  let key = prettyExpr (Dist2 a b)
  in M.findWithDefault (Dist2 a b) key sub
substE sub (Add a b) = Add (substE sub a) (substE sub b)
substE sub (Sub a b) = Sub (substE sub a) (substE sub b)
substE sub (Mul a b) = Mul (substE sub a) (substE sub b)
substE sub (Pow e n) = Pow (substE sub e) n
substE sub (Sqrt e) = Sqrt (substE sub e)
substE _ e = e
