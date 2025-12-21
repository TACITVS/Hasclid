module Geometry.Barycentric
  ( applyBarycentric
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (find)

-- | Apply Barycentric Coordinate transformation to simplify geometric inequalities.
-- COMPLETELY REPLACES Cartesian model with Barycentric model.
applyBarycentric :: [String] -> Theory -> Formula -> (Theory, Formula, [String])
applyBarycentric points theory goal =
  case points of
    (p1:p2:p3:p4:_) -> 
      let 
          -- Abstract Side Lengths (squared)
          a2 = Var "a2"; b2 = Var "b2"; c2 = Var "c2"
          
          -- Barycentric coords for p4: u, v, w
          u = Var "ba_u"; v = Var "ba_v"; w = Var "ba_w"
          
          -- Barycentric Distance Formulas (Squared)
          -- PA^2 = c^2 v^2 + b^2 w^2 + (b^2+c^2-a^2)vw
          distP1P4 = Add (Add (Mul c2 (Mul v v)) (Mul b2 (Mul w w))) (Mul (Sub (Add b2 c2) a2) (Mul v w))
          -- PB^2 = c^2 u^2 + a^2 w^2 + (c^2+a^2-b^2)uw
          distP2P4 = Add (Add (Mul c2 (Mul u u)) (Mul a2 (Mul w w))) (Mul (Sub (Add a2 c2) b2) (Mul u w))
          -- PC^2 = b^2 u^2 + a^2 v^2 + (b^2+a^2-c^2)uv
          distP3P4 = Add (Add (Mul b2 (Mul u u)) (Mul a2 (Mul v v))) (Mul (Sub (Add a2 b2) c2) (Mul u v))

          -- Substitution Map: Explicitly map Dist2 calls to these formulas
          barySub = M.fromList
            [ (prettyExpr (Dist2 p1 p4), distP1P4)
            , (prettyExpr (Dist2 p2 p4), distP2P4)
            , (prettyExpr (Dist2 p3 p4), distP3P4)
            , (prettyExpr (Dist2 p4 p1), distP1P4)
            , (prettyExpr (Dist2 p4 p2), distP2P4)
            , (prettyExpr (Dist2 p4 p3), distP3P4)
            , (prettyExpr (Dist2 p2 p3), a2)
            , (prettyExpr (Dist2 p3 p2), a2)
            , (prettyExpr (Dist2 p1 p3), b2)
            , (prettyExpr (Dist2 p3 p1), b2)
            , (prettyExpr (Dist2 p1 p2), c2)
            , (prettyExpr (Dist2 p2 p1), c2)
            ]
            
          -- Constraint: u + v + w = 1
          sumConstraint = Eq (Add u (Add v w)) (Const 1)
          
          -- Positivity Constraints for Side Squares
          posConstraints = [Gt a2 (Const 0), Gt b2 (Const 0), Gt c2 (Const 0)]

          -- REWRITE THEORY:
          -- 1. Apply substitutions to everything.
          -- 2. FILTER out any definitions that were purely Cartesian (now redundant).
          --    e.g. "R1s = Dist2 A P" becomes "R1s = <bary_poly>". Keep it.
          --    e.g. "a2 = Dist2 B C" becomes "a2 = a2". Tautology. Remove it.
          --    e.g. "xA = 0". Remove it (Cartesian).
          
          transformFormula f = substBary barySub f
          
          isRedundant (Eq l r) = l == r
          isRedundant _ = False  -- Other formulas are not redundant definitions
          
          isCartesian (Eq (Var v) _) = "x" `isPrefixOf` v || "y" `isPrefixOf` v || "z" `isPrefixOf` v
          isCartesian _ = False
          
          -- Helper to check prefix
          isPrefixOf [] _ = True
          isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
          isPrefixOf _ [] = False

          theory' = posConstraints ++ [sumConstraint] ++ 
                    filter (\f -> not (isRedundant f) && not (isCartesian f)) (map transformFormula theory)
          
          goal' = transformFormula goal
          
      in (theory', goal', ["Barycentric Model: Point " ++ p4 ++ " relative to " ++ p1 ++ p2 ++ p3])
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
substE sub (Div a b) = Div (substE sub a) (substE sub b) -- Added Div support
substE _ e = e
