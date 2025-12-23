module Heuristics
  ( tryRaviSubstitution
  , tryTangentSubstitution
  , trySymmetryBreaking
  , tryHeronSubstitution
  , tryCotangentSubstitution
  ) where

import Expr
import Data.List (nub, sort, find)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

-- | Ravi Substitution: a = y+z, b = z+x, c = x+y
-- Applicable when a, b, c are sides of a triangle.
-- We look for variables named a, b, c or a2, b2, c2.
tryRaviSubstitution :: Theory -> Formula -> (Theory, Formula, [String])
tryRaviSubstitution theory goal =
  let vars = nub $ concatMap varsInFormula (goal : theory)
      -- Look for a, b, c
      hasABC = all (`elem` vars) ["a", "b", "c"]
      -- Look for a2, b2, c2 (standard squared sides)
      hasA2B2C2 = all (`elem` vars) ["a2", "b2", "c2"]
      _ = if hasABC || hasA2B2C2 then trace ("Ravi detected: " ++ show vars) () else ()
  in if hasABC then applyRavi ["a", "b", "c"] theory goal False
     else if hasA2B2C2 then applyRavi ["a2", "b2", "c2"] theory goal True
     else (theory, goal, [])

applyRavi :: [String] -> Theory -> Formula -> Bool -> (Theory, Formula, [String])
applyRavi [va, vb, vc] theory goal isSquared =
  let x = Var "rv_x"; y = Var "rv_y"; z = Var "rv_z"
      (exprA, exprB, exprC) = if isSquared
                              then (Pow (Add y z) 2, Pow (Add z x) 2, Pow (Add x y) 2)
                              else (Add y z, Add z x, Add x y)
      subs = M.fromList [(va, exprA), (vb, exprB), (vc, exprC)]
      
      -- Also substitute S2 (Squared Area) if present
      -- S2 = xyz(x+y+z)
      s2Subs = if "S2" `elem` (nub $ concatMap varsInFormula (goal : theory))
               then M.insert "S2" (Mul (Mul x (Mul y z)) (Add x (Add y z))) subs
               else subs
      
      -- 16S2 = 16xyz(x+y+z)
      s16Subs = if "S16" `elem` (nub $ concatMap varsInFormula (goal : theory))
                then M.insert "S16" (Mul (Const 16) (Mul (Mul x (Mul y z)) (Add x (Add y z)))) s2Subs
                else s2Subs

      newTheory = [Gt x (Const 0), Gt y (Const 0), Gt z (Const 0)] ++ 
                  map (applySubstitutionsFormula s16Subs) theory
      newGoal = applySubstitutionsFormula s16Subs goal
  in (newTheory, newGoal, ["Applied Ravi Substitution: " ++ va ++ "," ++ vb ++ "," ++ vc ++ " -> x,y,z"])
applyRavi _ theory goal _ = (theory, goal, [])

-- | Tangent Substitution: x=tan(A/2), y=tan(B/2), z=tan(C/2)
-- x,y,z > 0 and xy + yz + zx = 1
-- Then a = 4R x / (1+x^2), etc. or more simply:
-- a = (1+x^2)yz, b = (1+y^2)zx, c = (1+z^2)xy (not quite)
-- Standard: a = y+z, b = z+x, c = x+y is usually better (Ravi).
-- But if the problem has tan(A), tan(B), tan(C):
-- tan A + tan B + tan C = tan A * tan B * tan C
tryTangentSubstitution :: Theory -> Formula -> (Theory, Formula, [String])
tryTangentSubstitution theory goal =
  let vars = nub $ concatMap varsInFormula (goal : theory)
      hasTans = all (`elem` vars) ["tanA", "tanB", "tanC"]
  in if hasTans
     then let x = Var "tanA"; y = Var "tanB"; z = Var "tanC"
              constraint = Eq (Add x (Add y z)) (Mul x (Mul y z))
              -- For acute triangles, tanA, tanB, tanC > 0
              positivity = [Gt x (Const 0), Gt y (Const 0), Gt z (Const 0)]
          in (positivity ++ [constraint] ++ theory, goal, ["Applied Tangent Identity: tanA+tanB+tanC = tanA*tanB*tanC"])
     else (theory, goal, [])

-- | Apply Heron's Formula if S2 or S16 is present but not defined
tryHeronSubstitution :: Theory -> Formula -> (Theory, Formula, [String])
tryHeronSubstitution theory goal =
  let vars = nub $ concatMap varsInFormula (goal : theory)
      hasABC = all (`elem` vars) ["a", "b", "c"]
      hasA2B2C2 = all (`elem` vars) ["a2", "b2", "c2"]
      hasS2 = "S2" `elem` vars
      hasS16 = "S16" `elem` vars
      
      heron16 [a2, b2, c2] = 
        Sub (Add (Mul (Const 2) (Mul a2 b2)) (Add (Mul (Const 2) (Mul b2 c2)) (Mul (Const 2) (Mul c2 a2))))
            (Add (Pow a2 2) (Add (Pow b2 2) (Pow c2 2)))
      
      heron16_abc [a, b, c] = 
        let s = Div (Add a (Add b c)) (Const 2)
        in Mul (Const 16) (Mul s (Mul (Sub s a) (Mul (Sub s b) (Sub s c))))

  in if hasS16 && hasA2B2C2 && not (isDefined "S16" theory)
     then let h = heron16 [Var "a2", Var "b2", Var "c2"]
          in ([Eq (Var "S16") h] ++ theory, goal, ["Applied Heron's Formula for S16"])
     else if hasS16 && hasABC && not (isDefined "S16" theory)
     then let h = heron16_abc [Var "a", Var "b", Var "c"]
          in ([Eq (Var "S16") h] ++ theory, goal, ["Applied Heron's Formula for S16"])
     else (theory, goal, [])

-- | Cotangent Substitution: 
-- (b2+c2-a2) = 4S * x, (a2+c2-b2) = 4S * y, (a2+b2-c2) = 4S * z
-- where xy+yz+zx = 1 and S is area.
-- To avoid Sqrt, we use:
-- (b2+c2-a2)^2 = S16 * x^2
-- a2 = 2S(y+z), b2 = 2S(x+z), c2 = 2S(x+y)
-- We can use a trick: Let S4 = 4S. Then S16 = S4^2.
-- a2 = S4/2 * (y+z) => 2*a2 = S4 * (y+z)
-- This still has S4.
-- But if we only have TermA, TermB, TermC and S16, we can substitute them directly!
tryCotangentSubstitution :: Theory -> Formula -> (Theory, Formula, [String])
tryCotangentSubstitution theory goal =
  let vars = nub $ concatMap varsInFormula (goal : theory)
      hasA2B2C2 = all (`elem` vars) ["a2", "b2", "c2"]
      hasS16 = "S16" `elem` vars
      
      -- Look for TermA, TermB, TermC or define them
      termA = Sub (Add (Var "b2") (Var "c2")) (Var "a2")
      termB = Sub (Add (Var "a2") (Var "c2")) (Var "b2")
      termC = Sub (Add (Var "a2") (Var "b2")) (Var "c2")
      
      -- If we see TermA, TermB, TermC, we can substitute their SQUARES to avoid Sqrt(S16)
      -- But better: just use a symbolic area variable 'area4' such that area4^2 = S16
      s4 = Var "ct_s4"
      x = Var "ct_x"; y = Var "ct_y"; z = Var "ct_z"
      
      subs = M.fromList
        [ ("a2", Div (Mul s4 (Add y z)) (Const 2))
        , ("b2", Div (Mul s4 (Add x z)) (Const 2))
        , ("c2", Div (Mul s4 (Add x y)) (Const 2))
        , ("S16", Pow s4 2)
        , ("S2", Div (Pow s4 2) (Const 16))
        , ("TermA", Mul s4 x)
        , ("TermB", Mul s4 y)
        , ("TermC", Mul s4 z)
        ]
      
      constraint = Eq (Add (Mul x y) (Add (Mul y z) (Mul z x))) (Const 1)
      positivity = [Gt x (Const 0), Gt y (Const 0), Gt z (Const 0), Gt s4 (Const 0)]
      
  in if hasA2B2C2 && hasS16
     then 
       let newTheory = [Eq s4 (Const 1)] ++ positivity ++ [constraint] ++ map (applySubstitutionsFormula subs) theory
           newGoal = applySubstitutionsFormula subs goal
       in (newTheory, newGoal, ["Applied Cotangent Substitution (Scale WLOG s4=1): a2,b2,c2,S16 -> x,y,z"])
     else (theory, goal, [])

isDefined :: String -> Theory -> Bool
isDefined v theory = any isDef theory
  where
    isDef (Eq (Var x) _) = x == v
    isDef _ = False


-- | Symmetry Breaking: Assume a <= b <= c
trySymmetryBreaking :: Theory -> Formula -> (Theory, Formula, [String])
trySymmetryBreaking theory goal =
  let vars = nub $ concatMap varsInFormula (goal : theory)
      abc = ["a", "b", "c"]
      a2b2c2 = ["a2", "b2", "c2"]
      target = if all (`elem` vars) abc then Just abc
               else if all (`elem` vars) a2b2c2 then Just a2b2c2
               else Nothing
  in case target of
       Just [v1, v2, v3] ->
         let extra = [Le (Var v1) (Var v2), Le (Var v2) (Var v3)]
         in (extra ++ theory, goal, ["Applied Symmetry Breaking: " ++ v1 ++ " <= " ++ v2 ++ " <= " ++ v3])
       _ -> (theory, goal, [])

-- Helper (duplicated from Preprocessing to avoid cycle if needed, 
-- but better to import if we can)
-- For now, let's just use what's available or re-implement if simple.
varsInFormula :: Formula -> [String]
varsInFormula (Eq e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Ge e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Gt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Le e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Lt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (And f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Or f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Not f) = varsInFormula f
varsInFormula (Forall _ f) = varsInFormula f
varsInFormula (Exists _ f) = varsInFormula f
varsInFormula _ = []

varsInExpr :: Expr -> [String]
varsInExpr (Var v) = [v]
varsInExpr (Add e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Sub e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Mul e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Div e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Pow e _) = varsInExpr e
varsInExpr (Sqrt e) = varsInExpr e
varsInExpr _ = []

applySubstitutionsFormula :: M.Map String Expr -> Formula -> Formula
applySubstitutionsFormula subs (Eq e1 e2) = Eq (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsFormula subs (Ge e1 e2) = Ge (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsFormula subs (Gt e1 e2) = Gt (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsFormula subs (Le e1 e2) = Le (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsFormula subs (Lt e1 e2) = Lt (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsFormula subs (And f1 f2) = And (applySubstitutionsFormula subs f1) (applySubstitutionsFormula subs f2)
applySubstitutionsFormula subs (Or f1 f2) = Or (applySubstitutionsFormula subs f1) (applySubstitutionsFormula subs f2)
applySubstitutionsFormula subs (Not f) = Not (applySubstitutionsFormula subs f)
applySubstitutionsFormula _ f = f

applySubstitutionsExpr :: M.Map String Expr -> Expr -> Expr
applySubstitutionsExpr subs (Var v) = M.findWithDefault (Var v) v subs
applySubstitutionsExpr subs (Add e1 e2) = Add (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsExpr subs (Sub e1 e2) = Sub (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsExpr subs (Mul e1 e2) = Mul (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsExpr subs (Div e1 e2) = Div (applySubstitutionsExpr subs e1) (applySubstitutionsExpr subs e2)
applySubstitutionsExpr subs (Pow e n) = Pow (applySubstitutionsExpr subs e) n
applySubstitutionsExpr subs (Sqrt e) = Sqrt (applySubstitutionsExpr subs e)
applySubstitutionsExpr _ e = e
