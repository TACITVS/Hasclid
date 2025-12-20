module RationalElim
  ( eliminateRational
  ) where

import Expr
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)

-- | Eliminate division by introducing polynomial equivalents.
-- Transforms rational expressions into polynomial systems with sign conditions:
--   f/g >= 0  →  (f >= 0 ∧ g > 0) ∨ (f <= 0 ∧ g < 0)
--   f/g = 0   →  f = 0 ∧ g ≠ 0
--   f1/g1 = f2/g2  →  f1·g2 = f2·g1 ∧ g1 ≠ 0 ∧ g2 ≠ 0
-- MEMOIZATION: Reuses denominator constraints for identical expressions
-- Returns (transformedTheory, transformedGoal) both division-free.
eliminateRational :: Theory -> Formula -> (Theory, Formula)
eliminateRational theory goal =
  let -- First simplify rational arithmetic (combine fractions)
      theory' = map simplifyRationalArithmetic theory
      goal' = simplifyRationalArithmetic goal
      -- Then eliminate divisions
      ((theory'', goal''), (extras, _)) = runState (do
        t' <- mapM elimFormula theory'
        g' <- elimFormula goal'
        return (t', g')) ([], Map.empty)
  in (extras ++ theory'', goal'')

type DivMemo = Map.Map Expr Formula
type ElimM = State ([Formula], DivMemo)

-- | Simplify rational arithmetic before elimination
-- Combines fractions: (a/b) + (c/d) → (ad + bc)/(bd)
-- Handles nested divisions: (a/b)/c → a/(bc), a/(b/c) → (ac)/b
simplifyRationalArithmetic :: Formula -> Formula
simplifyRationalArithmetic (Eq l r) = Eq (simpExprArith l) (simpExprArith r)
simplifyRationalArithmetic (Ge l r) = Ge (simpExprArith l) (simpExprArith r)
simplifyRationalArithmetic (Gt l r) = Gt (simpExprArith l) (simpExprArith r)
simplifyRationalArithmetic (Le l r) = Le (simpExprArith l) (simpExprArith r)
simplifyRationalArithmetic (Lt l r) = Lt (simpExprArith l) (simpExprArith r)
simplifyRationalArithmetic (And f1 f2) = And (simplifyRationalArithmetic f1) (simplifyRationalArithmetic f2)
simplifyRationalArithmetic (Or f1 f2) = Or (simplifyRationalArithmetic f1) (simplifyRationalArithmetic f2)
simplifyRationalArithmetic (Not f) = Not (simplifyRationalArithmetic f)
simplifyRationalArithmetic (Forall v f) = Forall v (simplifyRationalArithmetic f)
simplifyRationalArithmetic (Exists v f) = Exists v (simplifyRationalArithmetic f)

-- | Simplify expression arithmetic with aggressive optimizations
simpExprArith :: Expr -> Expr
-- CRITICAL: Constant division c1/c2 → c (avoids unnecessary CAD complexity!)
simpExprArith (Div (Const a) (Const b))
  | b /= 0 = Const (a / b)
  | otherwise = Div (Const a) (Const b)  -- Keep division by zero symbolic
-- Nested division: (a/b)/c → a/(bc)
simpExprArith (Div (Div a b) c) = simpExprArith (Div a (Mul b c))
-- Nested division: a/(b/c) → (ac)/b
simpExprArith (Div a (Div b c)) = simpExprArith (Div (Mul a c) b)
-- CANCEL: (a*c)/(b*c) → a/b when we can detect common factors (BEFORE recursion to avoid infinite loop)
simpExprArith (Div (Mul a c1) (Mul b c2)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul c1 a) (Mul c2 b)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul c1 a) (Mul b c2)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul a c1) (Mul c2 b)) | c1 == c2 = simpExprArith (Div a b)
-- Addition of fractions: (a/b) + (c/d) → (ad + bc)/(bd)
simpExprArith (Add (Div a b) (Div c d)) =
  simpExprArith (Div (Add (Mul a d) (Mul b c)) (Mul b d))
-- Subtraction of fractions: (a/b) - (c/d) → (ad - bc)/(bd)
simpExprArith (Sub (Div a b) (Div c d)) =
  simpExprArith (Div (Sub (Mul a d) (Mul b c)) (Mul b d))
-- Multiplication of fractions: (a/b) * (c/d) → (ac)/(bd)
simpExprArith (Mul (Div a b) (Div c d)) =
  simpExprArith (Div (Mul a c) (Mul b d))
-- IDENTITY: x * (1/x) → 1 (check before general multiplication - direct pattern)
simpExprArith (Mul a (Div (Const 1) b)) | a == b = Const 1
simpExprArith (Mul (Div (Const 1) a) b) | a == b = Const 1
-- SIMPLIFY: a * (b/c) → (a*b)/c (before general multiplication)
simpExprArith (Mul a (Div b c)) = simpExprArith (Div (Mul a b) c)
-- SIMPLIFY: (a/b) * c → (a*c)/b (before general multiplication)
simpExprArith (Mul (Div a b) c) = simpExprArith (Div (Mul a c) b)
-- Recursive cases with simplification
simpExprArith (Add a b) =
  let sa = simpExprArith a
      sb = simpExprArith b
  in case (sa, sb) of
       (Const 0, x) -> x
       (x, Const 0) -> x
       _ -> Add sa sb
simpExprArith (Sub a b) =
  let sa = simpExprArith a
      sb = simpExprArith b
  in case (sa, sb) of
       (x, Const 0) -> x
       _ | sa == sb -> Const 0
       _ -> Sub sa sb
simpExprArith (Mul a b) =
  let sa = simpExprArith a
      sb = simpExprArith b
  in case (sa, sb) of
       (Const 0, _) -> Const 0
       (_, Const 0) -> Const 0
       (Const 1, x) -> x
       (x, Const 1) -> x
       _ -> Mul sa sb
simpExprArith (Div a b) =
  let sa = simpExprArith a
      sb = simpExprArith b
  in case (sa, sb) of
       (Const 0, _) -> Const 0
       (x, Const 1) -> x
       _ | sa == sb -> Const 1
       _ -> Div sa sb
simpExprArith (Pow e n) = Pow (simpExprArith e) n
simpExprArith (Sqrt e) = Sqrt (simpExprArith e)
simpExprArith (Determinant rows) = Determinant (map (map simpExprArith) rows)
simpExprArith (Circle p c r) = Circle p c (simpExprArith r)
simpExprArith e = e  -- Base cases: Var, Const, geometric primitives

-- | Main formula elimination
elimFormula :: Formula -> ElimM Formula

-- Special case: f/g = 0  →  f = 0 ∧ g ≠ 0
elimFormula (Eq (Div f g) (Const 0)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  addDenominatorNonzero g'
  return (Eq f' (Const 0))

-- Special case: 0 = f/g  →  f = 0 ∧ g ≠ 0
elimFormula (Eq (Const 0) (Div f g)) = elimFormula (Eq (Div f g) (Const 0))

-- Special case: f1/g1 = f2/g2  →  f1·g2 = f2·g1 ∧ g1 ≠ 0 ∧ g2 ≠ 0
elimFormula (Eq (Div f1 g1) (Div f2 g2)) = do
  f1' <- elimExpr f1
  g1' <- elimExpr g1
  f2' <- elimExpr f2
  g2' <- elimExpr g2
  addDenominatorNonzero g1'
  addDenominatorNonzero g2'
  return (Eq (Mul f1' g2') (Mul f2' g1'))

-- Special case: f1/g1 = c (constant)  →  f1 = c·g1 ∧ g1 ≠ 0
elimFormula (Eq (Div f g) c) | not (containsDivExpr c) = do
  f' <- elimExpr f
  g' <- elimExpr g
  c' <- elimExpr c
  addDenominatorNonzero g'
  return (Eq f' (Mul c' g'))

elimFormula (Eq c (Div f g)) | not (containsDivExpr c) = elimFormula (Eq (Div f g) c)

-- Inequality: f/g >= 0  →  (f >= 0 ∧ g > 0) ∨ (f <= 0 ∧ g < 0)
elimFormula (Ge (Div f g) (Const 0)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Ge f' (Const 0)) (Gt g' (Const 0)))
              (And (Le f' (Const 0)) (Lt g' (Const 0)))

elimFormula (Ge (Const 0) (Div f g)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Le f' (Const 0)) (Gt g' (Const 0)))
              (And (Ge f' (Const 0)) (Lt g' (Const 0)))

-- Inequality: f/g > 0  →  (f > 0 ∧ g > 0) ∨ (f < 0 ∧ g < 0)
elimFormula (Gt (Div f g) (Const 0)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Gt f' (Const 0)) (Gt g' (Const 0)))
              (And (Lt f' (Const 0)) (Lt g' (Const 0)))

elimFormula (Gt (Const 0) (Div f g)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Lt f' (Const 0)) (Gt g' (Const 0)))
              (And (Gt f' (Const 0)) (Lt g' (Const 0)))

-- Inequality: f/g <= 0  →  (f <= 0 ∧ g > 0) ∨ (f >= 0 ∧ g < 0)
elimFormula (Le (Div f g) (Const 0)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Le f' (Const 0)) (Gt g' (Const 0)))
              (And (Ge f' (Const 0)) (Lt g' (Const 0)))

elimFormula (Le (Const 0) (Div f g)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Ge f' (Const 0)) (Gt g' (Const 0)))
              (And (Le f' (Const 0)) (Lt g' (Const 0)))

-- Inequality: f/g < 0  →  (f < 0 ∧ g > 0) ∨ (f > 0 ∧ g < 0)
elimFormula (Lt (Div f g) (Const 0)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Lt f' (Const 0)) (Gt g' (Const 0)))
              (And (Gt f' (Const 0)) (Lt g' (Const 0)))

elimFormula (Lt (Const 0) (Div f g)) = do
  f' <- elimExpr f
  g' <- elimExpr g
  return $ Or (And (Gt f' (Const 0)) (Gt g' (Const 0)))
              (And (Lt f' (Const 0)) (Lt g' (Const 0)))

-- General comparison with divisions: f1/g1 OP f2/g2  →  f1·g2 OP f2·g1 (with sign handling)
-- For now, cross-multiply and add nonzero constraints
-- TODO: This is not complete - need to handle sign carefully
elimFormula (Ge (Div f1 g1) (Div f2 g2)) = do
  f1' <- elimExpr f1
  g1' <- elimExpr g1
  f2' <- elimExpr f2
  g2' <- elimExpr g2
  addDenominatorNonzero g1'
  addDenominatorNonzero g2'
  -- For now, assume positive denominators (will be improved)
  return (Ge (Mul f1' g2') (Mul f2' g1'))

-- Generic fallback: traverse and eliminate divisions in expressions
elimFormula (Eq l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Eq l' r')

elimFormula (Ge l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Ge l' r')

elimFormula (Gt l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Gt l' r')

elimFormula (Le l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Le l' r')

elimFormula (Lt l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Lt l' r')

elimFormula (And f1 f2) = And <$> elimFormula f1 <*> elimFormula f2
elimFormula (Or f1 f2) = Or <$> elimFormula f1 <*> elimFormula f2
elimFormula (Not f) = Not <$> elimFormula f
elimFormula (Forall vars f) = Forall vars <$> elimFormula f
elimFormula (Exists vars f) = Exists vars <$> elimFormula f

-- | Eliminate divisions from expressions
-- First apply algebraic simplifications, then eliminate remaining divisions
elimExpr :: Expr -> ElimM Expr
elimExpr e =
  let simplified = simpExprArith e  -- Apply algebraic simplifications first!
  in elimExpr' simplified

-- | Internal elimination after simplification
elimExpr' :: Expr -> ElimM Expr
elimExpr' (Add a b) = Add <$> elimExpr' a <*> elimExpr' b
elimExpr' (Sub a b) = Sub <$> elimExpr' a <*> elimExpr' b
elimExpr' (Mul a b) = Mul <$> elimExpr' a <*> elimExpr' b
-- Division in expression context: leave as-is (will be handled at formula level)
-- However, we can do some smart simplifications:
-- x/x → 1 (with x ≠ 0) - though simpExprArith should have caught this
elimExpr' (Div a b)
  | a == b = do
      a' <- elimExpr' a
      addDenominatorNonzero a'
      return (Const 1)
  | otherwise = Div <$> elimExpr' a <*> elimExpr' b
elimExpr' (Pow e n) = Pow <$> elimExpr' e <*> pure n
elimExpr' (Sqrt e) = Sqrt <$> elimExpr' e
elimExpr' (Sum i lo hi body) = Sum i <$> elimExpr' lo <*> elimExpr' hi <*> elimExpr' body
elimExpr' (Determinant rows) = Determinant <$> mapM (mapM elimExpr') rows
elimExpr' (Circle p c r) = Circle p c <$> elimExpr' r
-- Geometric primitives and base cases
elimExpr' e@(Dist2 _ _) = return e
elimExpr' e@(Collinear _ _ _) = return e
elimExpr' e@(Dot _ _ _ _) = return e
elimExpr' e@(Midpoint _ _ _) = return e
elimExpr' e@(Perpendicular _ _ _ _) = return e
elimExpr' e@(Parallel _ _ _ _) = return e
elimExpr' e@(AngleEq2D _ _ _ _ _ _) = return e
elimExpr' e@(AngleEq2DAbs _ _ _ _ _ _) = return e
elimExpr' e@(Var _) = return e
elimExpr' e@(Const _) = return e
elimExpr' e@(IntVar _) = return e
elimExpr' e@(IntConst _) = return e

-- | Add constraint that denominator is nonzero: g ≠ 0  ≡  (g > 0 ∨ g < 0)
-- OPTIMIZATION: If g is clearly positive (e.g. Dist2, squared altitude), only add g > 0.
addDenominatorNonzero :: Expr -> ElimM ()
addDenominatorNonzero den = do
  (constraints, memo) <- get
  case Map.lookup den memo of
    Just _ -> return ()  -- Already added this constraint
    Nothing -> do
      let isPositive = isGeomPositive den
          constraint = if isPositive
                       then Gt den (Const 0)
                       else Or (Gt den (Const 0)) (Lt den (Const 0))
      put (constraint : constraints, Map.insert den constraint memo)

-- | Heuristic: Is an expression definitely positive based on geometric axioms?
isGeomPositive :: Expr -> Bool
isGeomPositive (Dist2 _ _) = True
isGeomPositive (Pow e 2) = True
isGeomPositive (Sqrt _) = True
isGeomPositive (Mul a b) = isGeomPositive a && isGeomPositive b
isGeomPositive (Add a b) = isGeomPositive a && isGeomPositive b
isGeomPositive (Var v) = 
  -- Side lengths and distance variables are usually positive
  v `elem` ["a", "b", "c", "R1", "R2", "R3", "ha", "hb", "hc"] || 
  "ba_" `isPrefixOf` v -- Barycentric coords are non-negative for inside points
isGeomPositive _ = False
