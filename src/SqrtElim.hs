module SqrtElim
  ( eliminateSqrt
  ) where

import Expr
import Control.Monad.State
import qualified Data.Map.Strict as Map

-- | Eliminate sqrt by introducing auxiliary variables.
-- Each (sqrt e) becomes a fresh variable v with constraints:
--   v^2 = e    and    v >= 0    and    e >= 0
-- MEMOIZATION: Reuses same auxiliary variable for identical sqrt expressions
-- Special rewrites:
--   sqrt(t^2)  -> t with added constraint t >= 0
--   sqrt a >= sqrt b  ->  a >= b  AND a >= 0 AND b >= 0
--   sqrt a >  sqrt b  ->  a >  b  AND a >= 0 AND b >= 0
--   sqrt a =  sqrt b  ->  a = b   AND a >= 0 AND b >= 0
--   sqrt a == b       ->  a == b^2 AND a >= 0 AND b >= 0
--   sqrt a >= b       ->  a >= b^2 AND a >= 0 AND b >= 0
--   sqrt a >  b       ->  a >  b^2 AND a >= 0 AND b >= 0
-- Returns (transformedTheory, transformedGoal) both sqrt-free.
eliminateSqrt :: Theory -> Formula -> (Theory, Formula)
eliminateSqrt theory goal =
  let ((theory', goal'), (extras, _)) = runState (do
        t' <- mapM elimFormula theory
        g' <- elimFormula goal
        return (t', g')) ([], Map.empty)
  in (extras ++ theory', goal')

type SqrtMemo = Map.Map Expr String
type ElimM = State ([Formula], SqrtMemo)

freshVar :: ElimM String
freshVar = do
  (_, memo) <- get
  let idx = Map.size memo + 1
  return ("sqrt_aux" ++ show idx)

-- Helper: Check if expression contains sqrt
containsSqrt :: Expr -> Bool
containsSqrt (Sqrt _) = True
containsSqrt (Add a b) = containsSqrt a || containsSqrt b
containsSqrt (Sub a b) = containsSqrt a || containsSqrt b
containsSqrt (Mul a b) = containsSqrt a || containsSqrt b
containsSqrt (Div a b) = containsSqrt a || containsSqrt b
containsSqrt (Pow e _) = containsSqrt e
containsSqrt _ = False

-- Helper: Check if sqrt appears in a SUM (Add/Sub) context
-- This is the case that smart simplifications can't handle
containsSqrtInSum :: Expr -> Bool
containsSqrtInSum (Add a b) = containsSqrt a || containsSqrt b
containsSqrtInSum (Sub a b) = containsSqrt a || containsSqrt b
containsSqrtInSum (Mul a b) = containsSqrtInSum a || containsSqrtInSum b
containsSqrtInSum (Div a b) = containsSqrtInSum a || containsSqrtInSum b
containsSqrtInSum (Pow e _) = containsSqrtInSum e
containsSqrtInSum _ = False

elimFormula :: Formula -> ElimM Formula
elimFormula (Eq (Sqrt a) (Sqrt b)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Eq a' b')
elimFormula (Ge (Sqrt a) (Sqrt b)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Ge a' b')
elimFormula (Gt (Sqrt a) (Sqrt b)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Gt a' b')
-- sqrt a ? b (non-sqrt)
elimFormula (Eq (Sqrt a) b) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Eq a' (Pow b' 2))
elimFormula (Eq b (Sqrt a)) = elimFormula (Eq (Sqrt a) b)
elimFormula (Ge (Sqrt a) b) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Ge a' (Pow b' 2))
elimFormula (Ge b (Sqrt a)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Ge (Pow b' 2) a')
elimFormula (Gt (Sqrt a) b) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Gt a' (Pow b' 2))
elimFormula (Gt b (Sqrt a)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Gt (Pow b' 2) a')
-- Le and Lt cases (newly added operators)
elimFormula (Le (Sqrt a) (Sqrt b)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Le a' b')
elimFormula (Lt (Sqrt a) (Sqrt b)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Lt a' b')
elimFormula (Le (Sqrt a) b) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Le a' (Pow b' 2))
elimFormula (Le b (Sqrt a)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Le (Pow b' 2) a')
elimFormula (Lt (Sqrt a) b) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Lt a' (Pow b' 2))
elimFormula (Lt b (Sqrt a)) = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Lt (Pow b' 2) a')
-- Automatic squaring for equations containing sqrt in SUMS
-- This handles cases like: sqrt(a) = sqrt(b) + sqrt(c)
-- By squaring: a = (sqrt(b) + sqrt(c))^2
-- NOTE: Only triggers for sqrt in Add/Sub contexts to avoid infinite loops
-- Simple cases like sqrt(x)*sqrt(x)=x are handled by smart simplifications
elimFormula (Eq l r)
  | containsSqrtInSum l || containsSqrtInSum r = do
      -- Square both sides: (l)^2 = (r)^2
      -- Use elimExpr (not recursive elimFormula) to avoid infinite squaring
      -- The expansion (a+b)^2 still has sqrt in sums, but elimExpr handles it
      l' <- elimExpr (Mul l l)
      r' <- elimExpr (Mul r r)
      return (Eq l' r')
elimFormula (Eq l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Eq l' r')
-- Smart squaring for inequalities with sqrt in sums
-- e.g., sqrt(a) + sqrt(b) >= sqrt(c) becomes (sqrt(a) + sqrt(b))^2 >= c
-- ITERATIVE: Keep squaring until no sqrt in sums remain
elimFormula (Ge l r)
  | containsSqrtInSum l || containsSqrtInSum r = do
      -- Square both sides (valid since all sqrt are >= 0)
      l' <- elimExpr (Mul l l)
      r' <- elimExpr (Mul r r)
      -- Add non-negativity constraints for the original expressions
      addNonNegConstraints l
      addNonNegConstraints r
      -- Recursively eliminate if still has sqrt in sums
      elimFormula (Ge l' r')
elimFormula (Ge l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Ge l' r')
elimFormula (Gt l r)
  | containsSqrtInSum l || containsSqrtInSum r = do
      -- Square both sides
      l' <- elimExpr (Mul l l)
      r' <- elimExpr (Mul r r)
      addNonNegConstraints l
      addNonNegConstraints r
      -- Recursively eliminate
      elimFormula (Gt l' r')
elimFormula (Gt l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Gt l' r')
elimFormula (Le l r)
  | containsSqrtInSum l || containsSqrtInSum r = do
      l' <- elimExpr (Mul l l)
      r' <- elimExpr (Mul r r)
      addNonNegConstraints l
      addNonNegConstraints r
      -- Recursively eliminate
      elimFormula (Le l' r')
elimFormula (Le l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Le l' r')
elimFormula (Lt l r)
  | containsSqrtInSum l || containsSqrtInSum r = do
      l' <- elimExpr (Mul l l)
      r' <- elimExpr (Mul r r)
      addNonNegConstraints l
      addNonNegConstraints r
      -- Recursively eliminate
      elimFormula (Lt l' r')
elimFormula (Lt l r) = do
  l' <- elimExpr l
  r' <- elimExpr r
  return (Lt l' r')
elimFormula (And f1 f2) = And <$> elimFormula f1 <*> elimFormula f2
elimFormula (Or f1 f2) = Or <$> elimFormula f1 <*> elimFormula f2
elimFormula (Not f) = Not <$> elimFormula f
elimFormula (Forall vars f) = Forall vars <$> elimFormula f
elimFormula (Exists vars f) = Exists vars <$> elimFormula f

elimExpr :: Expr -> ElimM Expr
elimExpr (Add a b) = Add <$> elimExpr a <*> elimExpr b
elimExpr (Sub a b) = Sub <$> elimExpr a <*> elimExpr b
-- Smart sqrt multiplication: sqrt(a) * sqrt(b) → sqrt(a*b) OR sqrt(a) * sqrt(a) → a
elimExpr (Mul (Sqrt a) (Sqrt b))
  | a == b = do
      -- sqrt(x) * sqrt(x) = x (with x >= 0)
      a' <- elimExpr a
      addConstraint (Ge a' (Const 0))
      return a'
  | otherwise = do
      -- sqrt(a) * sqrt(b) = sqrt(a*b) (with a,b >= 0)
      a' <- elimExpr a
      b' <- elimExpr b
      addConstraint (Ge a' (Const 0))
      addConstraint (Ge b' (Const 0))
      elimExpr (Sqrt (Mul a' b'))
elimExpr (Mul a b) = Mul <$> elimExpr a <*> elimExpr b
-- Smart sqrt division: a / sqrt(a) → sqrt(a) (for a > 0)
elimExpr (Div a (Sqrt b))
  | a == b = do
      a' <- elimExpr a
      addConstraint (Ge a' (Const 0))
      elimExpr (Sqrt a)
elimExpr (Div a b) = Div <$> elimExpr a <*> elimExpr b
-- Smart sqrt power: (sqrt(a))^2 = a (with a >= 0)
elimExpr (Pow (Sqrt a) 2) = do
  a' <- elimExpr a
  addConstraint (Ge a' (Const 0))
  return a'
elimExpr (Pow e n) = Pow <$> elimExpr e <*> pure n
elimExpr (Sqrt e) = do
  e' <- elimExpr e
  case e of
    Pow t 2 -> do
      t' <- elimExpr t
      addConstraint (Ge t' (Const 0))
      return t'
    _ -> do
      -- Check memo first - reuse variable if we've seen this expression
      (_, memo) <- get
      case Map.lookup e' memo of
        Just varName -> return (Var varName)  -- REUSE existing variable!
        Nothing -> do
          -- Create new variable and memoize it
          v <- freshVar
          let vExpr = Var v
              eqConstraint = Eq (Pow vExpr 2) e'
              geConstraint = Ge vExpr (Const 0)
              radicandConstraint = Ge e' (Const 0)
          addConstraint eqConstraint
          addConstraint geConstraint
          addConstraint radicandConstraint
          -- Add to memo
          modify (\(cs, m) -> (cs, Map.insert e' v m))
          return vExpr
elimExpr (Determinant rows) = Determinant <$> mapM (mapM elimExpr) rows
elimExpr (Circle p c r) = Circle p c <$> elimExpr r
-- Geometric primitives and base cases
elimExpr e@(Dist2 _ _) = return e
elimExpr e@(Collinear _ _ _) = return e
elimExpr e@(Dot _ _ _ _) = return e
elimExpr e@(Midpoint _ _ _) = return e
elimExpr e@(Perpendicular _ _ _ _) = return e
elimExpr e@(Parallel _ _ _ _) = return e
elimExpr e@(AngleEq2D _ _ _ _ _ _) = return e
elimExpr e@(AngleEq2DAbs _ _ _ _ _ _) = return e
elimExpr e@(Var _) = return e
elimExpr e@(Const _) = return e

addConstraint :: Formula -> ElimM ()
addConstraint f = modify (\(constraints, memo) -> (f : constraints, memo))

-- Add non-negativity constraints for all sqrt subexpressions in an expression
addNonNegConstraints :: Expr -> ElimM ()
addNonNegConstraints (Sqrt e) = do
  e' <- elimExpr e
  addConstraint (Ge e' (Const 0))
  addNonNegConstraints e
addNonNegConstraints (Add a b) = addNonNegConstraints a >> addNonNegConstraints b
addNonNegConstraints (Sub a b) = addNonNegConstraints a >> addNonNegConstraints b
addNonNegConstraints (Mul a b) = addNonNegConstraints a >> addNonNegConstraints b
addNonNegConstraints (Div a b) = addNonNegConstraints a >> addNonNegConstraints b
addNonNegConstraints (Pow e _) = addNonNegConstraints e
addNonNegConstraints _ = return ()
