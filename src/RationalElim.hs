module RationalElim
  ( eliminateRational
  ) where

import Expr
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)

-- | Eliminate division by introducing polynomial equivalents.
-- Returns (transformedTheory, transformedGoal, varDefinitions)
eliminateRational :: Theory -> Formula -> (Theory, Formula, Map.Map String Expr)
eliminateRational theory goal =
  let theory' = map simplifyRationalArithmetic theory
      goal' = simplifyRationalArithmetic goal
      ((theory'', goal''), (extras, _, varMemo, _)) = runState (do
        t' <- mapM elimFormula theory'
        g' <- elimFormula goal'
        return (t', g')) ([], Map.empty, Map.empty, 0)
  in (extras ++ theory'', goal'', varMemo)

type DivMemo = Map.Map Expr Formula
type VarMemo = Map.Map String Expr
type ElimM = State ([Formula], DivMemo, VarMemo, Int)

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

simpExprArith :: Expr -> Expr
simpExprArith (Div (Const a) (Const b)) | b /= 0 = Const (a / b) | otherwise = Div (Const a) (Const b)
simpExprArith (Div (Div a b) c) = simpExprArith (Div a (Mul b c))
simpExprArith (Div a (Div b c)) = simpExprArith (Div (Mul a c) b)
simpExprArith (Div (Mul a c1) (Mul b c2)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul c1 a) (Mul c2 b)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul c1 a) (Mul b c2)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Div (Mul a c1) (Mul c2 b)) | c1 == c2 = simpExprArith (Div a b)
simpExprArith (Add (Div a b) (Div c d)) = simpExprArith (Div (Add (Mul a d) (Mul b c)) (Mul b d))
simpExprArith (Add (Div a b) c) = let sc = simpExprArith c in simpExprArith (Div (Add a (Mul sc b)) b)
simpExprArith (Add c (Div a b)) = let sc = simpExprArith c in simpExprArith (Div (Add (Mul sc b) a) b)
simpExprArith (Sub (Div a b) (Div c d)) = simpExprArith (Div (Sub (Mul a d) (Mul b c)) (Mul b d))
simpExprArith (Sub (Div a b) c) = let sc = simpExprArith c in simpExprArith (Div (Sub a (Mul sc b)) b)
simpExprArith (Sub c (Div a b)) = let sc = simpExprArith c in simpExprArith (Div (Sub (Mul sc b) a) b)
simpExprArith (Mul (Div a b) (Div c d)) = simpExprArith (Div (Mul a c) (Mul b d))
simpExprArith (Mul a (Div b c)) = simpExprArith (Div (Mul a b) c)
simpExprArith (Mul (Div a b) c) = simpExprArith (Div (Mul a c) b)
simpExprArith (Add a b) = let sa = simpExprArith a; sb = simpExprArith b in case (sa, sb) of (Const 0, x) -> x; (x, Const 0) -> x; _ -> Add sa sb
simpExprArith (Sub a b) = let sa = simpExprArith a; sb = simpExprArith b in case (sa, sb) of (x, Const 0) -> x; _ | sa == sb -> Const 0; _ -> Sub sa sb
simpExprArith (Mul a b) = let sa = simpExprArith a; sb = simpExprArith b in case (sa, sb) of (Const 0, _) -> Const 0; (_, Const 0) -> Const 0; (Const 1, x) -> x; (x, Const 1) -> x; _ -> Mul sa sb
simpExprArith (Div a b) = let sa = simpExprArith a; sb = simpExprArith b in case (sa, sb) of (Const 0, _) -> Const 0; (x, Const 1) -> x; _ | sa == sb -> Const 1; _ -> Div sa sb
simpExprArith (Pow e n) = Pow (simpExprArith e) n
simpExprArith (Sqrt e) = Sqrt (simpExprArith e)
simpExprArith (Determinant rows) = Determinant (map (map simpExprArith) rows)
simpExprArith (Circle p c r) = Circle p c (simpExprArith r)
simpExprArith e = e

elimFormula (Eq (Div f g) (Const 0)) = do { f' <- elimExpr' f; g' <- elimExpr' g; addDenominatorNonzero g'; return (Eq f' (Const 0)) }
elimFormula (Eq (Const 0) (Div f g)) = elimFormula (Eq (Div f g) (Const 0))
elimFormula (Eq (Div f1 g1) (Div f2 g2)) = do { f1' <- elimExpr' f1; g1' <- elimExpr' g1; f2' <- elimExpr' f2; g2' <- elimExpr' g2; addDenominatorNonzero g1'; addDenominatorNonzero g2'; return (Eq (Mul f1' g2') (Mul f2' g1')) }
elimFormula (Eq (Div f g) c) | not (containsDivExpr c) = do { f' <- elimExpr' f; g' <- elimExpr' g; c' <- elimExpr' c; addDenominatorNonzero g'; return (Eq f' (Mul c' g')) }
elimFormula (Eq c (Div f g)) | not (containsDivExpr c) = elimFormula (Eq (Div f g) c)
elimFormula (Ge (Div f g) k) | k /= Const 0 = do { f' <- elimExpr' f; g' <- elimExpr' g; k' <- elimExpr' k; elimFormula (Ge (Div (Sub f' (Mul k' g')) g') (Const 0)) }
elimFormula (Ge k (Div f g)) | k /= Const 0 = do { f' <- elimExpr' f; g' <- elimExpr' g; k' <- elimExpr' k; elimFormula (Ge (Div (Sub (Mul k' g') f') g') (Const 0)) }
elimFormula (Gt (Div f g) k) | k /= Const 0 = do { f' <- elimExpr' f; g' <- elimExpr' g; k' <- elimExpr' k; elimFormula (Gt (Div (Sub f' (Mul k' g')) g') (Const 0)) }
elimFormula (Gt k (Div f g)) | k /= Const 0 = do { f' <- elimExpr' f; g' <- elimExpr' g; k' <- elimExpr' k; elimFormula (Gt (Div (Sub (Mul k' g') f') g') (Const 0)) }
elimFormula (Ge (Div f g) (Const 0)) = do { f' <- elimExpr' f; g' <- elimExpr' g; if isGeomPositive g' then return $ And (Ge f' (Const 0)) (Gt g' (Const 0)) else return $ Or (And (Ge f' (Const 0)) (Gt g' (Const 0))) (And (Le f' (Const 0)) (Lt g' (Const 0))) }
elimFormula (Gt (Div f g) (Const 0)) = do { f' <- elimExpr' f; g' <- elimExpr' g; if isGeomPositive g' then return $ And (Gt f' (Const 0)) (Gt g' (Const 0)) else return $ Or (And (Gt f' (Const 0)) (Gt g' (Const 0))) (And (Lt f' (Const 0)) (Lt g' (Const 0))) }
elimFormula (Le (Div f g) (Const 0)) = do { f' <- elimExpr' f; g' <- elimExpr' g; return $ Or (And (Le f' (Const 0)) (Gt g' (Const 0))) (And (Ge f' (Const 0)) (Lt g' (Const 0))) }
elimFormula (Lt (Div f g) (Const 0)) = do { f' <- elimExpr' f; g' <- elimExpr' g; return $ Or (And (Lt f' (Const 0)) (Gt g' (Const 0))) (And (Gt f' (Const 0)) (Lt g' (Const 0))) }
elimFormula (Eq l r) = Eq <$> elimExpr' l <*> elimExpr' r
elimFormula (Ge l r) = do { let sa = simpExprArith l; sb = simpExprArith r in case simpExprArith (Sub sa sb) of Div f g -> elimFormula (Ge (Div f g) (Const 0)); _ -> Ge <$> elimExpr' sa <*> elimExpr' sb }
elimFormula (Gt l r) = do { let sa = simpExprArith l; sb = simpExprArith r in case simpExprArith (Sub sa sb) of Div f g -> elimFormula (Gt (Div f g) (Const 0)); _ -> Gt <$> elimExpr' sa <*> elimExpr' sb }
elimFormula (Le l r) = elimFormula (Ge r l)
elimFormula (Lt l r) = elimFormula (Gt r l)
elimFormula (And f1 f2) = And <$> elimFormula f1 <*> elimFormula f2
elimFormula (Or f1 f2) = Or <$> elimFormula f1 <*> elimFormula f2
elimFormula (Not f) = Not <$> elimFormula f
elimFormula (Forall vars f) = Forall vars <$> elimFormula f
elimFormula (Exists vars f) = Exists vars <$> elimFormula f

elimExpr' (Add a b) = Add <$> elimExpr' a <*> elimExpr' b
elimExpr' (Sub a b) = Sub <$> elimExpr' a <*> elimExpr' b
elimExpr' (Mul a b) = Mul <$> elimExpr' a <*> elimExpr' b
elimExpr' (Div a b) = do
  a' <- elimExpr' a; b' <- elimExpr' b
  if a' == b' then addDenominatorNonzero a' >> return (Const 1)
  else if b' == Const 1 then return a'
  else do { v <- freshDivVar; addDenominatorNonzero b'; addConstraint (Eq (Mul (Var v) b') a'); addVarDef v (Div a' b'); return (Var v) }
elimExpr' (Pow e n) = Pow <$> elimExpr' e <*> pure n
elimExpr' (Sqrt e) = Sqrt <$> elimExpr' e
elimExpr' (Sum i lo hi body) = Sum i <$> elimExpr' lo <*> elimExpr' hi <*> elimExpr' body
elimExpr' (Determinant rows) = Determinant <$> mapM (mapM elimExpr') rows
elimExpr' (Circle p c r) = Circle p c <$> elimExpr' r
elimExpr' e = return e

freshDivVar = do
  (cs, dm, vm, n) <- get
  let next = n + 1
  put (cs, dm, vm, next)
  return ("zz_div_aux" ++ show next)
addConstraint f = modify (\(cs, dm, vm, n) -> (f : cs, dm, vm, n))
addVarDef v e = modify (\(cs, dm, vm, n) -> (cs, dm, Map.insert v e vm, n))
addDenominatorNonzero den = do
  (cs, dm, vm, n) <- get
  case Map.lookup den dm of
    Just _ -> return ()
    Nothing -> let constraint = if isGeomPositive den then Gt den (Const 0) else Or (Gt den (Const 0)) (Lt den (Const 0)) in put (constraint : cs, Map.insert den constraint dm, vm, n)

isGeomPositive (Dist2 _ _) = True
isGeomPositive (Pow _ 2) = True
isGeomPositive (Sqrt _) = True
isGeomPositive (Mul a b) = isGeomPositive a && isGeomPositive b
isGeomPositive (Add a b) = isGeomPositive a && isGeomPositive b
isGeomPositive (Var v) = v `elem` ["a", "b", "c", "R1", "R2", "R3", "ha", "hb", "hc", "a2", "b2", "c2", "R1s", "R2s", "R3s"] || "ba_" `isPrefixOf` v || "zz_sqrt_aux" `isPrefixOf` v
isGeomPositive _ = False