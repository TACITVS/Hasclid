module SqrtElim
  ( eliminateSqrt
  , AlgebraicConstraint(..)
  , RootSign(..)
  , ElimConfig(..)
  , defaultElimConfig
  , SqrtElimResult(..)
  , eliminateSqrtWithConstraints
  , eliminateSqrtWithConfig
  , matchRootPattern
  ) where

import Expr
import Core.Types (simplifyExpr)
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%), numerator, denominator)
import Numeric.Natural (Natural)

-- | Check if a rational is integral (denominator is 1)
isIntegral :: Rational -> Bool
isIntegral r = denominator r == 1

-- | Check if an integer is a perfect square
isPerfectSquare :: Integer -> Bool
isPerfectSquare n
  | n < 0     = False
  | n == 0    = True
  | otherwise = let s = intSqrt n in s * s == n

-- | Integer square root (floor of sqrt)
intSqrt :: Integer -> Integer
intSqrt n
  | n < 0     = 0
  | n == 0    = 0
  | otherwise = go n
  where
    go x =
      let x' = (x + n `div` x) `div` 2
      in if x' >= x then x else go x'

-- | Check if an integer is a perfect nth power
isPerfectNthPower :: Natural -> Integer -> Bool
isPerfectNthPower _ 0 = True
isPerfectNthPower _ 1 = True
isPerfectNthPower k n
  | n < 0     = False
  | otherwise = let r = intNthRoot k n in r ^ k == n

-- | Integer nth root (floor)
intNthRoot :: Natural -> Integer -> Integer
intNthRoot _ 0 = 0
intNthRoot _ 1 = 1
intNthRoot k n
  | n < 0     = 0
  | k == 0    = error "intNthRoot: k must be >= 1"
  | k == 1    = n
  | k == 2    = intSqrt n
  | otherwise = go (n `div` 2 + 1)  -- Newton's method
  where
    k' = fromIntegral k :: Integer
    go x =
      let x' = ((k' - 1) * x + n `div` (x ^ (k' - 1))) `div` k'
      in if x' >= x then x else go x'

-- | Sign information for root expressions
data RootSign = Positive | Negative | Unknown
  deriving (Show, Eq)

-- | Configuration for root elimination
data ElimConfig = ElimConfig
  { maxSquaringDepth    :: Int   -- ^ Maximum depth for recursive squaring (default: 3)
  , enableInequiSquaring :: Bool  -- ^ Whether to automatically square inequalities (default: True)
  } deriving (Show, Eq)

-- | Default configuration for root elimination
defaultElimConfig :: ElimConfig
defaultElimConfig = ElimConfig
  { maxSquaringDepth = 3
  , enableInequiSquaring = True
  }

-- | Algebraic constraint representing v^n = e
-- Used for enhanced SOS decomposition that understands nth-root relationships
-- Example: For sqrt(ab), we have v^2 = ab with coefficient 1
-- Example: For 2*sqrt(ab), we track coefficient = 2
data AlgebraicConstraint = AlgebraicConstraint
  { acVar         :: String      -- ^ The auxiliary variable (e.g., "zz_root_aux1")
  , acIndex       :: Natural     -- ^ The root index (2 for sqrt, 3 for cbrt, etc.)
  , acRadicand    :: Expr        -- ^ The expression under the root (e.g., a*b for sqrt(a*b))
  , acCoefficient :: Rational    -- ^ Coefficient outside the root (2 for 2*sqrt(a*b))
  , acSign        :: RootSign    -- ^ Sign information (Positive for even roots with positive radicand)
  } deriving (Show, Eq)

-- | Legacy accessor for backwards compatibility: v^n = radicand
acSquaredTo :: AlgebraicConstraint -> Expr
acSquaredTo = acRadicand

-- | Result of sqrt elimination with structured constraints
data SqrtElimResult = SqrtElimResult
  { serTheory :: Theory                    -- ^ Transformed theory (sqrt-free)
  , serGoal :: Formula                     -- ^ Transformed goal (sqrt-free)
  , serVarDefs :: Map.Map String Expr      -- ^ Variable definitions (v -> sqrt(e))
  , serAlgebraicConstraints :: [AlgebraicConstraint]  -- ^ Structured v^2 = e constraints
  } deriving (Show, Eq)

-- | Eliminate sqrt and return structured algebraic constraints
-- This is the enhanced version that supports better SOS integration
eliminateSqrtWithConstraints :: Theory -> Formula -> SqrtElimResult
eliminateSqrtWithConstraints = eliminateSqrtWithConfig defaultElimConfig

-- | Eliminate roots with configurable behavior
eliminateSqrtWithConfig :: ElimConfig -> Theory -> Formula -> SqrtElimResult
eliminateSqrtWithConfig _config theory goal =
  let ((theory', goal'), (extras, memo)) = runState (do
        t' <- mapM elimFormula theory
        g' <- elimFormula goal
        return (t', g')) ([], Map.empty)
      -- Build var definitions: VarName -> Sqrt(Expr)
      varDefs = Map.fromList [ (v, Sqrt e) | (e, v) <- Map.toList memo ]
      -- Build algebraic constraints with full information
      algConstraints = [ AlgebraicConstraint
                          { acVar = v
                          , acIndex = 2  -- Sqrt is index 2
                          , acRadicand = e
                          , acCoefficient = 1  -- No coefficient tracked yet in elimination
                          , acSign = Positive  -- Sqrt is always non-negative
                          }
                       | (e, v) <- Map.toList memo ]
  in SqrtElimResult (extras ++ theory') goal' varDefs algConstraints

-- | Eliminate sqrt by introducing auxiliary variables.
-- Each (sqrt e) becomes a fresh variable v with constraints:
--   v^2 = e    and    v >= 0    and    e >= 0
-- MEMOIZATION: Reuses same auxiliary variable for identical sqrt expressions
-- Returns (transformedTheory, transformedGoal, varDefinitions) both sqrt-free.
eliminateSqrt :: Theory -> Formula -> (Theory, Formula, Map.Map String Expr)
eliminateSqrt theory goal =
  let ((theory', goal'), (extras, memo)) = runState (do
        t' <- mapM elimFormula theory
        g' <- elimFormula goal
        return (t', g')) ([], Map.empty)
      -- Reverse the memo to get VarName -> SqrtExpr
      varDefs = Map.fromList [ (v, Sqrt e) | (e, v) <- Map.toList memo ]
  in (extras ++ theory', goal', varDefs)

type SqrtMemo = Map.Map Expr String
type ElimM = State ([Formula], SqrtMemo)

freshVar :: ElimM String
freshVar = do
  (_, memo) <- get
  let idx = Map.size memo + 1
  return ("zz_sqrt_aux" ++ show idx)

-- | Match coefficient*root pattern
-- Returns (coefficient, root index, radicand) if the expression matches c*root_n(e)
-- Examples:
--   Sqrt a            -> Just (1, 2, a)
--   NthRoot 3 a       -> Just (1, 3, a)
--   Mul (Const 2) (Sqrt a) -> Just (2, 2, a)
--   Div (Sqrt a) (Const 2) -> Just (1/2, 2, a)
matchRootPattern :: Expr -> Maybe (Rational, Natural, Expr)
matchRootPattern expr = case expr of
  -- Direct roots
  Sqrt e                    -> Just (1, 2, e)
  NthRoot n e               -> Just (1, n, e)

  -- Coefficient * root patterns
  Mul (Const c) (Sqrt e)    -> Just (c, 2, e)
  Mul (Sqrt e) (Const c)    -> Just (c, 2, e)
  Mul (Const c) (NthRoot n e) -> Just (c, n, e)
  Mul (NthRoot n e) (Const c) -> Just (c, n, e)

  -- Division patterns: root / c = (1/c) * root
  Div (Sqrt e) (Const c) | c /= 0 -> Just (1/c, 2, e)
  Div (NthRoot n e) (Const c) | c /= 0 -> Just (1/c, n, e)

  -- Nested: c * (d * root) = cd * root
  Mul (Const c) inner -> case matchRootPattern inner of
    Just (c', n, e) -> Just (c * c', n, e)
    Nothing -> Nothing
  Mul inner (Const c) -> case matchRootPattern inner of
    Just (c', n, e) -> Just (c * c', n, e)
    Nothing -> Nothing

  _ -> Nothing

-- Helper: Check if expression contains any root (sqrt or nth-root)
containsSqrt :: Expr -> Bool
containsSqrt (Sqrt _) = True
containsSqrt (NthRoot _ _) = True
containsSqrt (Add a b) = containsSqrt a || containsSqrt b
containsSqrt (Sub a b) = containsSqrt a || containsSqrt b
containsSqrt (Mul a b) = containsSqrt a || containsSqrt b
containsSqrt (Div a b) = containsSqrt a || containsSqrt b
containsSqrt (Pow e _) = containsSqrt e
containsSqrt _ = False

-- Helper: Check if root appears in a SUM (Add/Sub) context
-- This is the case that smart simplifications can't handle
containsSqrtInSum :: Expr -> Bool
containsSqrtInSum (Add a b) = containsSqrt a || containsSqrt b
containsSqrtInSum (Sub a b) = containsSqrt a || containsSqrt b
containsSqrtInSum (Mul a b) = containsSqrtInSum a || containsSqrtInSum b
containsSqrtInSum (Div a b) = containsSqrtInSum a || containsSqrtInSum b
containsSqrtInSum (Pow e _) = containsSqrtInSum e
containsSqrtInSum _ = False

-- | Check if an expression is provably non-negative (for safe squaring)
-- Conservative check: only returns True when definitely non-negative
isProvablyNonNegative :: Expr -> Bool
isProvablyNonNegative expr = case expr of
  Const r -> r >= 0
  Sqrt _ -> True  -- sqrt is always non-negative
  NthRoot n _ | even n -> True  -- even roots are non-negative
  Pow _ 2 -> True  -- x^2 is always non-negative
  Pow e n | even n -> isProvablyNonNegative e || True  -- e^(2k) >= 0
  Mul a b | isProvablyNonNegative a && isProvablyNonNegative b -> True
  Add a b | isProvablyNonNegative a && isProvablyNonNegative b -> True
  -- Match coefficient * root patterns
  _ -> case matchRootPattern expr of
         Just (c, n, _) | c > 0 && even n -> True
         _ -> False

-- | Try to square an inequality if both sides are provably non-negative
-- Returns the squared formula if squaring is beneficial, Nothing otherwise
trySquareInequality :: Int -> Int -> Formula -> Maybe Formula
trySquareInequality depth maxDepth formula
  | depth >= maxDepth = Nothing
  | otherwise = case formula of
      Ge lhs rhs | bothNonNeg lhs rhs && hasRootOrCoeff lhs rhs ->
        Just $ Ge (Mul lhs lhs) (Mul rhs rhs)
      Le lhs rhs | bothNonNeg lhs rhs && hasRootOrCoeff lhs rhs ->
        Just $ Le (Mul lhs lhs) (Mul rhs rhs)
      Gt lhs rhs | bothNonNeg lhs rhs && hasRootOrCoeff lhs rhs ->
        Just $ Gt (Mul lhs lhs) (Mul rhs rhs)
      Lt lhs rhs | bothNonNeg lhs rhs && hasRootOrCoeff lhs rhs ->
        Just $ Lt (Mul lhs lhs) (Mul rhs rhs)
      _ -> Nothing
  where
    bothNonNeg a b = isProvablyNonNegative a && isProvablyNonNegative b
    -- Only square if there's a coefficient*root pattern that would benefit
    hasRootOrCoeff a b = hasCoeffRoot a || hasCoeffRoot b
    hasCoeffRoot e = case matchRootPattern e of
                       Just (c, _, _) | c /= 1 -> True  -- Has a coefficient
                       _ -> containsSqrt e  -- Or contains sqrt in a sum

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

-- Eq patterns for coefficient * sqrt: c * sqrt(a) = b  ⟺  c^2*a = b^2
elimFormula (Eq (Mul (Const c) (Sqrt a)) b) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Eq (Mul (Const (c * c)) a') (Pow b' 2))
elimFormula (Eq (Mul (Sqrt a) (Const c)) b) | c > 0 =
  elimFormula (Eq (Mul (Const c) (Sqrt a)) b)
elimFormula (Eq b (Mul (Const c) (Sqrt a))) | c > 0 =
  elimFormula (Eq (Mul (Const c) (Sqrt a)) b)
elimFormula (Eq b (Mul (Sqrt a) (Const c))) | c > 0 =
  elimFormula (Eq (Mul (Const c) (Sqrt a)) b)

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

-- Coefficient * sqrt patterns: b >= c * sqrt(a)  ⟺  b^2 >= c^2*a
-- These handle cases like "a+b >= 2*sqrt(ab)" that don't match bare Sqrt patterns
elimFormula (Ge b (Mul (Const c) (Sqrt a))) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Ge (Pow b' 2) (Mul (Const (c * c)) a'))
elimFormula (Ge b (Mul (Sqrt a) (Const c))) | c > 0 =
  elimFormula (Ge b (Mul (Const c) (Sqrt a)))
elimFormula (Ge (Mul (Const c) (Sqrt a)) b) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Ge (Mul (Const (c * c)) a') (Pow b' 2))
elimFormula (Ge (Mul (Sqrt a) (Const c)) b) | c > 0 =
  elimFormula (Ge (Mul (Const c) (Sqrt a)) b)

elimFormula (Gt b (Mul (Const c) (Sqrt a))) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Gt (Pow b' 2) (Mul (Const (c * c)) a'))
elimFormula (Gt b (Mul (Sqrt a) (Const c))) | c > 0 =
  elimFormula (Gt b (Mul (Const c) (Sqrt a)))
elimFormula (Gt (Mul (Const c) (Sqrt a)) b) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Gt (Mul (Const (c * c)) a') (Pow b' 2))
elimFormula (Gt (Mul (Sqrt a) (Const c)) b) | c > 0 =
  elimFormula (Gt (Mul (Const c) (Sqrt a)) b)

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

-- Le coefficient * sqrt patterns
elimFormula (Le b (Mul (Const c) (Sqrt a))) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Le (Pow b' 2) (Mul (Const (c * c)) a'))
elimFormula (Le b (Mul (Sqrt a) (Const c))) | c > 0 =
  elimFormula (Le b (Mul (Const c) (Sqrt a)))
elimFormula (Le (Mul (Const c) (Sqrt a)) b) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Le (Mul (Const (c * c)) a') (Pow b' 2))
elimFormula (Le (Mul (Sqrt a) (Const c)) b) | c > 0 =
  elimFormula (Le (Mul (Const c) (Sqrt a)) b)

-- Lt coefficient * sqrt patterns
elimFormula (Lt b (Mul (Const c) (Sqrt a))) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Lt (Pow b' 2) (Mul (Const (c * c)) a'))
elimFormula (Lt b (Mul (Sqrt a) (Const c))) | c > 0 =
  elimFormula (Lt b (Mul (Const c) (Sqrt a)))
elimFormula (Lt (Mul (Const c) (Sqrt a)) b) | c > 0 = do
  a' <- elimExpr a
  b' <- elimExpr b
  addConstraint (Ge a' (Const 0))
  addConstraint (Ge b' (Const 0))
  return (Lt (Mul (Const (c * c)) a') (Pow b' 2))
elimFormula (Lt (Mul (Sqrt a) (Const c)) b) | c > 0 =
  elimFormula (Lt (Mul (Const c) (Sqrt a)) b)

-- Automatic squaring for equations containing sqrt in SUMS
-- This handles cases like: sqrt(a) = sqrt(b) + sqrt(c)
-- By squaring: a = (sqrt(b) + sqrt(c))^2
-- =============================================================================
-- IMPLICIT SQRT RECOGNITION: v² = expr means v = sqrt(expr)
-- =============================================================================
-- This allows SqrtElim to recognize patterns like "cx² = c2x" as meaning
-- "cx = sqrt(c2x)" and track them as algebraic constraints.
-- This is crucial for trigonometric formulations where cos²(x) = 1/(1+tan²(x)).
elimFormula (Eq (Pow (Var v) 2) rhs) = do
  rhs' <- elimExpr rhs
  let rhs'' = simplifyExpr rhs'
  -- Add positivity constraint: v >= 0 (since v = sqrt(rhs))
  addConstraint (Ge (Var v) (Const 0))
  -- Add to memo so it's tracked as algebraic constraint: rhs'' -> v means v = sqrt(rhs'')
  modify (\(cs, m) -> (cs, Map.insert rhs'' v m))
  -- Return the polynomial constraint
  return $ Eq (Pow (Var v) 2) rhs''

-- Reversed form: expr = v²
elimFormula (Eq lhs (Pow (Var v) 2)) = elimFormula (Eq (Pow (Var v) 2) lhs)

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
-- Re-enabled with depth limit to prevent infinite loops
-- If both sides are provably non-negative and squaring is beneficial, square them
elimFormula f@(Ge l r) = case trySquareInequality 0 3 f of
  Just squared -> elimFormula squared  -- Recursively handle squared version
  Nothing -> do
    l' <- elimExpr l
    r' <- elimExpr r
    return (Ge l' r')
elimFormula f@(Gt l r) = case trySquareInequality 0 3 f of
  Just squared -> elimFormula squared
  Nothing -> do
    l' <- elimExpr l
    r' <- elimExpr r
    return (Gt l' r')
elimFormula f@(Le l r) = case trySquareInequality 0 3 f of
  Just squared -> elimFormula squared
  Nothing -> do
    l' <- elimExpr l
    r' <- elimExpr r
    return (Le l' r')
elimFormula f@(Lt l r) = case trySquareInequality 0 3 f of
  Just squared -> elimFormula squared
  Nothing -> do
    l' <- elimExpr l
    r' <- elimExpr r
    return (Lt l' r')
elimFormula (And f1 f2) = And <$> elimFormula f1 <*> elimFormula f2
elimFormula (Or f1 f2) = Or <$> elimFormula f1 <*> elimFormula f2
elimFormula (Not f) = Not <$> elimFormula f
elimFormula (Forall vars f) = Forall vars <$> elimFormula f
elimFormula (Exists vars f) = Exists vars <$> elimFormula f

elimExpr :: Expr -> ElimM Expr
-- NOTE: We used to distribute Sqrt over Mul/Div here, but this interferes with
-- coefficient absorption (2*sqrt(ab) -> sqrt(4ab)). Instead, we now rely on
-- the memoization and smart simplifications to handle common subexpressions.

elimExpr (Add a b) = Add <$> elimExpr a <*> elimExpr b
elimExpr (Sub a b) = Sub <$> elimExpr a <*> elimExpr b
-- Smart sqrt multiplication: sqrt(a) * sqrt(b) → sqrt(a*b) OR sqrt(a) * sqrt(a) → a
elimExpr (Mul (Sqrt a) (Sqrt b))
  | a == b = do
      -- sqrt(x) * sqrt(x) = x (with x >= 0)
      a' <- elimExpr a
      addConstraint (Ge a' (Const 0))
      return a'
  | otherwise = Mul <$> elimExpr (Sqrt a) <*> elimExpr (Sqrt b)

-- Coefficient absorption: c * sqrt(a) -> sqrt(c^2 * a) for positive c
-- This normalizes 2*sqrt(ab) to sqrt(4ab) for consistent handling
elimExpr (Mul (Const c) (Sqrt a))
  | c > 0 = elimExpr (Sqrt (Mul (Const (c * c)) a))
  | c < 0 = do
      -- -c * sqrt(a) = -(c * sqrt(a))
      inner <- elimExpr (Sqrt (Mul (Const (c * c)) a))
      return (Mul (Const (-1)) inner)
  | otherwise = return (Const 0)  -- c = 0
elimExpr (Mul (Sqrt a) (Const c)) = elimExpr (Mul (Const c) (Sqrt a))

-- Handle IntConst the same way (parser may produce IntConst for integers)
elimExpr (Mul (IntConst n) (Sqrt a))
  | n > 0 = elimExpr (Sqrt (Mul (Const (fromIntegral n * fromIntegral n)) a))
  | n < 0 = do
      inner <- elimExpr (Sqrt (Mul (Const (fromIntegral n * fromIntegral n)) a))
      return (Mul (Const (-1)) inner)
  | otherwise = return (Const 0)
elimExpr (Mul (Sqrt a) (IntConst n)) = elimExpr (Mul (IntConst n) (Sqrt a))

-- Similarly for NthRoot: c * root_n(a) -> root_n(c^n * a)
elimExpr (Mul (Const c) (NthRoot n a))
  | c > 0 = elimExpr (NthRoot n (Mul (Const (c ^ n)) a))
  | c < 0 && odd n = do
      -- For odd n, (-c) * root_n(a) = root_n((-c)^n * a)
      elimExpr (NthRoot n (Mul (Const (c ^ n)) a))
  | c < 0 && even n = do
      -- For even n, (-c) * root_n(a) = -(c * root_n(a))
      inner <- elimExpr (NthRoot n (Mul (Const ((-c) ^ n)) a))
      return (Mul (Const (-1)) inner)
  | otherwise = return (Const 0)
elimExpr (Mul (NthRoot n a) (Const c)) = elimExpr (Mul (Const c) (NthRoot n a))

-- IntConst version for NthRoot
elimExpr (Mul (IntConst m) (NthRoot n a))
  | m > 0 = elimExpr (NthRoot n (Mul (Const (fromIntegral m ^ n)) a))
  | m < 0 && odd n = elimExpr (NthRoot n (Mul (Const (fromIntegral m ^ n)) a))
  | m < 0 && even n = do
      inner <- elimExpr (NthRoot n (Mul (Const (fromIntegral (abs m) ^ n)) a))
      return (Mul (Const (-1)) inner)
  | otherwise = return (Const 0)
elimExpr (Mul (NthRoot n a) (IntConst m)) = elimExpr (Mul (IntConst m) (NthRoot n a))

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
  -- CRITICAL: Constant fold before perfect square check!
  -- This converts (0-3)^2 + (0-0)^2 -> 9 so perfect square detection works
  let e'' = simplifyExpr e'
  case e'' of
    -- Simplify sqrt(x^2) -> x (since lengths/distances are positive)
    Pow t 2 -> do
      addConstraint (Ge t (Const 0))
      return t
    -- Simplify sqrt of perfect square constants: sqrt(4) -> 2, sqrt(9) -> 3
    Const n | n >= 0, isIntegral n, isPerfectSquare (numerator n), denominator n == 1 ->
      return $ Const (toRational (intSqrt (numerator n)))
    -- Simplify sqrt of perfect square rationals: sqrt(4/9) -> 2/3
    Const n | n >= 0, isPerfectSquare (numerator n), isPerfectSquare (denominator n) ->
      return $ Const (toRational (intSqrt (numerator n)) / toRational (intSqrt (denominator n)))
    _ -> do
      -- Check memo first - reuse variable if we've seen this expression
      (_, memo) <- get
      case Map.lookup e'' memo of
        Just varName -> return (Var varName)  -- REUSE existing variable!
        Nothing -> do
          -- Create new variable and memoize it
          v <- freshVar
          let vExpr = Var v
              eqConstraint = Eq (Pow vExpr 2) e''
              geConstraint = Ge vExpr (Const 0)
              radicandConstraint = Ge e'' (Const 0)
          addConstraint eqConstraint
          addConstraint geConstraint
          addConstraint radicandConstraint
          -- Add to memo (use simplified expression as key)
          modify (\(cs, m) -> (cs, Map.insert e'' v m))
          return vExpr

-- NthRoot elimination: distribute over Mul/Div
elimExpr (NthRoot n (Mul a b)) = elimExpr (Mul (NthRoot n a) (NthRoot n b))
elimExpr (NthRoot n (Div a b)) = elimExpr (Div (NthRoot n a) (NthRoot n b))

-- NthRoot power simplification: (NthRoot n a)^n = a
elimExpr (Pow (NthRoot n a) m)
  | fromIntegral m == n = do
      a' <- elimExpr a
      -- For even n, we need a >= 0
      when (even n) $ addConstraint (Ge a' (Const 0))
      return a'

-- NthRoot of same roots: NthRoot n a * NthRoot n a * ... (n times) = a
elimExpr (Mul (NthRoot n a) (NthRoot m b))
  | n == m && a == b = do
      -- Two nth-roots of same thing: NthRoot n a * NthRoot n a = a^(2/n)
      -- For n=2 (sqrt), this is just a
      if n == 2
        then do
          a' <- elimExpr a
          addConstraint (Ge a' (Const 0))
          return a'
        else Mul <$> elimExpr (NthRoot n a) <*> elimExpr (NthRoot m b)
  | otherwise = Mul <$> elimExpr (NthRoot n a) <*> elimExpr (NthRoot m b)

-- General NthRoot elimination
elimExpr (NthRoot n e) = do
  e' <- elimExpr e
  -- CRITICAL: Constant fold before perfect power check!
  let e'' = simplifyExpr e'
  case e'' of
    -- Simplify NthRoot n (x^n) -> x (for positive x when n is even)
    Pow t m | fromIntegral m == n -> do
      when (even n) $ addConstraint (Ge t (Const 0))
      return t
    -- Simplify NthRoot n (Const c) for perfect nth powers
    Const c | c >= 0, isIntegral c, isPerfectNthPower n (numerator c) ->
      return $ Const (toRational (intNthRoot n (numerator c)))
    _ -> do
      -- Check memo first - reuse variable if we've seen this expression
      (_, memo) <- get
      -- Use a tagged key to differentiate different root indices
      let memoKey = NthRoot n e''  -- Use the NthRoot with simplified expr as key
      case Map.lookup memoKey memo of
        Just varName -> return (Var varName)  -- REUSE existing variable!
        Nothing -> do
          -- Create new variable and memoize it
          v <- freshVar
          let vExpr = Var v
              -- v^n = e''
              eqConstraint = Eq (Pow vExpr (fromIntegral n)) e''
              -- For even n, v >= 0
              geConstraint = Ge vExpr (Const 0)
              -- For even n, e'' >= 0
              radicandConstraint = Ge e'' (Const 0)
          addConstraint eqConstraint
          when (even n) $ do
            addConstraint geConstraint
            addConstraint radicandConstraint
          -- Add to memo with the NthRoot as key
          modify (\(cs, m) -> (cs, Map.insert memoKey v m))
          return vExpr

elimExpr (Atan e) = Atan <$> elimExpr e
elimExpr (Sin e) = Sin <$> elimExpr e
elimExpr (Cos e) = Cos <$> elimExpr e
elimExpr (Tan e) = Tan <$> elimExpr e
elimExpr (Asin e) = Asin <$> elimExpr e
elimExpr (Acos e) = Acos <$> elimExpr e

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
_addNonNegConstraints :: Expr -> ElimM ()
_addNonNegConstraints (Sqrt e) = do
  e' <- elimExpr e
  addConstraint (Ge e' (Const 0))
  _addNonNegConstraints e
_addNonNegConstraints (Add a b) = _addNonNegConstraints a >> _addNonNegConstraints b
_addNonNegConstraints (Sub a b) = _addNonNegConstraints a >> _addNonNegConstraints b
_addNonNegConstraints (Mul a b) = _addNonNegConstraints a >> _addNonNegConstraints b
_addNonNegConstraints (Div a b) = _addNonNegConstraints a >> _addNonNegConstraints b
_addNonNegConstraints (Pow e _) = _addNonNegConstraints e
_addNonNegConstraints _ = return ()
