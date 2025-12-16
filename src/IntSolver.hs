{-# LANGUAGE DeriveGeneric #-}

module IntSolver
  ( IntSolveOptions(..)
  , IntSolveOutcome(..)
  , defaultIntSolveOptions
  , reasonOutcome
  , intSolve
  , intSat
  , intEvalFormula
  , intEvalExpr
  , evaluateIfSingleton
  , intLin
  , intLinDiff
  , gcdCoeffs
  , buildIntSubMap
  , intVarsFormula
  , intVarsExpr
  , intIntervalSolveT
  ) where

import Expr
import Timeout
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.List (nub, minimumBy, delete, sort)
import Data.Maybe (isJust, fromMaybe, catMaybes, maybeToList)
import Data.Ratio (numerator, denominator)
import Control.Monad (foldM)

-- =============================================
-- Integer Evaluation (minimal, ground/constant only)
-- =============================================

-- Try to solve integer goals by evaluation/substitution of integer constants.
-- Returns Just True/False if decided, Nothing otherwise.
data IntSolveOptions = IntSolveOptions
  { allowBruteForce :: Bool
  } deriving (Show, Eq)

defaultIntSolveOptions :: IntSolveOptions
defaultIntSolveOptions = IntSolveOptions { allowBruteForce = False }

data IntSolveOutcome = IntSolveOutcome
  { intResult :: Maybe Bool
  , intUsedBrute :: Bool
  , intBruteCandidate :: Bool
  , intUsedBranch :: Bool
  , intUsedLP :: Bool
  , intLPInfeasible :: Bool
  } deriving (Show, Eq)

reasonOutcome :: IntSolveOutcome -> Bool -> String
reasonOutcome outcome truth =
  let verdict = if truth then "proved" else "refuted"
  in if intUsedBrute outcome
       then "Integer goal " ++ verdict ++ " by bounded brute-force search"
       else if intUsedBranch outcome
              then "Integer goal " ++ verdict ++ " by bounded branch search"
             else if intUsedLP outcome
                    then "Integer goal " ++ verdict ++ " after LP-bound tightening"
              else "Integer goal " ++ verdict ++ " by integer evaluator"

-- Uses the interval solver with a tautological goal to detect contradictions.
intSat :: IntSolveOptions -> Theory -> IntSolveOutcome
intSat opts th =
  let taut = Eq (IntConst 0) (IntConst 0)
      quickUnsat = any constraintUnsatQuick th
  in if quickUnsat
       then IntSolveOutcome (Just False) False False False False False
       else if null th
         then IntSolveOutcome Nothing False False False False False
         else intIntervalSolve opts True th taut

isLinearFormula :: Formula -> Bool
isLinearFormula (Eq l r) = isJust (intLinDiff l r)
isLinearFormula (Ge l r) = isJust (intLinDiff l r)
isLinearFormula (Gt l r) = isJust (intLinDiff l r)
isLinearFormula _ = False

-- Quick contradiction detection for simple non-negative expressions
constraintUnsatQuick :: Formula -> Bool
constraintUnsatQuick (Eq l r) =
  (nonNegExpr l && constNegative r) || (nonNegExpr r && constNegative l)
constraintUnsatQuick (Gt l r) =
  constNonPositive l && nonNegExpr r
constraintUnsatQuick _ = False

constNegative :: Expr -> Bool
constNegative (IntConst n) = n < 0
constNegative (Const r) = denominator r == 1 && numerator r < 0
constNegative _ = False

constNonPositive :: Expr -> Bool
constNonPositive (IntConst n) = n <= 0
constNonPositive (Const r) = denominator r == 1 && numerator r <= 0
constNonPositive _ = False

nonNegExpr :: Expr -> Bool
nonNegExpr (IntConst n) = n >= 0
nonNegExpr (Const r) = denominator r == 1 && numerator r >= 0
nonNegExpr (Pow _ k) = even k && k > 0
nonNegExpr _ = False

intSolve :: IntSolveOptions -> Theory -> Formula -> IntSolveOutcome
intSolve opts theory goal =
  let subM = buildIntSubMap theory
      theorySub = map (substituteInts subM) theory
      goalSub = substituteInts subM goal
  in case intEvalFormula goalSub of
       Just b  -> IntSolveOutcome (Just b) False False False False False
       Nothing -> intIntervalSolve opts True theorySub goalSub

-- Build substitutions from Eq IntVar IntConst (or symmetric) in theory
buildIntSubMap :: Theory -> M.Map String Expr
buildIntSubMap theory = fixedPoint M.empty
  where
    intEqs = [ f | f@(Eq _ _) <- theory ]

    fixedPoint acc =
      let acc' = foldl step acc intEqs
      in if acc' == acc then acc else fixedPoint acc'

    step m (Eq (IntVar v) rhs) = checkEval m v rhs
    step m (Eq lhs (IntVar v)) = checkEval m v lhs
    step m (Eq l r) = tryLinear m l r
    step m _ = m

    checkEval m v expr =
      let exprSub = substituteIntsExpr m expr
      in case intEvalExpr exprSub of
           Just n -> M.insert v (IntConst n) m
           Nothing -> tryLinear m (IntVar v) expr

    tryLinear m l r =
      case intLinDiff (substituteIntsExpr m l) (substituteIntsExpr m r) of
        Just (coeffs, c) ->
          let unknowns = Map.keys (Map.filter (/= 0) coeffs)
          in case unknowns of
               [v] ->
                 let a = coeffs Map.! v
                 in if a /= 0 && (-c) `mod` a == 0
                    then M.insert v (IntConst ((-c) `div` a)) m
                    else m
               _ -> m
        _ -> m

intEvalFormula :: Formula -> Maybe Bool
intEvalFormula (Eq l r) = do
  -- Try linear diff first
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c == 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a == b)
intEvalFormula (Ge l r) = do
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c >= 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a >= b)
intEvalFormula (Gt l r) = do
  case intLinDiff l r of
    Just (coeffs, c) | Map.null coeffs -> return (c > 0)
    _ -> do
      a <- intEvalExpr l
      b <- intEvalExpr r
      return (a > b)
intEvalFormula (Le l r) = intEvalFormula (Ge r l)
intEvalFormula (Lt l r) = intEvalFormula (Gt r l)
intEvalFormula (Divides l r) = do
  m <- intEvalExpr l
  val <- intEvalExpr r
  if m == 0 then Nothing else return (val `mod` m == 0)
intEvalFormula (And f1 f2) =
  case (intEvalFormula f1, intEvalFormula f2) of
    (Just False, _) -> Just False
    (_, Just False) -> Just False
    (Just True, Just True) -> Just True
    _ -> Nothing
intEvalFormula (Or f1 f2) =
  case (intEvalFormula f1, intEvalFormula f2) of
    (Just True, _) -> Just True
    (_, Just True) -> Just True
    (Just False, Just False) -> Just False
    _ -> Nothing
intEvalFormula (Not f) = fmap not (intEvalFormula f)
intEvalFormula _ = Nothing

intEvalExpr :: Expr -> Maybe Integer
intEvalExpr (IntConst n) = Just n
intEvalExpr (IntVar _) = Nothing
intEvalExpr (Const r) =
  let n = numerator r
      d = denominator r
  in if d == 1 then Just n else Nothing
intEvalExpr (Add a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x + y)
intEvalExpr (Sub a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x - y)
intEvalExpr (Mul a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  return (x * y)
intEvalExpr (Mod a b) = do
  x <- intEvalExpr a
  y <- intEvalExpr b
  if y == 0 then Nothing else return (x `mod` y)
intEvalExpr (Pow e n) = do
  x <- intEvalExpr e
  return (x ^ (fromIntegral n :: Integer))
intEvalExpr _ = Nothing

-- Linear integer form: returns (coefficients map, constant term)
-- Represents sum coeffs[var]*var + const
intLin :: Expr -> Maybe (Map.Map String Integer, Integer)
intLin (IntConst n) = Just (Map.empty, n)
intLin (Const r) =
  let n = numerator r; d = denominator r
  in if d == 1 then Just (Map.empty, n) else Nothing
intLin (IntVar v) = Just (Map.singleton v 1, 0)
intLin (Add a b) = do
  (ma, ca) <- intLin a
  (mb, cb) <- intLin b
  return (Map.unionWith (+) ma mb, ca + cb)
intLin (Sub a b) = do
  (ma, ca) <- intLin a
  (mb, cb) <- intLin b
  return (Map.unionWith (+) ma (Map.map negate mb), ca - cb)
intLin (Mul a b) =
  case (intLin a, intLin b) of
    (Just (ma, ca), Just (mb, cb))
      | Map.null ma -> Just (Map.map (* ca) mb, ca * cb)
      | Map.null mb -> Just (Map.map (* cb) ma, ca * cb)
    _ -> Nothing
intLin (Pow e 1) = intLin e
intLin _ = Nothing

-- Linearized difference l - r
intLinDiff :: Expr -> Expr -> Maybe (Map.Map String Integer, Integer)
intLinDiff l r = intLin (Sub l r)

-- Compute gcd of coefficients
gcdCoeffs :: Map.Map String Integer -> Integer
gcdCoeffs m
  | Map.null m = 0
  | otherwise = foldl1 gcd (map abs (Map.elems m))

-- Intersect an interval with a congruence class modulo g
congruenceInterval :: Integer -> Interval -> Maybe Interval
congruenceInterval g (Interval lo hi) =
  let snap n = Just (n + ((-n) `mod` g))
  in case (lo, hi) of
       (Just l, Just h) ->
         let l' = l + ((-l) `mod` g)
             h' = h - (h `mod` g)
         in if l' > h' then Nothing else Just (Interval (Just l') (Just h'))
       (Just l, Nothing) -> snap l >>= \l' -> Just (Interval (Just l') Nothing)
       (Nothing, Just h) -> Just (Interval Nothing (Just (h - (h `mod` g))))
       _ -> Just (Interval Nothing Nothing)

-- Modular feasibility check: gcd(coeffs) must divide constant for equality.
modularUnsat :: Map.Map String Integer -> Integer -> Bool
modularUnsat coeffs c =
  let g = gcdCoeffs coeffs
  in g /= 0 && (c `mod` g) /= 0

-- =============================================
-- Integer Interval Reasoner (very lightweight)
-- =============================================

data Interval = Interval { lower :: Maybe Integer, upper :: Maybe Integer } deriving (Show, Eq)

topInterval :: Interval
topInterval = Interval Nothing Nothing

singletonInterval :: Integer -> Interval
singletonInterval n = Interval (Just n) (Just n)

intersectInterval :: Interval -> Interval -> Maybe Interval
intersectInterval (Interval l1 u1) (Interval l2 u2) =
  let l = maxMaybe l1 l2
      u = minMaybe u1 u2
  in case (l, u) of
       (Just a, Just b) | a > b -> Nothing
       _ -> Just (Interval l u)

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing b = b
maxMaybe a Nothing = a
maxMaybe (Just a) (Just b) = Just (max a b)

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just (min a b)

shiftInterval :: Integer -> Interval -> Interval
shiftInterval s (Interval l u) = Interval (fmap (+s) l) (fmap (+s) u)

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (a, 1, 0)
gcdExt a b =
  let (g, x, y) = gcdExt b (a `mod` b)
  in (g, y, x - (a `div` b) * y)

modInv :: Integer -> Integer -> Maybe Integer
modInv a m =
  let (g, x, _) = gcdExt a m
  in if g == 1 then Just (x `mod` m) else Nothing

-- Extract integer variables from expressions/formulas
intVarsExpr :: Expr -> [String]
intVarsExpr (IntVar v) = [v]
intVarsExpr (Add a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Sub a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Mul a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Div a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Mod a b) = intVarsExpr a ++ intVarsExpr b
intVarsExpr (Pow e _) = intVarsExpr e
intVarsExpr (Sqrt e) = intVarsExpr e
intVarsExpr (Determinant rows) = concatMap intVarsExpr (concat rows)
intVarsExpr (Circle _ _ e) = intVarsExpr e
intVarsExpr _ = []

intVarsFormula :: Formula -> [String]
intVarsFormula (Eq l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Ge l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Gt l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Le l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Lt l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (Divides l r) = intVarsExpr l ++ intVarsExpr r
intVarsFormula (And f1 f2) = intVarsFormula f1 ++ intVarsFormula f2
intVarsFormula (Or f1 f2) = intVarsFormula f1 ++ intVarsFormula f2
intVarsFormula (Not f) = intVarsFormula f
intVarsFormula (Forall qs f) =
  let bound = [ qvName q | q <- qs, qvType q == QuantInt ]
  in filter (`notElem` bound) (intVarsFormula f)
intVarsFormula (Exists qs f) =
  let bound = [ qvName q | q <- qs, qvType q == QuantInt ]
  in filter (`notElem` bound) (intVarsFormula f)

-- Ceiling division for integers
ceilDiv :: Integer -> Integer -> Integer
ceilDiv _ 0 = 0
ceilDiv a b =
  let (q, r) = quotRem a b
  in if r == 0 then q else if (a > 0) == (b > 0) then q + 1 else q

-- Floor division for integers
floorDiv :: Integer -> Integer -> Integer
floorDiv _ 0 = 0
floorDiv a b =
  let (q, r) = quotRem a b
  in if r == 0 then q else if (a > 0) == (b > 0) then q else q - 1

-- Update a single variable interval from a linear inequality a*v + c >= 0 (or >0)
boundFromIneq :: Bool -> Integer -> Integer -> Interval
boundFromIneq strict a c
  | a == 0 =
      if (if strict then c > 0 else c >= 0)
         then topInterval
         else Interval (Just 1) (Just 0) -- empty, will be caught by intersect
  | a > 0 =
      let base = -c
          lb = if strict then ceilDiv (base + 1) a else ceilDiv base a
      in Interval (Just lb) Nothing
  | otherwise =
      let base = -c
          ub = if strict then floorDiv (base + 1) a else floorDiv base a
      in Interval Nothing (Just ub)

-- Decide linear goal using intervals if possible
evaluateIfSingleton :: Map.Map String Interval -> Expr -> Maybe Integer
evaluateIfSingleton env expr = eval expr
  where
    eval (IntConst n) = Just n
    eval (Const r) | denominator r == 1 = Just (numerator r)
    eval (IntVar v) = checkVar v
    eval (Var v) = checkVar v
    eval (Add a b) = (+) <$> eval a <*> eval b
    eval (Sub a b) = (-) <$> eval a <*> eval b
    eval (Mul a b) = (*) <$> eval a <*> eval b
    eval (Div a b) = do
       n <- eval a
       d <- eval b
       if d == 0 then Nothing else Just (n `div` d)
    eval (Mod a b) = do
       n <- eval a
       d <- eval b
       if d == 0 then Nothing else Just (n `mod` d)
    eval (Pow e n) = (^) <$> eval e <*> pure (fromIntegral n)
    eval _ = Nothing

    checkVar v =
      case Map.lookup v env of
        Just (Interval (Just lo) (Just hi)) | lo == hi -> Just lo
        _ -> Nothing

decideWithIntervals :: Map.Map String Interval -> Formula -> Maybe Bool
decideWithIntervals env (Eq l r) =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> Just (c == 0)
      | otherwise ->
          if modularUnsat coeffs c
             then Just False
             else
               case linearBounds coeffs c env of
                 Just (mn, mx)
                   | mn == 0 && mx == 0 -> Just True
                   | mx < 0 || mn > 0 -> Just False
                   | otherwise -> Nothing
                 _ -> Nothing
    Nothing ->
      case (evaluateIfSingleton env l, evaluateIfSingleton env r) of
        (Just a, Just b) -> Just (a == b)
        _ -> Nothing
decideWithIntervals env (Ge l r) = checkIneq False env l r
decideWithIntervals env (Gt l r) = checkIneq True env l r
decideWithIntervals env (Le l r) = checkIneq False env r l
decideWithIntervals env (Lt l r) = checkIneq True env r l
decideWithIntervals env (Divides mExpr expr) =
  case (intEvalExpr mExpr, intLin expr) of
    (Just m, Just (coeffs, k)) | m /= 0 ->
       case linearBounds coeffs k env of
         Just (lo, hi) ->
            if lo == hi 
            then Just (lo `mod` m == 0)
            else 
              if abs m == 1 then Just True else Nothing
         Nothing -> Nothing
    _ -> 
      case (evaluateIfSingleton env mExpr, evaluateIfSingleton env expr) of
        (Just m, Just val) | m /= 0 -> Just (val `mod` m == 0)
        _ -> Nothing
decideWithIntervals env (And f1 f2) =
  case (decideWithIntervals env f1, decideWithIntervals env f2) of
    (Just False, _) -> Just False
    (_, Just False) -> Just False
    (Just True, Just True) -> Just True
    _ -> Nothing
decideWithIntervals env (Or f1 f2) =
  case (decideWithIntervals env f1, decideWithIntervals env f2) of
    (Just True, _) -> Just True
    (_, Just True) -> Just True
    (Just False, Just False) -> Just False
    _ -> Nothing
decideWithIntervals env (Not f) = fmap not (decideWithIntervals env f)
decideWithIntervals _ _ = Nothing

checkIneq :: Bool -> Map.Map String Interval -> Expr -> Expr -> Maybe Bool
checkIneq strict env l r =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> Just (if strict then c > 0 else c >= 0)
      | otherwise ->
          case linearBounds coeffs c env of
            Just (mn, mx)
              | strict ->
                  if mn > 0 then Just True else if mx <= 0 then Just False else Nothing
              | otherwise ->
                  if mn >= 0 then Just True else if mx < 0 then Just False else Nothing
            _ -> Nothing
    Nothing ->
      case (evaluateIfSingleton env l, evaluateIfSingleton env r) of
        (Just a, Just b) -> Just (if strict then a > b else a >= b)
        _ -> Nothing

-- Interval-based integer solver: propagates simple linear constraints and then
-- tries to decide the goal.
intIntervalSolve :: IntSolveOptions -> Bool -> Theory -> Formula -> IntSolveOutcome
intIntervalSolve opts allowBranch theory goal =
  let vars = nub (concatMap intVarsFormula (goal : theory))
      initial = Map.fromList [ (v, topInterval) | v <- vars ]
      theoryWithGoal = goal : theory
      initialWithSolution =
        case solveDiophantine theoryWithGoal of
          Left False -> Left False
          Right (Just singles) ->
            foldM (\env (v, val) ->
                     case Map.lookup v env of
                       Nothing -> Right (Map.insert v (singletonInterval val) env)
                       Just iv ->
                         case intersectInterval iv (singletonInterval val) of
                           Just iv' -> Right (Map.insert v iv' env)
                           Nothing -> Left False
                  ) initial (Map.toList singles)
          Right Nothing -> Right initial
      -- Refinement + elimination loop
      solveLoop envIn theoryIn usedLP 0 = Right (envIn, theoryIn, usedLP)
      solveLoop envIn theoryIn usedLP n =
        let step = do
              envRefined <- iterateRefineFix envIn 20 theoryIn
              theoryPruned <- pruneConstraints envRefined theoryIn
              (envLP, usedLP1) <- linearRelaxation theoryIn goal envRefined
              envRef2 <- iterateRefineFix envLP 20 theoryPruned
              theoryPruned2 <- pruneConstraints envRef2 theoryPruned
              (envCooper, theoryCooper) <- multiCooper envRef2 theoryPruned2 goal
              let changed = envCooper /= envIn || theoryCooper /= theoryIn || usedLP1
                  usedLP' = usedLP || usedLP1
              pure (changed, envCooper, theoryCooper, usedLP')
        in case step of
             Left b -> Left b
             Right (changed, envCooper, theoryCooper, usedLP') ->
               if changed
                 then solveLoop envCooper theoryCooper usedLP' (n-1)
                 else Right (envCooper, theoryCooper, usedLP')

  in case initialWithSolution of
       Left b -> IntSolveOutcome (Just b) False False False False False
       Right env0 ->
          case solveLoop env0 theory False (8 :: Int) of
           Left _ -> IntSolveOutcome (Just True) False False False False False
           Right (envFinal, theoryFinal, usedLP) ->
             case decideWithIntervals envFinal goal of
               Just res -> IntSolveOutcome (Just res) False False False usedLP False
               Nothing  ->
                 if allowBranch
                   then case branchSmall opts envFinal theoryFinal goal of
                          Just res -> IntSolveOutcome (Just res) False False True usedLP False
                          Nothing -> IntSolveOutcome Nothing False False False usedLP False
                   else IntSolveOutcome Nothing False False False usedLP False

-- | Timeout-aware version of intIntervalSolve
-- Checks timeout before each iteration of the refinement loop
intIntervalSolveT :: IntSolveOptions -> Bool -> Theory -> Formula -> TimeoutM IntSolveOutcome
intIntervalSolveT opts allowBranch theory goal = do
  let vars = nub (concatMap intVarsFormula (goal : theory))
      initial = Map.fromList [ (v, topInterval) | v <- vars ]
      theoryWithGoal = goal : theory
      initialWithSolution =
        case solveDiophantine theoryWithGoal of
          Left False -> Left False
          Right (Just singles) ->
            foldM (\env (v, val) ->
                     case Map.lookup v env of
                       Nothing -> Right (Map.insert v (singletonInterval val) env)
                       Just iv ->
                         case intersectInterval iv (singletonInterval val) of
                           Just iv' -> Right (Map.insert v iv' env)
                           Nothing -> Left False
                  ) initial (Map.toList singles)
          Right Nothing -> Right initial
      -- Timeout-aware refinement loop
      solveLoopT envIn theoryIn usedLP 0 = return $ Right (envIn, theoryIn, usedLP)
      solveLoopT envIn theoryIn usedLP n = do
        timedOut <- checkTimeout
        if timedOut
          then error "Integer solver timeout exceeded in refinement loop"
          else
            let step = do
                  envRefined <- iterateRefineFix envIn 20 theoryIn
                  theoryPruned <- pruneConstraints envRefined theoryIn
                  (envLP, usedLP1) <- linearRelaxation theoryIn goal envRefined
                  envRef2 <- iterateRefineFix envLP 20 theoryPruned
                  theoryPruned2 <- pruneConstraints envRef2 theoryPruned
                  (envCooper, theoryCooper) <- multiCooper envRef2 theoryPruned2 goal
                  let changed = envCooper /= envIn || theoryCooper /= theoryIn || usedLP1
                      usedLP' = usedLP || usedLP1
                  pure (changed, envCooper, theoryCooper, usedLP')
            in case step of
                 Left b -> return $ Left b
                 Right (changed, envCooper, theoryCooper, usedLP') ->
                   if changed
                     then solveLoopT envCooper theoryCooper usedLP' (n-1)
                     else return $ Right (envCooper, theoryCooper, usedLP')

  case initialWithSolution of
    Left b -> return $ IntSolveOutcome (Just b) False False False False False
    Right env0 -> do
      result <- solveLoopT env0 theory False (8 :: Int)
      case result of
        Left _ -> return $ IntSolveOutcome (Just True) False False False False False
        Right (envFinal, theoryFinal, usedLP) ->
          case decideWithIntervals envFinal goal of
            Just res -> return $ IntSolveOutcome (Just res) False False False usedLP False
            Nothing  ->
              if allowBranch
                then case branchSmall opts envFinal theoryFinal goal of
                       Just res -> return $ IntSolveOutcome (Just res) False False True usedLP False
                       Nothing -> return $ IntSolveOutcome Nothing False False False usedLP False
                else return $ IntSolveOutcome Nothing False False False usedLP False

-- Refinement helpers parameterized by theory
iterateRefineFix :: Map.Map String Interval -> Int -> Theory -> Either Bool (Map.Map String Interval)
iterateRefineFix env 0 _ = Right env
iterateRefineFix env n theory =
  case refinePass env theory of
    Left b -> Left b
    Right env' ->
      if env' == env then Right env else iterateRefineFix env' (n-1) theory

refinePass :: Map.Map String Interval -> Theory -> Either Bool (Map.Map String Interval)
refinePass env theory =
  let eqRes = foldl stepEq (Right env) theory
      stepEq (Left b) _ = Left b
      stepEq (Right acc) f@(Eq _ _) = applyEq acc f
      stepEq (Right acc) _ = Right acc
  in eqRes >>= \envAfterEqs ->
       let geRes = foldl stepGe (Right envAfterEqs) theory
           stepGe (Left b) _ = Left b
           stepGe (Right acc) f@(Divides _ _) = applyDivides acc f
           stepGe (Right acc) f = applyGe acc (case f of Gt _ _ -> True; _ -> False) f
       in geRes

applyEq :: Map.Map String Interval -> Formula -> Either Bool (Map.Map String Interval)
applyEq env (Eq l r) =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs -> if c == 0 then Right env else Left False
      | Map.size coeffs == 1 ->
          case Map.toList coeffs of
            [(v, a)] ->
              if a == 0
                then Left False
                else
                  let (q, r) = quotRem (-c) a
                  in if r /= 0
                     then Left False
                     else case Map.lookup v env of
                            Nothing -> Right (Map.insert v (singletonInterval q) env)
                            Just iv ->
                              case intersectInterval iv (singletonInterval q) of
                                Just newIv -> Right (Map.insert v newIv env)
                                Nothing -> Left False
            _ -> Right env
      | otherwise ->
          case refineEqIntervals env coeffs c of
            Left b -> Left b
            Right env' ->
              case linearBounds coeffs c env' of
                Just (mn, mx)
                  | mn == 0 && mx == 0 -> Right env'
                  | mx < 0 || mn > 0 -> Left False
                  | otherwise -> Right env'
                _ -> Right env'
    _ -> Right env
applyEq env _ = Right env

applyGe :: Map.Map String Interval -> Bool -> Formula -> Either Bool (Map.Map String Interval)
applyGe env strict (Ge l r) = applyIneq env strict l r
applyGe env strict (Gt l r) = applyIneq env strict l r
applyGe env _ _ = Right env

applyIneq :: Map.Map String Interval -> Bool -> Expr -> Expr -> Either Bool (Map.Map String Interval)
applyIneq env strict l r =
  case intLinDiff l r of
    Just (coeffs, c)
      | Map.null coeffs ->
          let ok = if strict then c > 0 else c >= 0
          in if ok then Right env else Left False
      | Map.size coeffs == 1 ->
          case Map.toList coeffs of
            [(v, a)] ->
              let newIv = boundFromIneq strict a c
                  g = gcdCoeffs coeffs
                  newIv' = if g > 0 then fromMaybe newIv (congruenceInterval g newIv) else newIv
              in case Map.lookup v env of
                   Nothing -> Right (Map.insert v newIv' env)
                   Just iv ->
                     case intersectInterval iv newIv' of
                       Just iv' -> Right (Map.insert v iv' env)
                       Nothing -> Left False
            _ -> Right env
      | otherwise ->
          case refineIneqIntervals env coeffs c strict of
            Left b -> Left b
            Right env' -> Right env'
    _ -> Right env

applyDivides :: Map.Map String Interval -> Formula -> Either Bool (Map.Map String Interval)
applyDivides env (Divides mExpr expr) =
  case (intEvalExpr mExpr, intLin expr) of
    (Just m, Just (coeffs, k)) | m /= 0 ->
       if Map.null coeffs 
       then if k `mod` m == 0 then Right env else Left False
       else if Map.size coeffs == 1 
            then 
              let [(v, a)] = Map.toList coeffs
                  b = (-k) `mod` m
                  g = gcd a m
              in if b `mod` g /= 0 
                 then Left False
                 else
                   let a' = a `div` g
                       b' = b `div` g
                       m' = abs (m `div` g)
                   in case modInv a' m' of
                        Just inv ->
                           let x0 = (b' * inv) `mod` m'
                           in case Map.lookup v env of
                                Nothing -> Right env
                                Just iv ->
                                  let ivShifted = shiftInterval (-x0) iv
                                  in case congruenceInterval m' ivShifted of
                                       Just ivShiftedRefined ->
                                          let ivRefined = shiftInterval x0 ivShiftedRefined
                                          in Right (Map.insert v ivRefined env)
                                       Nothing -> Left False
                        Nothing -> Right env
            else Right env
    _ -> Right env
applyDivides env _ = Right env

-- Compute tightest finite bounds of linear expression sum a_i * v_i + c
-- using current intervals. Returns Nothing if any needed bound is infinite.
linearBounds :: Map.Map String Integer -> Integer -> Map.Map String Interval -> Maybe (Integer, Integer)
linearBounds coeffs c env = foldM step (c, c) (Map.toList coeffs)
  where
    step (mn, mx) (v, a)
      | a == 0 = Just (mn, mx)
      | otherwise =
          case Map.lookup v env of
            Nothing -> Nothing
            Just iv ->
              case boundsForCoeff a (lower iv) (upper iv) of
                Nothing -> Nothing
                Just (lTerm, uTerm) -> Just (mn + lTerm, mx + uTerm)

-- Multiply interval by coefficient; requires finite bounds
boundsForCoeff :: Integer -> Maybe Integer -> Maybe Integer -> Maybe (Integer, Integer)
boundsForCoeff coef lo hi
  | coef > 0 = do
      l <- fmap (* coef) lo
      u <- fmap (* coef) hi
      return (l, u)
  | otherwise = do
      l <- fmap (* coef) hi
      u <- fmap (* coef) lo
      return (l, u)

-- GCD of coefficient magnitudes (0 if empty)
-- Interval refinement for linear equalities with multiple variables.
-- Uses current finite bounds of other variables to tighten each variable.
refineEqIntervals :: Map.Map String Interval -> Map.Map String Integer -> Integer -> Either Bool (Map.Map String Interval)
refineEqIntervals env coeffs c =
  foldM refineOne env (Map.toList coeffs)
  where
    finite (Interval (Just _) (Just _)) = True
    finite _ = False

    -- Compute combined min/max of all variables except v
    restRange v =
      let others = Map.delete v coeffs
      in foldM (\(mn, mx) (u, a) ->
                 case Map.lookup u env of
                   Just iv | finite iv ->
                     case boundsForCoeff a (lower iv) (upper iv) of
                       Just (l, u') -> Right (mn + l, mx + u')
                       Nothing -> Left False
                   _ -> Left False
               ) (0, 0) (Map.toList others)

    refineOne acc (v, a) =
      case Map.lookup v acc of
        Nothing -> Right acc
        Just iv
          | not (finite iv) -> Right acc
          | otherwise ->
              case restRange v of
                Left False -> Left False
                Left _ -> Right acc
                Right (restMin, restMax) ->
                  let g = gcdCoeffs coeffs
                      targetMin = -restMax - c
                      targetMax = -restMin - c
                      (lo', hi') =
                        if a > 0
                          then (ceilDiv targetMin a, floorDiv targetMax a)
                          else (ceilDiv targetMax a, floorDiv targetMin a)
                      tightened = Interval (Just lo') (Just hi')
                      -- also enforce congruence mod gcd if applicable
                      tightened' =
                        if g > 0
                          then case congruenceInterval g iv of
                                 Just congr -> intersectInterval tightened congr
                                 Nothing -> Nothing
                          else Just tightened
                  in case tightened' of
                       Just newIv -> Right (Map.insert v newIv acc)
                       Nothing -> Left False

-- Interval refinement for linear inequalities with multiple variables.
-- Uses current finite bounds (when available) to tighten single-variable bounds.
refineIneqIntervals :: Map.Map String Interval -> Map.Map String Integer -> Integer -> Bool -> Either Bool (Map.Map String Interval)
refineIneqIntervals env coeffs c strict =
  foldM refineOne env (Map.toList coeffs)
  where
    refineOne acc (v, a) =
      case Map.lookup v acc of
        Nothing -> Right acc
        Just iv ->
          case restMaxSum v coeffs acc of
            Nothing -> Right acc
            Just rMax ->
              let c' = c + rMax
                  newIvBase = boundFromIneq strict a c'
                  g = gcdCoeffs coeffs
                  newIv = if g > 0 then fromMaybe newIvBase (congruenceInterval g newIvBase) else newIvBase
              in case intersectInterval iv newIv of
                   Just iv' -> Right (Map.insert v iv' acc)
                   Nothing -> Left False

-- Maximum possible contribution of all variables except v.
-- Returns Nothing if any needed bound is infinite/unknown.
restMaxSum :: String -> Map.Map String Integer -> Map.Map String Interval -> Maybe Integer
restMaxSum v coeffs env = foldM step 0 (Map.toList (Map.delete v coeffs))
  where
    step acc (u, a) =
      case Map.lookup u env of
        Just iv ->
          case maxContribution a iv of
            Just m -> Just (acc + m)
            Nothing -> Nothing
        Nothing -> Nothing

maxContribution :: Integer -> Interval -> Maybe Integer
maxContribution a (Interval lo hi)
  | a >= 0 = fmap (* a) hi
  | otherwise = fmap (* a) lo

-- Targeted small-branch search: pick the smallest finite interval and try assignments
branchSmall :: IntSolveOptions -> Map.Map String Interval -> Theory -> Formula -> Maybe Bool
branchSmall _opts env theory _goal = solveRec env theory
  where
    solveRec currEnv currTheory =
      case pruneConstraints currEnv currTheory of
        Left False -> Nothing
        Right [] -> Just True
        Right remTheory ->
          case smallestNonSingleton currEnv of
            Nothing -> Nothing
            Just (v, vals) ->
               if length vals > 1000 then Nothing
               else searchVals v currEnv remTheory vals

    searchVals _ _ _ [] = Nothing
    searchVals v e th (x:xs) =
      let nextEnv = Map.insert v (singletonInterval x) e
      in case solveRec nextEnv th of
           Just True -> Just True
           _ -> searchVals v e th xs

    smallestNonSingleton m =
      let candidates = [ (v, [lo..hi]) 
                       | (v, Interval (Just lo) (Just hi)) <- Map.toList m
                       , lo < hi ]
      in if null candidates 
         then Nothing 
         else Just (minimumBy (\(_,a) (_,b) -> compare (length a) (length b)) candidates)

-- Remove constraints already decided by current intervals; detect contradictions early.
pruneConstraints :: Map.Map String Interval -> Theory -> Either Bool Theory
pruneConstraints env = foldr step (Right [])
  where
    step f acc =
      case acc of
        Left b -> Left b
        Right kept ->
          case decideWithIntervals env f of
            Just True -> Right kept
            Just False -> Left False
            Nothing -> Right (f : kept)

-- =============================================
-- LP relaxation (very lightweight)
-- =============================================

-- Linear relaxation for bounds using coefficient signs and current intervals.
-- For now, recompute global min/max for each variable and intersect.
linearRelaxation :: Theory -> Formula -> Map.Map String Interval -> Either Bool (Map.Map String Interval, Bool)
linearRelaxation theory goal env =
  foldM tighten (env, False) (varsInEnv env)
  where
    varsInEnv m = Map.keys m
    tighten (accEnv, usedFlag) v =
      let contribs = collectConstraints v theory goal
          merged = foldM (applyConstraint v accEnv) (accEnv, usedFlag) contribs
      in case merged of
           Left False -> Left False
           Right (acc', flag') -> Right (acc', flag')

    collectConstraints v theory goal =
      [ (coeffs, c, False)
      | Eq l r <- theory
      , Just (coeffs, c) <- [intLinDiff l r]
      , Map.member v coeffs
      ] ++
      [ (coeffs, c, strict)
      | f <- theory ++ [goal]
      , (coeffs, c, strict) <- linearIneq v f
      ]

    linearIneq v (Ge l r) = maybeToList (lin v False l r)
    linearIneq v (Gt l r) = maybeToList (lin v True l r)
    linearIneq _ _ = []

    lin v strict l r =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs -> Just (coeffs, c, strict)
        _ -> Nothing

    applyConstraint v envAcc (_currentEnv, used) (coeffs, c, strict) =
      let a = coeffs Map.! v
          (restMin, restMax) = sumBounds (Map.delete v coeffs) envAcc
          (lo, hi) =
            if a > 0
              then (ceilDiv (-c - restMax) a, floorDiv (-c - restMin - if strict then 1 else 0) a)
              else (ceilDiv (-c - restMin - if strict then 1 else 0) a, floorDiv (-c - restMax) a)
          newIv = Interval (Just lo) (Just hi)
      in case Map.lookup v envAcc of
           Just iv ->
             case intersectInterval iv newIv of
               Just iv' ->
                 if iv' == iv
                   then Right (envAcc, used)
                   else Right (Map.insert v iv' envAcc, True)
               Nothing -> Left False
           Nothing -> Right (Map.insert v newIv envAcc, True)

    sumBounds coeffs envAcc =
      Map.foldlWithKey' (\(mn,mx) var a ->
        case Map.lookup var envAcc of
          Just (Interval lo hi) ->
            let loC = fmap (* a) (if a >= 0 then lo else hi)
                hiC = fmap (* a) (if a >= 0 then hi else lo)
            in (addMaybe mn loC, addMaybe mx hiC)
          Nothing -> (mn,mx)
      ) (0,0) coeffs

    addMaybe base m = maybe base (\x -> base + x) m

-- =============================================
-- Diophantine solver (Smith/Hermite-lite)
-- =============================================

-- Attempts to solve a system of linear Diophantine equations represented as Theory.
-- Returns Left False on inconsistency, Right (Just assignments) if a unique integer solution
-- for some variables is found, Right Nothing if underdetermined but consistent.
solveDiophantine :: Theory -> Either Bool (Maybe (Map.Map String Integer))
solveDiophantine theory =
  let eqs = [ (coeffs, c) | Eq l r <- theory, Just (coeffs, c) <- [intLinDiff l r] ]
      vars = sort . nub $ concatMap (Map.keys . fst) eqs
  in if null eqs then Right Nothing else solveSystem vars (map simplifyEq eqs)

simplifyEq :: (Map.Map String Integer, Integer) -> (Map.Map String Integer, Integer)
simplifyEq (coeffs, c) =
  let g1 = gcdCoeffs coeffs
      g = if g1 == 0 then abs c else gcd g1 (abs c)
  in if g <= 1 then (coeffs, c) else (Map.map (`div` g) coeffs, c `div` g)

solveSystem :: [String] -> [(Map.Map String Integer, Integer)] -> Either Bool (Maybe (Map.Map String Integer))
solveSystem [] eqs =
  if any (\(m,c) -> Map.null m && c /= 0) eqs then Left False else Right Nothing
solveSystem (v:vs) eqs =
  let (withV, withoutV) = L.partition (\(m,_) -> Map.findWithDefault 0 v m /= 0) eqs
  in case withV of
       [] -> solveSystem vs eqs
       (pivot:eqsV) ->
         case eliminateVar v pivot eqsV of
           Left False -> Left False
           Right reduced ->
             case solveSystem vs (withoutV ++ reduced) of
               Left False -> Left False
               Right maybeSol ->
                 case singletonValue v (pivot:withoutV ++ reduced) of
                   Just val ->
                     let sol = Map.insert v val (fromMaybe Map.empty maybeSol)
                     in Right (Just sol)
                   Nothing -> Right maybeSol

-- Eliminate variable v from the equations using pivot as base
eliminateVar :: String -> (Map.Map String Integer, Integer) -> [(Map.Map String Integer, Integer)] -> Either Bool [(Map.Map String Integer, Integer)]
eliminateVar v pivot eqs =
  let a = Map.findWithDefault 0 v (fst pivot)
  in foldM (step a pivot) [] eqs
  where
    step a (m0,c0) acc (m,c) =
      let b = Map.findWithDefault 0 v m
      in if b == 0 then Right ((m,c):acc)
         else
           let l = lcm (abs a) (abs b)
               scale0 = l `div` a
               scale = l `div` b
               m' = Map.filter (/=0) $ Map.unionWith (-) (Map.map (* scale) m) (Map.map (* scale0) m0)
               c' = scale*c - scale0*c0
               simplified = simplifyEq (m', c')
           in if Map.null (fst simplified) && snd simplified /= 0
                 then Left False
                 else Right (simplified:acc)

-- If an equation reduces to a single variable, compute its value if integer.
singletonValue :: String -> [(Map.Map String Integer, Integer)] -> Maybe Integer
singletonValue v eqs =
  let singles = [ (a, c) | (m,c) <- eqs, Map.size m == 1, Map.member v m, let a = m Map.! v ]
  in case singles of
       [] -> Nothing
       ((a,c):_) ->
         if a /= 0 && c `mod` a == 0
           then Just ((-c) `div` a)
           else Nothing

-- =============================================
-- Cooper-style elimination (single variable)
-- =============================================

-- A simplified Cooper elimination for one variable: we try to tighten bounds using
-- inequalities mentioning a single variable, including modulus constraints derived
-- from equalities.
cooperEliminate :: Map.Map String Interval -> Theory -> Formula -> Either Bool (Map.Map String Interval, Theory)
cooperEliminate env theory goal =
  case pickFiniteVar env of
    Nothing -> Right (env, theory)
    Just v ->
      let withV = [ f | f <- theory, mentions v f ]
          eqMods = catMaybes [ modulusFromEq v f | f <- withV ]
          tightened = foldM (tightenWith v) env (withV ++ [goal])
      in case tightened of
           Left b -> Left b
           Right env' ->
             case applyModuli v eqMods env' of
               Left b -> Left b
               Right env'' -> Right (env'', theory)
  where
    mentions v (Eq l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Ge l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Gt l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Le l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))
    mentions v (Lt l r) = Map.member v (fst (fromMaybe (Map.empty,0) (intLinDiff l r)))

    modulusFromEq v (Eq l r) =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs ->
          let a = coeffs Map.! v
          in Just (abs a, (-c) `mod` abs a)
        _ -> Nothing
    modulusFromEq _ _ = Nothing

    tightenWith v envAcc f =
      case f of
        Ge l r -> tightenIneq v envAcc l r False
        Gt l r -> tightenIneq v envAcc l r True
        Le l r -> tightenIneq v envAcc r l False  -- Flip: l <= r becomes r >= l
        Lt l r -> tightenIneq v envAcc r l True   -- Flip: l < r becomes r > l
        _ -> Right envAcc

    tightenIneq v envAcc l r strict =
      case intLinDiff l r of
        Just (coeffs, c) | Map.member v coeffs ->
          let a = coeffs Map.! v
              (restMin, restMax) = sumBounds (Map.delete v coeffs) envAcc
              (lo, hi) =
                if a > 0
                  then (ceilDiv (-c - restMax) a, floorDiv (-c - restMin - if strict then 1 else 0) a)
                  else (ceilDiv (-c - restMin - if strict then 1 else 0) a, floorDiv (-c - restMax) a)
              newIv = Interval (Just lo) (Just hi)
          in case Map.lookup v envAcc of
               Just iv ->
                 case intersectInterval iv newIv of
                   Just iv' -> Right (Map.insert v iv' envAcc)
                   Nothing -> Left False
               Nothing -> Right (Map.insert v newIv envAcc)
        _ -> Right envAcc

    sumBounds coeffs envAcc =
      Map.foldlWithKey' (\(mn,mx) var a ->
        case Map.lookup var envAcc of
          Just (Interval lo hi) ->
            let loC = fmap (* a) (if a >= 0 then lo else hi)
                hiC = fmap (* a) (if a >= 0 then hi else lo)
            in (maybe mn (+mn) loC, maybe mx (+mx) hiC)
          Nothing -> (mn,mx)
      ) (0,0) coeffs

    applyModuli v mods envAcc =
      case Map.lookup v envAcc of
        Nothing -> Right envAcc
        Just iv ->
          let intersected = foldM (\ivAcc (m, _) ->
                                      case congruenceInterval m ivAcc of
                                        Just iv' -> Right iv'
                                        Nothing -> Left False
                                  ) iv mods
          in case intersected of
               Left False -> Left False
               Right iv' -> Right (Map.insert v iv' envAcc)

    pickFiniteVar envAcc =
      let finite = [ (v, iv) | (v, iv@(Interval (Just _) (Just _))) <- Map.toList envAcc ]
      in case finite of
           [] -> Nothing
           (x:_) -> Just (fst x)

pickFiniteVarCooper :: Map.Map String Interval -> Maybe String
pickFiniteVarCooper envAcc =
  let finite = [ (v, iv) | (v, iv@(Interval (Just _) (Just _))) <- Map.toList envAcc ]
  in case finite of
       [] -> Nothing
       (x:_) -> Just (fst x)

-- Repeated Cooper elimination on finite-bounded variables
multiCooper :: Map.Map String Interval -> Theory -> Formula -> Either Bool (Map.Map String Interval, Theory)
multiCooper env theory goal = cooperLoop env theory
  where
    cooperLoop envAcc theoryAcc =
      case pickFiniteVarCooper envAcc of
        Nothing -> Right (envAcc, theoryAcc)
        Just _ ->
          case cooperEliminate envAcc theoryAcc goal of
            Left b -> Left b
            Right (env', theory') ->
              if env' == envAcc then Right (envAcc, theoryAcc)
              else cooperLoop env' theory'
