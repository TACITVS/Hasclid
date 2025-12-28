{-|
Module: Preprocessing
Description: Intelligent preprocessing for automatic theorem proving

This module implements preprocessing strategies that commercial theorem provers use:
1. Automatic variable substitution from theory
2. 2D geometry detection and z-coordinate elimination
3. Early simplification before expensive polynomial operations
4. Constraint propagation and redundancy elimination

PHILOSOPHY: A true automatic theorem prover must be intelligent about problem structure,
not just apply brute force algorithms.
-}

module Preprocessing
  ( preprocess
  , preprocessGeometry
  , PreprocessingResult(..)
  , detectDimension
  , extractSubstitutions
  , applySubstitutionsFormula
  , applySubstitutionsExpr
  , simplifyTheory
  ) where

import Expr
import Geometry.Barycentric (applyBarycentric)
import Geometry.WLOG (detectPoints)
import Data.List (nub, find)
import Data.Maybe (mapMaybe, isJust)
import qualified Data.Map.Strict as M

-- | Result of preprocessing
data PreprocessingResult = PreprocessingResult
  { preprocessedTheory :: Theory
  , preprocessedGoal :: Formula
  , substitutions :: M.Map String Expr  -- Variable -> Value substitutions
  , dimension :: GeometryDimension
  , eliminatedVars :: [String]
  , simplificationLog :: [String]
  } deriving (Show, Eq)

data GeometryDimension = Dimension1D | Dimension2D | Dimension3D | DimensionUnknown
  deriving (Show, Eq)

-- | Main preprocessing pipeline
preprocess :: M.Map String Expr -> Theory -> Formula -> PreprocessingResult
preprocess initialSubs theory goal =
  let
    -- Step 1: Extract simple substitutions from theory (x = c, y = 0, etc.)
    extractedSubs = extractSubstitutions theory
    -- Merge with initial substitutions (initialSubs takes precedence for point coordinates)
    subs = M.union initialSubs extractedSubs
    log1 = ["Extracted " ++ show (M.size extractedSubs) ++ " substitutions from theory, " ++
            show (M.size initialSubs) ++ " from point definitions: " ++
            show (take 5 $ M.toList subs)]

    -- Step 1a: Expand distance definitions (R1s = dist2 A P, etc.) without expanding dist2 itself
    dist2Subs = extractDist2Substitutions theory
    theoryD = if M.null dist2Subs
              then theory
              else map (applySubstitutionsFormulaNoExpand dist2Subs) theory
    goalD = if M.null dist2Subs
            then goal
            else applySubstitutionsFormulaNoExpand dist2Subs goal
    log1a = if M.null dist2Subs
            then log1
            else log1 ++ ["Expanded " ++ show (M.size dist2Subs) ++ " distance definitions"]

    -- Step 1b: Apply barycentric distance substitution if inside-triangle constraints are present
    (theoryB, goalB, logBary) = applyBarycentricIfInside subs theoryD goalD
    log1b = if null logBary then log1a else log1a ++ logBary

    -- Step 1c: Add non-negativity constraints for squared distances and definitions
    (theoryNN, logNN) = addDist2NonnegConstraints theoryB goalB
    log1c = if null logNN then log1b else log1b ++ logNN

    -- Step 2: Detect geometry dimension (check both theory and substitutions)
    dim = detectDimension subs theoryNN goalB
    log2 = log1c ++ ["Detected dimension: " ++ show dim]

    -- Step 3: Add dimension-specific substitutions (e.g., all z-coords = 0 for 2D)
    subs' = case dim of
              Dimension2D -> addZeroZCoordinates theoryNN goalB subs
              _ -> subs
    log3 = if dim == Dimension2D && M.size subs' > M.size subs
           then log2 ++ ["2D detected: eliminated " ++ show (M.size subs' - M.size subs) ++ " z-coordinates"]
           else log2

    -- Step 4: Apply substitutions to theory and goal
    theory' = map (applySubstitutionsFormula subs') theoryNN
    goal' = applySubstitutionsFormula subs' goalB
    log4 = log3 ++ ["Applied " ++ show (M.size subs') ++ " substitutions"]

    -- Step 5: Simplify theory (remove tautologies, redundant constraints)
    theory'' = simplifyTheory theory'
    log5 = if length theory'' < length theory'
           then log4 ++ ["Simplified theory: " ++ show (length theory' - length theory'') ++ " redundant constraints removed"]
           else log4

  in PreprocessingResult
       { preprocessedTheory = theory''
       , preprocessedGoal = goal'
       , substitutions = subs'
       , dimension = dim
       , eliminatedVars = M.keys subs'
       , simplificationLog = log5
       }

-- | Lightweight geometry normalization before sqrt/rational elimination.
-- Expands dist2 definitions, applies barycentric rewrite if inside-triangle is detected,
-- and adds non-negativity constraints for squared distances.
preprocessGeometry :: M.Map String Expr -> Theory -> Formula -> (Theory, Formula, [String])
preprocessGeometry subs theory goal =
  let
    dist2Subs = extractDist2Substitutions theory
    theoryD = if M.null dist2Subs
              then theory
              else map (applySubstitutionsFormulaNoExpand dist2Subs) theory
    goalD = if M.null dist2Subs
            then goal
            else applySubstitutionsFormulaNoExpand dist2Subs goal
    log1 = if M.null dist2Subs
           then []
           else ["Expanded " ++ show (M.size dist2Subs) ++ " distance definitions"]

    (theoryB, goalB, logBary) = applyBarycentricIfInside subs theoryD goalD
    (theoryNN, logNN) = addDist2NonnegConstraints theoryB goalB
  in (theoryNN, goalB, log1 ++ logBary ++ logNN)

extractDist2Substitutions :: Theory -> M.Map String Expr
extractDist2Substitutions theory =
  M.fromList $ mapMaybe extractDist2 theory
  where
    extractDist2 (Eq (Var v) e@(Dist2 _ _)) = Just (v, e)
    extractDist2 (Eq e@(Dist2 _ _) (Var v)) = Just (v, e)
    extractDist2 _ = Nothing

applySubstitutionsFormulaNoExpand :: M.Map String Expr -> Formula -> Formula
applySubstitutionsFormulaNoExpand subs = mapFormulaNoExpand (applySubstitutionsExprNoExpand subs)

applySubstitutionsExprNoExpand :: M.Map String Expr -> Expr -> Expr
applySubstitutionsExprNoExpand subs = go
  where
    go (Var v) = case M.lookup v subs of
                   Just e -> e
                   Nothing -> Var v
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Sub e1 e2) = Sub (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (Div e1 e2) = Div (go e1) (go e2)
    go (Mod e1 e2) = Mod (go e1) (go e2)
    go (Pow e n) = Pow (go e) n
    go (Sqrt e) = Sqrt (go e)
    go (Determinant rows) = Determinant (map (map go) rows)
    go (Circle p c r) = Circle p c (go r)
    go (Sum i lo hi body) = Sum i (go lo) (go hi) (go body)
    go e = e

mapFormulaNoExpand :: (Expr -> Expr) -> Formula -> Formula
mapFormulaNoExpand f (Eq e1 e2) = Eq (f e1) (f e2)
mapFormulaNoExpand f (Le e1 e2) = Le (f e1) (f e2)
mapFormulaNoExpand f (Lt e1 e2) = Lt (f e1) (f e2)
mapFormulaNoExpand f (Ge e1 e2) = Ge (f e1) (f e2)
mapFormulaNoExpand f (Gt e1 e2) = Gt (f e1) (f e2)
mapFormulaNoExpand f (Divides e1 e2) = Divides (f e1) (f e2)
mapFormulaNoExpand f (And f1 f2) = And (mapFormulaNoExpand f f1) (mapFormulaNoExpand f f2)
mapFormulaNoExpand f (Or f1 f2) = Or (mapFormulaNoExpand f f1) (mapFormulaNoExpand f f2)
mapFormulaNoExpand f (Not form) = Not (mapFormulaNoExpand f form)
mapFormulaNoExpand f (Forall qvs form) = Forall qvs (mapFormulaNoExpand f form)
mapFormulaNoExpand f (Exists qvs form) = Exists qvs (mapFormulaNoExpand f form)

addDist2NonnegConstraints :: Theory -> Formula -> (Theory, [String])
addDist2NonnegConstraints theory goal =
  let
    allFormulas = goal : theory
    dist2Exprs = nub $ concatMap dist2ExprsInFormula allFormulas
    dist2Vars = nub $ concatMap dist2VarDefs theory
    varConstraints =
      [ Ge (Var v) (Const 0)
      | v <- dist2Vars
      , not (hasNonNegConstraint (Var v) allFormulas)
      ]
    exprConstraints =
      [ Ge e (Const 0)
      | e <- dist2Exprs
      , not (hasNonNegConstraint e allFormulas)
      ]
    newConstraints = varConstraints ++ exprConstraints
  in if null newConstraints
     then (theory, [])
     else (newConstraints ++ theory
          , ["Added " ++ show (length newConstraints) ++ " dist2 non-negativity constraints"])

dist2VarDefs :: Formula -> [String]
dist2VarDefs (Eq (Var v) (Dist2 _ _)) = [v]
dist2VarDefs (Eq (Dist2 _ _) (Var v)) = [v]
dist2VarDefs _ = []

dist2ExprsInFormula :: Formula -> [Expr]
dist2ExprsInFormula (Eq e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (Ge e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (Gt e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (Le e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (Lt e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (Divides e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInFormula (And f1 f2) = dist2ExprsInFormula f1 ++ dist2ExprsInFormula f2
dist2ExprsInFormula (Or f1 f2) = dist2ExprsInFormula f1 ++ dist2ExprsInFormula f2
dist2ExprsInFormula (Not f) = dist2ExprsInFormula f
dist2ExprsInFormula (Forall _ f) = dist2ExprsInFormula f
dist2ExprsInFormula (Exists _ f) = dist2ExprsInFormula f

dist2ExprsInExpr :: Expr -> [Expr]
dist2ExprsInExpr e@(Dist2 _ _) = [e]
dist2ExprsInExpr (Add e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInExpr (Sub e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInExpr (Mul e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInExpr (Div e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInExpr (Mod e1 e2) = dist2ExprsInExpr e1 ++ dist2ExprsInExpr e2
dist2ExprsInExpr (Pow e1 _) = dist2ExprsInExpr e1
dist2ExprsInExpr (Sqrt e1) = dist2ExprsInExpr e1
dist2ExprsInExpr (Determinant rows) = concatMap (concatMap dist2ExprsInExpr) rows
dist2ExprsInExpr (Circle _ _ r) = dist2ExprsInExpr r
dist2ExprsInExpr (Sum _ lo hi body) =
  dist2ExprsInExpr lo ++ dist2ExprsInExpr hi ++ dist2ExprsInExpr body
dist2ExprsInExpr _ = []

hasNonNegConstraint :: Expr -> [Formula] -> Bool
hasNonNegConstraint expr formulas =
  any (matchesNonNeg expr) formulas

matchesNonNeg :: Expr -> Formula -> Bool
matchesNonNeg expr f =
  case f of
    Ge e (Const 0) -> e == expr
    Gt e (Const 0) -> e == expr
    Eq e (Const 0) -> e == expr
    Eq (Const 0) e -> e == expr
    _ -> False

applyBarycentricIfInside :: M.Map String Expr -> Theory -> Formula -> (Theory, Formula, [String])
applyBarycentricIfInside subs theory goal =
  case findInsideBarycentric subs of
    Nothing -> (theory, goal, [])
    Just (trianglePts, insidePt) ->
      let points = trianglePts ++ [insidePt]
          (theory', goal', logBary) = applyBarycentric points theory goal
      in if null logBary then (theory, goal, []) else (theory', goal', logBary)

findInsideBarycentric :: M.Map String Expr -> Maybe ([String], String)
findInsideBarycentric subs =
  case find (\(k, e) -> isCoordVarName k && exprHasBarycentric e) (M.toList subs) of
    Nothing -> Nothing
    Just (coordVar, expr) ->
      let insidePt = drop 1 coordVar
      in case extractTrianglePoints expr of
           Just trianglePts -> Just (trianglePts, insidePt)
           Nothing -> Nothing

extractTrianglePoints :: Expr -> Maybe [String]
extractTrianglePoints expr =
  let pairs = collectBarycentricPairs expr
      lookupPoint v = lookup v pairs
  in case (lookupPoint "ba_u", lookupPoint "ba_v", lookupPoint "ba_w") of
       (Just pu, Just pv, Just pw) ->
         if length (nub [pu, pv, pw]) == 3
         then Just [pu, pv, pw]
         else Nothing
       _ -> Nothing

collectBarycentricPairs :: Expr -> [(String, String)]
collectBarycentricPairs expr =
  case expr of
    Add a b -> collectBarycentricPairs a ++ collectBarycentricPairs b
    Sub a b -> collectBarycentricPairs a ++ collectBarycentricPairs b
    Mul a b -> pairsFromMul a b ++ collectBarycentricPairs a ++ collectBarycentricPairs b
    Div a b -> collectBarycentricPairs a ++ collectBarycentricPairs b
    Pow a _ -> collectBarycentricPairs a
    Sqrt a -> collectBarycentricPairs a
    _ -> []

pairsFromMul :: Expr -> Expr -> [(String, String)]
pairsFromMul (Var v1) (Var v2) =
  case (baryVar v1, coordPointName v2) of
    (Just bary, Just pt) -> [(bary, pt)]
    _ -> case (baryVar v2, coordPointName v1) of
           (Just bary, Just pt) -> [(bary, pt)]
           _ -> []
pairsFromMul _ _ = []

exprHasBarycentric :: Expr -> Bool
exprHasBarycentric expr = any isBarycentricVar (varsInExpr expr)

isBarycentricVar :: String -> Bool
isBarycentricVar v = v == "ba_u" || v == "ba_v" || v == "ba_w"

baryVar :: String -> Maybe String
baryVar v
  | isBarycentricVar v = Just v
  | otherwise = Nothing

isCoordVarName :: String -> Bool
isCoordVarName ('z':'z':'_':_) = False
isCoordVarName ('x':rest) = not (null rest)
isCoordVarName ('y':rest) = not (null rest)
isCoordVarName ('z':rest) = not (null rest)
isCoordVarName _ = False

coordPointName :: String -> Maybe String
coordPointName v =
  case v of
    ('z':'z':'_':_) -> Nothing
    ('x':rest) -> nonEmpty rest
    ('y':rest) -> nonEmpty rest
    ('z':rest) -> nonEmpty rest
    _ -> Nothing
  where
    nonEmpty s = if null s then Nothing else Just s

-- | Detect geometry dimension by examining theory constraints and substitutions
-- INTELLIGENCE: If all z-coordinates are constrained to 0, it's 2D geometry
detectDimension :: M.Map String Expr -> Theory -> Formula -> GeometryDimension
detectDimension subs theory goal =
  let
    allFormulas = goal : theory
    allVars = nub (concatMap varsInFormula allFormulas ++ M.keys subs)
    zVars = [ v | v <- allVars, isZCoordVarName v ]

    -- Check if all z-variables are explicitly set to 0 (in theory or substitutions)
    allZerosAre0 = all (\zv -> hasConstraint zv (Const 0) theory || hasSubstitution zv (Const 0) subs) zVars

    -- If we have z-variables and they're all 0, it's 2D
    -- If we have no z-variables at all, assume 2D
    hasZVars = not (null zVars)
  in
    if hasZVars && allZerosAre0 then Dimension2D
    else if not hasZVars then Dimension2D  -- No z-vars means 2D
    else DimensionUnknown

  where
    isZCoordVarName v = isCoordVarName v && take 1 v == "z"

    hasConstraint var val formulas =
      any (\f -> case f of
                   Eq (Var v) e -> v == var && e == val
                   Eq e (Var v) -> v == var && e == val
                   _ -> False) formulas

    hasSubstitution var val subsMap =
      case M.lookup var subsMap of
        Just e -> e == val
        Nothing -> False

-- | Extract simple substitutions from theory
-- PATTERNS:
--   (= x c) where c is a constant → x ↦ c
--   (= x 0) → x ↦ 0
--   (= (+ x c1) c2) → x ↦ c2 - c1
extractSubstitutions :: Theory -> M.Map String Expr
extractSubstitutions theory =
  M.fromList $ mapMaybe extractSub theory
  where
    extractSub :: Formula -> Maybe (String, Expr)
    extractSub (Eq (Var v) e) | isConstantExpr e = Just (v, e)
    extractSub (Eq e (Var v)) | isConstantExpr e = Just (v, e)
    -- Variable-to-variable equality: y = x → y ↦ x (substitute later variable with earlier)
    extractSub (Eq (Var v1) (Var v2)) | v1 > v2 = Just (v1, Var v2)  -- Replace "y" with "x"
    extractSub (Eq (Var v1) (Var v2)) | v1 < v2 = Just (v2, Var v1)  -- Replace "z" with "x"
    -- Pattern: (* c x) = 0 → x ↦ 0 (for non-zero constant c)
    extractSub (Eq (Mul (Const c) (Var v)) (Const 0)) | c /= 0 = Just (v, Const 0)
    extractSub (Eq (Mul (Var v) (Const c)) (Const 0)) | c /= 0 = Just (v, Const 0)
    extractSub (Eq (Const 0) (Mul (Const c) (Var v))) | c /= 0 = Just (v, Const 0)
    extractSub (Eq (Const 0) (Mul (Var v) (Const c))) | c /= 0 = Just (v, Const 0)
    extractSub _ = Nothing

    -- Check if expression contains only constants (no variables)
    isConstantExpr :: Expr -> Bool
    isConstantExpr (Const _) = True
    isConstantExpr (IntConst _) = True
    isConstantExpr (Add e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Sub e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Mul e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Div e1 e2) = isConstantExpr e1 && isConstantExpr e2
    isConstantExpr (Pow e _) = isConstantExpr e
    isConstantExpr _ = False

-- | For 2D geometry, add z-coordinate = 0 substitutions
addZeroZCoordinates :: Theory -> Formula -> M.Map String Expr -> M.Map String Expr
addZeroZCoordinates theory goal subs =
  let
    allVars = nub $ concatMap varsInFormula (goal : theory)
    zVars = [ v | v <- allVars, isCoordVarName v && take 1 v == "z" ]
    zeroSubs = [(zv, Const 0) | zv <- zVars, not (M.member zv subs)]
  in M.union subs (M.fromList zeroSubs)

-- | Apply substitutions to a formula
applySubstitutionsFormula :: M.Map String Expr -> Formula -> Formula
applySubstitutionsFormula subs = mapFormula (applySubstitutionsExpr subs)

-- | Apply substitutions to an expression
-- Note: Midpoint and Parallel are now expanded at the :assume level, not here
applySubstitutionsExpr :: M.Map String Expr -> Expr -> Expr
applySubstitutionsExpr subs = go
  where
    go (Var v) = case M.lookup v subs of
                   Just e -> e
                   Nothing -> Var v
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Sub e1 e2) = Sub (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (Div e1 e2) = Div (go e1) (go e2)
    go (Mod e1 e2) = Mod (go e1) (go e2)
    go (Pow e n) = Pow (go e) n
    go (Sqrt e) = Sqrt (go e)
    go (Sin e) = Sin (go e)
    go (Cos e) = Cos (go e)
    go (Tan e) = Tan (go e)
    go (Asin e) = Asin (go e)
    go (Acos e) = Acos (go e)
    go (Atan e) = Atan (go e)
    go (Dist2 p1 p2) =
      -- Expand dist2 to coordinate form so substitutions can be applied
      let x1 = Var ("x" ++ p1); y1 = Var ("y" ++ p1); z1 = Var ("z" ++ p1)
          x2 = Var ("x" ++ p2); y2 = Var ("y" ++ p2); z2 = Var ("z" ++ p2)
          dx = Sub (go x1) (go x2)
          dy = Sub (go y1) (go y2)
          dz = Sub (go z1) (go z2)
      in Add (Add (Mul dx dx) (Mul dy dy)) (Mul dz dz)
    go (Collinear p1 p2 p3) =
      -- Expand collinear to coordinate form (determinant)
      Collinear p1 p2 p3  -- TODO: expand if needed
    go (Dot p1 p2 p3 p4) =
      -- Expand dot product to coordinate form
      Dot p1 p2 p3 p4  -- TODO: expand if needed
    go (Circle p c r) =
      -- Expand circle to distance form
      let distSq = go (Dist2 p c)
          radSq = Pow (go r) 2
      in Sub distSq radSq
    go (Midpoint p1 p2 p3) = Midpoint p1 p2 p3  -- Keep as-is, expanded at assume level
    go (Perpendicular p1 p2 p3 p4) = Perpendicular p1 p2 p3 p4
    go (Parallel p1 p2 p3 p4) = Parallel p1 p2 p3 p4  -- Keep as-is, expanded at assume level
    go (AngleEq2D p1 p2 p3 p4 p5 p6) = AngleEq2D p1 p2 p3 p4 p5 p6
    go (Determinant rows) = Determinant (map (map go) rows)
    go e = e  -- Constants, IntConst, IntVar

-- | Map a function over all expressions in a formula
mapFormula :: (Expr -> Expr) -> Formula -> Formula
mapFormula f (Eq e1 e2) = Eq (f e1) (f e2)
mapFormula f (Le e1 e2) = Le (f e1) (f e2)
mapFormula f (Lt e1 e2) = Lt (f e1) (f e2)
mapFormula f (Ge e1 e2) = Ge (f e1) (f e2)
mapFormula f (Gt e1 e2) = Gt (f e1) (f e2)
mapFormula f (Divides e1 e2) = Divides (f e1) (f e2)
mapFormula f (And f1 f2) = And (mapFormula f f1) (mapFormula f f2)
mapFormula f (Or f1 f2) = Or (mapFormula f f1) (mapFormula f f2)
mapFormula f (Not form) = Not (mapFormula f form)
mapFormula f (Forall qvs form) = Forall qvs (mapFormula f form)
mapFormula f (Exists qvs form) = Exists qvs (mapFormula f form)

-- | Simplify theory by removing tautologies and redundant constraints
simplifyTheory :: Theory -> Theory
simplifyTheory theory =
  let
    -- Remove obvious tautologies like (= 0 0), (= x x)
    notTautology (Eq e1 e2) = e1 /= e2
    notTautology (Le (Const c1) (Const c2)) = c1 > c2  -- Keep only non-trivial
    notTautology _ = True

    -- Remove duplicates
    simplified = nub $ filter notTautology theory
  in simplified

