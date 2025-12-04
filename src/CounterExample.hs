{-# LANGUAGE DeriveGeneric #-}

module CounterExample
  ( findCounterExample
  , CounterExample(..)
  , formatCounterExample
  ) where

import Expr (Formula(..), Expr(..), Poly(..), Monomial(..), Theory, polyFromConst, prettyRational, containsSqrtFormula)
import Prover (buildSubMap, toPolySub, evaluatePoly)
import qualified Data.Map.Strict as M
import Data.Ratio ((%), numerator, denominator)
import Data.List (nub)

-- =============================================
-- Counter-example Data Structure
-- =============================================

data CounterExample = CounterExample
  { assignment :: M.Map String Rational  -- Variable assignments
  , lhsValue :: Rational                 -- LHS evaluated
  , rhsValue :: Rational                 -- RHS evaluated
  , formula :: Formula                   -- Original formula
  } deriving (Show, Eq)

-- =============================================
-- Counter-example Finding
-- =============================================

-- | Find a counter-example for a failed proof
-- Returns Nothing if no counter-example found in reasonable time
findCounterExample :: Theory -> Formula -> Maybe CounterExample
findCounterExample theory formula =
  let
      -- Do not attempt polynomial-based counterexamples when sqrt is present
      hasSqrt = containsSqrtFormula formula || any containsSqrtFormula theory
  in if hasSqrt then Nothing else
  let
      subM = buildSubMap theory
      freeVars = getFreeVariables theory formula

      -- Try simple values first
      simpleAttempts = generateSimpleAssignments freeVars

      -- Then try random/strategic values
      strategicAttempts = generateStrategicAssignments freeVars

      allAttempts = simpleAttempts ++ strategicAttempts
  in
      findFirst (testAssignment subM formula) allAttempts

-- =============================================
-- Assignment Generation
-- =============================================

-- Extract free variables from formula (not bound by theory)
getFreeVariables :: Theory -> Formula -> [String]
getFreeVariables theory formula =
  let
      -- Variables bound by theory (substitutions)
      boundVars = [ v | Eq (Var v) _ <- theory ]

      -- All variables in formula
      formulaVars = case formula of
                      Eq l r -> extractVars l ++ extractVars r
                      Ge l r -> extractVars l ++ extractVars r
                      Gt l r -> extractVars l ++ extractVars r

      -- Free = formula vars - bound vars
      freeVars = nub $ filter (`notElem` boundVars) formulaVars
  in
      freeVars

extractVars :: Expr -> [String]
extractVars (Var x) = [x]
extractVars (Const _) = []
extractVars (Add e1 e2) = extractVars e1 ++ extractVars e2
extractVars (Sub e1 e2) = extractVars e1 ++ extractVars e2
extractVars (Mul e1 e2) = extractVars e1 ++ extractVars e2
extractVars (Div e1 e2) = extractVars e1 ++ extractVars e2
extractVars (Pow e _) = extractVars e
extractVars (IntVar _) = []
extractVars (IntConst _) = []
extractVars (Sqrt e) = extractVars e
extractVars (Dist2 p1 p2) = ["x" ++ p1, "y" ++ p1, "z" ++ p1, "x" ++ p2, "y" ++ p2, "z" ++ p2]
extractVars (Collinear p1 p2 p3) = ["x" ++ p1, "y" ++ p1, "x" ++ p2, "y" ++ p2, "x" ++ p3, "y" ++ p3]
extractVars (Dot a b c d) = concatMap (\p -> ["x" ++ p, "y" ++ p, "z" ++ p]) [a, b, c, d]
extractVars (Circle p c _) = ["x" ++ p, "y" ++ p, "z" ++ p, "x" ++ c, "y" ++ c, "z" ++ c]
extractVars (Midpoint a b m) = concatMap (\p -> ["x" ++ p, "y" ++ p, "z" ++ p]) [a, b, m]
extractVars (Perpendicular a b c d) = concatMap (\p -> ["x" ++ p, "y" ++ p, "z" ++ p]) [a, b, c, d]
extractVars (Parallel a b c d) = concatMap (\p -> ["x" ++ p, "y" ++ p, "z" ++ p]) [a, b, c, d]
extractVars (AngleEq2D a b c d e f) = concatMap (\p -> ["x" ++ p, "y" ++ p]) [a, b, c, d, e, f]
extractVars (AngleEq2DAbs a b c d e f) = concatMap (\p -> ["x" ++ p, "y" ++ p]) [a, b, c, d, e, f]

-- Generate simple test values
simpleValues :: [Rational]
simpleValues = [0, 1, -1, 2, -2, 1%2, -1%2, 3, -3, 1%3, 2%3]

-- Generate simple assignments (try all combinations of simple values)
generateSimpleAssignments :: [String] -> [M.Map String Rational]
generateSimpleAssignments [] = [M.empty]
generateSimpleAssignments vars =
  let
      -- Limit to first 3 variables and 5 values for performance
      limitedVars = take 3 vars
      limitedValues = take 5 simpleValues
  in
      [ M.fromList (zip limitedVars vals)
      | vals <- combinations limitedValues (length limitedVars)
      ]

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations [] _ = []
combinations (x:xs) n = map (x:) (combinations (x:xs) (n-1)) ++ combinations xs n

-- Generate strategic assignments (geometric patterns)
generateStrategicAssignments :: [String] -> [M.Map String Rational]
generateStrategicAssignments vars =
  let
      -- Pattern 1: All zeros
      allZeros = M.fromList [(v, 0) | v <- vars]

      -- Pattern 2: All ones
      allOnes = M.fromList [(v, 1) | v <- vars]

      -- Pattern 3: Sequential (1, 2, 3, ...)
      sequential = M.fromList (zip vars (map fromIntegral [1..]))

      -- Pattern 4: Alternating 0, 1, 0, 1, ...
      alternating = M.fromList (zip vars (cycle [0, 1]))
  in
      [allZeros, allOnes, sequential, alternating]

-- =============================================
-- Assignment Testing
-- =============================================

testAssignment :: M.Map String Poly -> Formula -> M.Map String Rational -> Maybe CounterExample
testAssignment subM formula assignment =
  let
      -- Extend substitution map with assignment
      assignmentPoly = M.map polyFromConst assignment
      fullSubM = M.union assignmentPoly subM

      -- Evaluate LHS and RHS
      (lhs, rhs) = case formula of
                     Eq l r -> (l, r)
                     Ge l r -> (l, r)
                     Gt l r -> (l, r)

      lhsPoly = toPolySub fullSubM lhs
      rhsPoly = toPolySub fullSubM rhs

      -- Convert to constant (if fully evaluated)
      lhsVal = polyToConstant lhsPoly
      rhsVal = polyToConstant rhsPoly
  in
      case (lhsVal, rhsVal) of
        (Just lv, Just rv) ->
          -- Check if this is indeed a counter-example
          case formula of
            Eq _ _ -> if lv /= rv then Just (CounterExample assignment lv rv formula) else Nothing
            Ge _ _ -> if not (lv >= rv) then Just (CounterExample assignment lv rv formula) else Nothing
            Gt _ _ -> if not (lv > rv) then Just (CounterExample assignment lv rv formula) else Nothing
        _ -> Nothing

-- Convert a polynomial to a constant if it's a constant polynomial
polyToConstant :: Poly -> Maybe Rational
polyToConstant (Poly m)
  | M.null m = Just 0
  | M.size m == 1 =
      case M.toList m of
        [(Monomial vm, c)] | M.null vm -> Just c  -- Only constant term
        _ -> Nothing
  | otherwise = Nothing

-- =============================================
-- Utilities
-- =============================================

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst _ [] = Nothing
findFirst f (x:xs) = case f x of
                       Just y -> Just y
                       Nothing -> findFirst f xs

-- =============================================
-- Formatting
-- =============================================

formatCounterExample :: CounterExample -> String
formatCounterExample ce =
  let
      assignmentStr = unlines [ "  " ++ var ++ " = " ++ prettyRational val
                              | (var, val) <- M.toList (assignment ce) ]

      lhsStr = prettyRational (lhsValue ce)
      rhsStr = prettyRational (rhsValue ce)

      (op, verdict) = case formula ce of
                        Eq _ _ -> ("=", "LHS != RHS")
                        Ge _ _ -> (">=", "LHS < RHS")
                        Gt _ _ -> (">", "LHS <= RHS")
  in
      unlines
        [ "COUNTER-EXAMPLE FOUND:"
        , ""
        , "Variable Assignment:"
        , assignmentStr
        , "Evaluation:"
        , "  LHS = " ++ lhsStr
        , "  RHS = " ++ rhsStr
        , "  " ++ lhsStr ++ " " ++ op ++ " " ++ rhsStr ++ " ? FALSE (" ++ verdict ++ ")"
        , ""
        , "This demonstrates the formula does not hold in general."
        ]
