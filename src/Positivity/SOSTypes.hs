module Positivity.SOSTypes
  ( SOSCertificate(..)
  , SOSPattern(..)
  , trySOSHeuristic
  ) where

import Expr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ratio
import Data.List (sort, nub)

-- | Certificate that a polynomial is a sum of squares
-- p(x) = sum_i (q_i(x))^2 where q_i are the SOS components
data SOSCertificate = SOSCertificate
  { sosPoly      :: Poly              -- Original polynomial
  , sosComponents :: [Poly]           -- Polynomials q_i such that p = sum (q_i)^2
  , sosPattern   :: SOSPattern        -- Pattern used for decomposition
  , sosWitness   :: Maybe String      -- Optional human-readable witness
  } deriving (Eq, Show)

-- | Common SOS patterns
data SOSPattern
  = TrivialSquare          -- p = q^2
  | SumOfSquares           -- p = q1^2 + q2^2 + ... + qn^2
  | CompletedSquare        -- p = (a+b)^2 - 2ab completed to (a-b)^2 + 2ab
  | CauchySchwarz          -- (a^2+b^2)(c^2+d^2) >= (ac+bd)^2
  | TriangleInequality     -- Special pattern for triangle inequality
  | AM_GM                  -- Arithmetic-Geometric mean inequality
  | Weitzenbock            -- Geometric inequality pattern
  | Custom String          -- Other patterns
  deriving (Eq, Show)

-- | Try to decompose polynomial using heuristic pattern matching
-- This is a simplified SOS checker that doesn't require SDP solving
trySOSHeuristic :: Poly -> [Formula] -> Maybe SOSCertificate
trySOSHeuristic poly _theory =
  -- Try different heuristic patterns in order
  case tryTrivialSquare poly of
    Just cert -> Just cert
    Nothing -> case trySimpleSumOfSquares poly of
      Just cert -> Just cert
      Nothing -> case tryCompletedSquare poly of
        Just cert -> Just cert
        Nothing -> case tryTriangleInequalityPattern poly of
          Just cert -> Just cert
          Nothing -> Nothing

-- | Check if polynomial is already a perfect square
tryTrivialSquare :: Poly -> Maybe SOSCertificate
tryTrivialSquare p =
  case extractSquareRoot p of
    Just q -> Just $ SOSCertificate p [q] TrivialSquare (Just "Perfect square")
    Nothing -> Nothing

-- | Try to extract square root of a polynomial
extractSquareRoot :: Poly -> Maybe Poly
extractSquareRoot (Poly m)
  | Map.size m == 1 =
      case Map.toList m of
        [(mon, coeff)] ->
          if isSquareMonomial mon && isSquareRational coeff
          then Just (Poly $ Map.singleton (sqrtMonomial mon) (sqrtRational coeff))
          else Nothing
        _ -> Nothing
  | otherwise = Nothing

isSquareMonomial :: Monomial -> Bool
isSquareMonomial (Monomial vars) = all even (Map.elems vars)

sqrtMonomial :: Monomial -> Monomial
sqrtMonomial (Monomial vars) = Monomial (Map.map (`div` 2) vars)

isSquareRational :: Rational -> Bool
isSquareRational r = r >= 0 && let n = numerator r; d = denominator r
                                in isSquareInt n && isSquareInt d

isSquareInt :: Integer -> Bool
isSquareInt n = n >= 0 && let s = Expr.integerSqrt n in s * s == n

sqrtRational :: Rational -> Rational
sqrtRational r = let n = numerator r; d = denominator r
                 in Expr.integerSqrt n % Expr.integerSqrt d

-- | Check if polynomial is a simple sum of squares: x^2 + y^2 + z^2 + ...
trySimpleSumOfSquares :: Poly -> Maybe SOSCertificate
trySimpleSumOfSquares (Poly m) =
  let terms = Map.toList m
      squares = [Poly (Map.singleton mon coeff) | (mon, coeff) <- terms, coeff > 0, isSquareMonomial mon]
  in if length squares == Map.size m && all (\(Poly tm) -> Map.size tm == 1) squares
     then Just $ SOSCertificate
            (Poly m)
            [case extractSquareRoot sq of
               Just q -> q
               Nothing -> sq  -- Shouldn't happen given our check above
            | sq <- squares]
            SumOfSquares
            (Just $ "Sum of " ++ show (length squares) ++ " squares")
     else Nothing

-- | Try to complete the square: a^2 + b^2 - 2ab = (a-b)^2
tryCompletedSquare :: Poly -> Maybe SOSCertificate
tryCompletedSquare p =
  case matchPattern_a2_plus_b2_minus_2ab p of
    Just (a, b) -> Just $ SOSCertificate
                     p
                     [subPoly a b]  -- (a-b)
                     CompletedSquare
                     (Just "Completed square (a-b)^2")
    Nothing -> case matchPattern_a2_plus_b2_plus_c2_minus_2ab_minus_2ac_minus_2bc p of
      Just (a, b, c) -> Just $ SOSCertificate
                          p
                          [subPoly (subPoly a b) c]  -- (a-b-c)
                          CompletedSquare
                          (Just "Completed square (a-b-c)^2")
      Nothing -> Nothing

-- | Match pattern: a^2 + b^2 - 2ab (which equals (a-b)^2)
matchPattern_a2_plus_b2_minus_2ab :: Poly -> Maybe (Poly, Poly)
matchPattern_a2_plus_b2_minus_2ab _ = Nothing  -- Simplified stub

-- | Match pattern for triangle inequality after expansion
-- After squaring: a^2 + b^2 + c^2 - 2ab - 2ac - 2bc
-- This should be >= 0, but it's not directly SOS
-- However, we can verify it's always non-negative
matchPattern_a2_plus_b2_plus_c2_minus_2ab_minus_2ac_minus_2bc :: Poly -> Maybe (Poly, Poly, Poly)
matchPattern_a2_plus_b2_plus_c2_minus_2ab_minus_2ac_minus_2bc _ = Nothing  -- Simplified stub

-- | Try to match triangle inequality pattern
-- sqrt(a) + sqrt(b) >= sqrt(c) becomes a + b + 2sqrt(ab) >= c after squaring
-- Then (a+b-c)^2 >= 4ab, which expands to a^2 + b^2 + c^2 + 2ab - 2ac - 2bc >= 4ab
-- Simplified: a^2 + b^2 + c^2 - 2ab - 2ac - 2bc >= 0
tryTriangleInequalityPattern :: Poly -> Maybe SOSCertificate
tryTriangleInequalityPattern poly =
  -- Check if polynomial matches the expanded triangle inequality pattern
  -- For simplicity, we'll use a known certificate for this pattern
  -- In production, this would use more sophisticated pattern matching
  Nothing  -- Stub for now

-- Helper: Add two polynomials
addPoly :: Poly -> Poly -> Poly
addPoly (Poly m1) (Poly m2) = Poly (Map.unionWith (+) m1 m2)

-- Helper: Subtract two polynomials
subPoly :: Poly -> Poly -> Poly
subPoly (Poly m1) (Poly m2) = Poly (Map.unionWith (+) m1 (Map.map negate m2))

-- Helper: Multiply two polynomials
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly m1) (Poly m2) =
  let terms = [(combineMonomial mon1 mon2, c1 * c2)
              | (mon1, c1) <- Map.toList m1
              , (mon2, c2) <- Map.toList m2]
  in Poly (Map.fromListWith (+) terms)

-- Helper: Multiply two monomials
combineMonomial :: Monomial -> Monomial -> Monomial
combineMonomial (Monomial m1) (Monomial m2) =
  Monomial $ Map.unionWith (+) m1 m2
