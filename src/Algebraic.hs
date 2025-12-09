module Algebraic where

import Data.Ratio
import qualified Sturm as S

-- | Algebraic Number
-- Represents a real root of a square-free integer polynomial P in an isolating interval (a, b].
-- P is represented as [Rational] (coefficients).
-- The interval (lower, upper] contains exactly one root.
data Algebraic
  = Exact Rational              -- Rational number
  | RootOf S.UPoly (Rational, Rational) -- Polynomial + Interval (a, b]
  deriving (Show)

-- | Check if an algebraic number is actually rational
isRational :: Algebraic -> Maybe Rational
isRational (Exact r) = Just r
isRational _ = Nothing

-- | Smart constructor: RootOf
-- Assumes poly is square-free and interval isolates a single root.
mkAlgebraic :: S.UPoly -> (Rational, Rational) -> Algebraic
mkAlgebraic p (a, b)
  | a == b = Exact a
  -- Check if endpoints are roots (rational roots)
  | S.evalPoly p a == 0 = Exact a
  | S.evalPoly p b == 0 = Exact b
  | otherwise = RootOf p (a, b)

-- | Refine the interval of an algebraic number by one step (bisection)
refine :: Algebraic -> Algebraic
refine (Exact r) = Exact r
refine (RootOf p (a, b)) =
  let mid = (a + b) / 2
      valMid = S.evalPoly p mid
  in if valMid == 0 
     then Exact mid
     else 
       let valA = S.evalPoly p a
       in if valA * valMid < 0
          then RootOf p (a, mid)
          else RootOf p (mid, b)

-- | Approximate algebraic number to a rational with precision epsilon
approximate :: Rational -> Algebraic -> Rational
approximate _ (Exact r) = r
approximate eps alg@(RootOf _ (a, b))
  | b - a < eps = (a + b) / 2
  | otherwise = approximate eps (refine alg)

instance Eq Algebraic where
  (Exact a) == (Exact b) = a == b
  (Exact a) == (RootOf p _) = S.evalPoly p a == 0
  (RootOf p _) == (Exact b) = S.evalPoly p b == 0
  x@(RootOf p1 (a1, b1)) == y@(RootOf p2 (a2, b2))
    | b1 <= a2 || b2 <= a1 = False -- Disjoint
    | otherwise =
        -- Check if they share a factor (common root)
        let g = S.uPolyGCD p1 p2
        in if S.degree g == 0
           then False -- No common roots
           else
             -- Check if the shared factor has a root in the intersection
             let low = max a1 a2
                 high = min b1 b2
             in S.rootsInInterval (S.sturmSequence g) low high > 0

instance Ord Algebraic where
  compare x y = 
    case (x, y) of
      (Exact a, Exact b) -> compare a b
      (Exact a, RootOf _ (b, c)) -> 
         if a <= b then LT else if a >= c then GT else compareExactRoot a y
      (RootOf _ (a, b), Exact c) -> 
         if b <= c then LT else if a >= c then GT else compareRootExact x c
      (RootOf _ (a1, b1), RootOf _ (a2, b2)) ->
         if b1 <= a2 then LT
         else if b2 <= a1 then GT
         else refineAndCompare x y

-- Refine until disjoint intervals or equality detected
compareExactRoot :: Rational -> Algebraic -> Ordering
compareExactRoot r alg@(RootOf p (a, b))
  | r <= a = LT
  | r >= b = GT
  | S.evalPoly p r == 0 = EQ
  | otherwise = compareExactRoot r (refine alg)

compareRootExact :: Algebraic -> Rational -> Ordering
compareRootExact alg r = case compareExactRoot r alg of LT -> GT; GT -> LT; EQ -> EQ

refineAndCompare :: Algebraic -> Algebraic -> Ordering
refineAndCompare x y
  | x == y = EQ
  | otherwise = 
      let x' = refine x
          y' = refine y
          (lx, ux) = bounds x'
          (ly, uy) = bounds y'
      in if ux <= ly then LT
         else if uy <= lx then GT
         else refineAndCompare x' y'

bounds :: Algebraic -> (Rational, Rational)
bounds (Exact r) = (r, r)
bounds (RootOf _ b) = b

data Sign = Negative | Zero | Positive deriving (Show, Eq)

-- | Determine the sign of a univariate polynomial at an algebraic number
signOfUPolyAt :: S.UPoly -> Algebraic -> Sign
signOfUPolyAt q (Exact r) = 
  let val = S.evalPoly q r
  in if val > 0 then Positive else if val < 0 then Negative else Zero
signOfUPolyAt q alg@(RootOf p (a, b)) =
  -- Check if q vanishes at the root (common factor)
  let g = S.uPolyGCD p q
  in if S.degree g > 0 && S.rootsInInterval (S.sturmSequence g) a b > 0
     then Zero
     else refineSign q alg

refineSign :: S.UPoly -> Algebraic -> Sign
refineSign q alg@(RootOf _ (a, b)) =
  let va = S.evalPoly q a
      vb = S.evalPoly q b
  in if va > 0 && vb > 0 then Positive
     else if va < 0 && vb < 0 then Negative
     else refineSign q (refine alg)
refineSign _ (Exact _) = error "Unreachable: refineSign called on Exact"
