module Interval
  ( Interval(..)
  , width
  , mid
  , member
  , intersect
  , hull
  , scale
  , addI, subI, mulI, divI, powI
  , sinI, cosI, tanI
  , asinI, acosI, atanI
  , fromRationalI
  , toRationalI
  , fullRange
  ) where

import Data.Ratio
import Data.List (sort)

-- | Interval [lower, upper]
data Interval = I !Double !Double
  deriving (Show, Eq, Ord)

-- =============================================
-- IEEE 754 Directed Rounding
-- =============================================

-- | Next representable double greater than x
nextUp :: Double -> Double
nextUp x
  | isNaN x || isInfinite x = x
  | x == 0.0 = 5.0e-324 -- Smallest positive denormal (2^-1074)
  | x < 0.0 && x > -5.0e-324 = -0.0 -- Approach 0 from negative
  | otherwise =
      let (m, e) = decodeFloat x
      in encodeFloat (m + 1) e


-- | Next representable double less than x
nextDown :: Double -> Double
nextDown x = negate (nextUp (-x))

-- | Add with rounding towards +inf
addUp :: Double -> Double -> Double
addUp a b = nextUp (a + b)

-- | Add with rounding towards -inf
addDown :: Double -> Double -> Double
addDown a b = nextDown (a + b)

-- | Sub with rounding towards +inf
subUp :: Double -> Double -> Double
subUp a b = nextUp (a - b)

-- | Sub with rounding towards -inf
subDown :: Double -> Double -> Double
subDown a b = nextDown (a - b)

-- | Mul with rounding towards +inf
mulUp :: Double -> Double -> Double
mulUp a b = nextUp (a * b)

-- | Mul with rounding towards -inf
mulDown :: Double -> Double -> Double
mulDown a b = nextDown (a * b)

-- | Div with rounding towards +inf
divUp :: Double -> Double -> Double
divUp a b = nextUp (a / b)

-- | Div with rounding towards -inf
divDown :: Double -> Double -> Double
divDown a b = nextDown (a / b)

-- =============================================
-- Interval Construction
-- =============================================

-- | Construct interval from rational with directed rounding
fromRationalI :: Rational -> Interval
fromRationalI r =
  let d = fromRational r :: Double
      exact = toRational d == r
  in if exact
     then I d d
     else I (nextDown d) (nextUp d)

-- | Convert back to rational (approximate, usually center)
toRationalI :: Interval -> Rational
toRationalI (I l u) = toRational ((l + u) / 2)

width :: Interval -> Double
width (I l u) = u - l

mid :: Interval -> Double
mid (I l u) = (l + u) / 2

member :: Double -> Interval -> Bool
member x (I l u) = x >= l && x <= u

intersect :: Interval -> Interval -> Maybe Interval
intersect (I l1 u1) (I l2 u2) =
  let l = max l1 l2
      u = min u1 u2
  in if l <= u then Just (I l u) else Nothing

hull :: Interval -> Interval -> Interval
hull (I l1 u1) (I l2 u2) = I (min l1 l2) (max u1 u2)

scale :: Double -> Interval -> Interval
scale s (I l u) =
  if s >= 0
  then I (mulDown s l) (mulUp s u)
  else I (mulDown s u) (mulUp s l)

-- =============================================
-- Interval Arithmetic
-- =============================================

addI :: Interval -> Interval -> Interval
addI (I a b) (I c d) = I (addDown a c) (addUp b d)

subI :: Interval -> Interval -> Interval
subI (I a b) (I c d) = I (subDown a d) (subUp b c)

mulI :: Interval -> Interval -> Interval
mulI (I a b) (I c d) =
  let p1 = mulDown a c; p2 = mulDown a d; p3 = mulDown b c; p4 = mulDown b d
      q1 = mulUp a c;   q2 = mulUp a d;   q3 = mulUp b c;   q4 = mulUp b d
  in I (minimum [p1, p2, p3, p4]) (maximum [q1, q2, q3, q4])

divI :: Interval -> Interval -> Interval
divI (I a b) (I c d)
  | c > 0 || d < 0 =
      let p1 = divDown a c; p2 = divDown a d; p3 = divDown b c; p4 = divDown b d
          q1 = divUp a c;   q2 = divUp a d;   q3 = divUp b c;   q4 = divUp b d
      in I (minimum [p1, p2, p3, p4]) (maximum [q1, q2, q3, q4])
  | otherwise = I (-1/0) (1/0) -- Division by zero interval

powI :: Interval -> Int -> Interval
powI (I a b) n
  | even n =
      if a >= 0 then I (powDown a n) (powUp b n)
      else if b <= 0 then I (powDown b n) (powUp a n)
      else I 0 (max (powUp a n) (powUp b n))
  | otherwise = I (powDown a n) (powUp b n)
  where
    powDown x k = nextDown (x ^ k)
    powUp x k = nextUp (x ^ k)

-- =============================================
-- Transcendental Functions (Widened)
-- =============================================

safeFunc :: (Double -> Double) -> Interval -> Interval
safeFunc f (I a b) =
  let samples = [f a, f b, f ((a+b)/2)]
      minV = minimum samples
      maxV = maximum samples
      -- Relative padding + absolute floor
      padding = 1e-14 * (abs maxV + 1.0)
  in I (minV - padding) (maxV + padding)

sinI :: Interval -> Interval
sinI (I a b) =
  let base = safeFunc sin (I a b)
  in if width (I a b) > 2*pi
     then I (-1) 1
     else I (max (-1) (minI base)) (min 1 (maxI base))

cosI :: Interval -> Interval
cosI (I a b) =
  let base = safeFunc cos (I a b)
  in if width (I a b) > 2*pi
     then I (-1) 1
     else I (max (-1) (minI base)) (min 1 (maxI base))

tanI :: Interval -> Interval
tanI (I a b) = safeFunc tan (I a b)

asinI :: Interval -> Interval
asinI (I a b) = safeFunc asin (I (max (-1) a) (min 1 b))

acosI :: Interval -> Interval
acosI (I a b) = safeFunc acos (I (max (-1) a) (min 1 b))

atanI :: Interval -> Interval
atanI = safeFunc atan

minI, maxI :: Interval -> Double
minI (I l _) = l
maxI (I _ u) = u

fullRange :: Interval
fullRange = I (-1/0) (1/0)
