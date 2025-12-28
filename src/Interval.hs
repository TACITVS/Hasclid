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

-- | Construct interval from rational, with slight padding for safety
fromRationalI :: Rational -> Interval
fromRationalI r = 
  let d = fromRational r :: Double
      padding = 1e-10 * (abs d + 1.0)
  in I (d - padding) (d + padding)

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
  then I (s*l) (s*u)
  else I (s*u) (s*l)

-- Arithmetic

addI :: Interval -> Interval -> Interval
addI (I a b) (I c d) = I (a+c) (b+d)

subI :: Interval -> Interval -> Interval
subI (I a b) (I c d) = I (a-d) (b-c)

mulI :: Interval -> Interval -> Interval
mulI (I a b) (I c d) =
  let products = [a*c, a*d, b*c, b*d]
  in I (minimum products) (maximum products)

divI :: Interval -> Interval -> Interval
divI (I a b) (I c d)
  | c > 0 || d < 0 = mulI (I a b) (I (1/d) (1/c))
  | otherwise      = I (-1/0) (1/0) -- Division by zero interval

powI :: Interval -> Int -> Interval
powI (I a b) n
  | even n = 
      if a >= 0 then I (a^n) (b^n)
      else if b <= 0 then I (b^n) (a^n)
      else I 0 (max (a^n) (b^n))
  | otherwise = I (a^n) (b^n)

-- Transcendental (with widening for safety)

safeFunc :: (Double -> Double) -> Interval -> Interval
safeFunc f (I a b) =
  let samples = [f a, f b, f ((a+b)/2)]
      minV = minimum samples
      maxV = maximum samples
      padding = 1e-9 * (abs maxV + 1.0)
  in I (minV - padding) (maxV + padding)

-- Note: These are naive implementations. A real library would check monotonicity and critical points.
-- For a prototype, we check endpoints + mid + monotonicity assumption or simple bounds.

sinI :: Interval -> Interval
sinI (I a b) = 
  -- Check if interval covers any (2k+1)pi/2
  -- Just naive widening for now to prove concept
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
tanI (I a b) = safeFunc tan (I a b) -- Unsafe near pi/2

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
