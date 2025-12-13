{-# LANGUAGE DeriveGeneric #-}

module Positivity.Numerical
  ( checkSOSNumeric
  , PolyD(..)
  , fromPoly
  , reconstructPoly
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (sortBy, foldl', nub)
import TermOrder (compareMonomials, TermOrder(..))
import Data.Ratio (numerator, denominator)
import Numeric.Natural (Natural)
import Positivity.SDP (solveSDP, GramMatrix)

-- =============================================
-- 1. Double-Precision Polynomials
-- =============================================

data PolyD = PolyD (M.Map Monomial Double) deriving (Show, Eq)

-- | Convert symbolic Poly to PolyD, substituting parameters
fromPoly :: M.Map String Double -> Poly -> PolyD
fromPoly params (Poly m) =
  let terms = [ (mRes, val) 
              | (mono, c) <- M.toList m
              , let (mRes, fac) = evalMonomial params mono
              , let val = fromRational c * fac
              ]
  in PolyD $ M.fromListWith (+) terms

evalMonomial :: M.Map String Double -> Monomial -> (Monomial, Double)
evalMonomial params (Monomial m) =
  let (vars, ps) = M.partitionWithKey (\k _ -> not (M.member k params)) m
      fac = product [ (params M.! k) ^ (fromIntegral e :: Int) | (k, e) <- M.toList ps ]
  in (Monomial vars, fac)

-- =============================================
-- 2. Basic Polynomial Operations (Double)
-- =============================================

isZeroD :: PolyD -> Bool
isZeroD (PolyD m) = M.null m || all (\x -> abs x < 1e-9) (M.elems m)

addD :: PolyD -> PolyD -> PolyD
addD (PolyD a) (PolyD b) = PolyD $ M.filter (\x -> abs x > 1e-9) $ M.unionWith (+) a b

subD :: PolyD -> PolyD -> PolyD
subD (PolyD a) (PolyD b) = PolyD $ M.filter (\x -> abs x > 1e-9) $ M.unionWith (-) a b

mulD :: PolyD -> PolyD -> PolyD
mulD (PolyD a) (PolyD b) =
  let terms = [ (monomialMul m1 m2, c1 * c2) | (m1, c1) <- M.toList a, (m2, c2) <- M.toList b ]
  in PolyD $ M.fromListWith (+) terms

scaleD :: PolyD -> Double -> PolyD
scaleD (PolyD m) s = PolyD $ M.map (*s) m

getLeadingTermD :: PolyD -> Maybe (Monomial, Double)
getLeadingTermD (PolyD m) =
  if M.null m then Nothing
  else 
    let sorted = sortBy (\(m1,_) (m2,_) -> compareMonomials GrevLex m2 m1) (M.toList m)
    in case sorted of
         (x:_) -> Just x
         [] -> Nothing

-- =============================================
-- 3. Numerical Cholesky Decomposition
-- =============================================

-- | Returns list of squares (as PolyD) if successful
checkSOSNumeric :: M.Map String Double -> Poly -> Maybe [PolyD]
checkSOSNumeric params p = 
  let pd = fromPoly params p
  in case cholesky pd [] of
       Just res -> Just res
       Nothing -> checkSOSSDP pd

checkSOSSDP :: PolyD -> Maybe [PolyD]
checkSOSSDP (PolyD m) =
  let 
      -- Generate basis monomials: All monomials with degree <= half max degree?
      -- Simplified: Take all monomials M such that M^2 appears in P with positive coeff?
      -- Better: Newton polytope.
      -- Heuristic: All monomials m present in P such that deg(m) <= deg(P)/2
      allMonos = M.keys m
      maxDeg = maximum (map degree allMonos)
      halfDeg = maxDeg `div` 2
      basis = filter (\x -> degree x <= halfDeg) allMonos -- Very rough heuristic
      
      -- Convert PolyD to Map Monomial Double
      target = m
  in case solveSDP target basis of
       Just q -> extractSquares q basis
       Nothing -> Nothing

degree :: Monomial -> Integer
degree (Monomial vars) = sum (map fromIntegral (M.elems vars))

extractSquares :: GramMatrix -> [Monomial] -> Maybe [PolyD]
extractSquares _ _ = Nothing -- Placeholder for Gram Matrix decomposition

cholesky :: PolyD -> [PolyD] -> Maybe [PolyD]
cholesky p acc
  | isZeroD p = Just (reverse acc)
  | otherwise =
      case getLeadingTermD p of
        Nothing -> Just (reverse acc)
        Just (ltM, ltC) ->
          if ltC < -1e-9 || not (isSquareMono ltM)
          then Nothing -- Failed (negative leading term or not even power)
          else
            let
               m = sqrtMono ltM
               c = ltC
               
               -- Greedy pivot: Take all terms divisible by m
               (divisible, _) = partitionD p m
               
               -- Divide by m to get 'quotient'
               quotient = divMonomialD divisible m
               
               -- ltPoly = c * m
               -- X = quotient - c*m
               -- Square base = sqrt(c) * m + (1/(2*sqrt(c))) * X
               -- S = (base)^2
               
               sqrtC = sqrt (max 0 c) -- clamp to 0 if tiny negative
               inv2SqrtC = if sqrtC < 1e-9 then 0 else 1.0 / (2.0 * sqrtC)
               
               ltPart = PolyD (M.singleton m sqrtC)
               
               -- Remove LT from quotient to get X
               xPart = subD quotient (PolyD (M.singleton m c))
               
               correction = scaleD xPart inv2SqrtC
               
               base = addD ltPart correction
               square = mulD base base
               
               newP = subD p square
            in cholesky newP (base : acc)

partitionD :: PolyD -> Monomial -> (PolyD, PolyD)
partitionD (PolyD m) base =
  let (yes, no) = M.partitionWithKey (\k _ -> isDivisible k base) m
  in (PolyD yes, PolyD no)

divMonomialD :: PolyD -> Monomial -> PolyD
divMonomialD (PolyD m) base =
  PolyD $ M.mapKeys (\k -> case monomialDiv k base of Just r -> r; Nothing -> k) m

-- Helpers imported logic locally to avoid circular deps if needed, but reusing Expr is fine.
isSquareMono :: Monomial -> Bool
isSquareMono (Monomial m) = all even (M.elems m)

sqrtMono :: Monomial -> Monomial
sqrtMono (Monomial m) = Monomial (M.map (`div` 2) m)

isDivisible :: Monomial -> Monomial -> Bool
isDivisible (Monomial a) (Monomial b) = M.isSubmapOfBy (<=) b a

-- =============================================
-- 4. Reconstruction (Double -> Rational[s])
-- =============================================

-- | Reconstruct a symbolic polynomial from PolyD using 's' as sqrt(3)
reconstructPoly :: String -> Double -> PolyD -> Poly
reconstructPoly paramName paramVal (PolyD m) =
  let polyList = [ liftTerm mono c | (mono, c) <- M.toList m ]
  in foldl' polyAdd polyZero polyList
  where
    liftTerm mono c =
      let (a, b) = findLinearRel c paramVal
          -- a * mono + b * s * mono
          pA = polyFromMonomial mono a
          sMono = monomialMul mono (Monomial (M.singleton paramName 1))
          pB = polyFromMonomial sMono b
      in polyAdd pA pB

    findLinearRel :: Double -> Double -> (Rational, Rational)
    findLinearRel val s =
      -- Try to find val ~= a + b*s
      -- Heuristic: iterate b, check if val - b*s is close to rational
      let range = [-6 .. 6] :: [Integer]
          candidates = [ (toRational b, toRationalApprox (val - fromIntegral b * s)) 
                       | b <- range
                       , let rem = val - fromIntegral b * s
                       , isSimpleRational rem
                       ]
      in case candidates of
           (res:_) -> (snd res, fst res) -- (a, b) -> a + b*s
           [] -> (toRationalApprox val, 0)

    isSimpleRational :: Double -> Bool
    isSimpleRational x =
      let r = toRationalApprox x
      in abs (fromRational r - x) < 1e-5 && denominator r < 100

toRationalApprox :: Double -> Rational
toRationalApprox x =
  toRational (round (x * 1000000) :: Integer) / 1000000

-- | Helper to create Poly from Monomial/Coeff
polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = Poly (M.singleton m c)