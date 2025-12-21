{-# LANGUAGE DeriveGeneric #-}

module Positivity.Numerical
  ( checkSOSNumeric
  , PolyD(..)
  , fromPoly
  , reconstructPoly
  , safeFromRational
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (sortBy, foldl', nub)
import TermOrder (compareMonomials, TermOrder(..))
import Data.Ratio (numerator, denominator)
import Numeric.Natural (Natural)
import Control.Exception (try, evaluate, ArithException(..))
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

-- =============================================
-- 1. Double-Precision Polynomials (Safe Mode with Log-Scaling)
-- =============================================

data PolyD = PolyD (M.Map Monomial Double) deriving (Show, Eq)

-- Custom integerLog2 for compatibility
integerLog2' :: Integer -> Int
integerLog2' n
  | n <= 0    = 0
  | n < 2     = 0
  | otherwise = 1 + integerLog2' (n `div` 2)

-- | Convert symbolic Poly to PolyD, substituting parameters
fromPoly :: M.Map String Double -> Poly -> PolyD
fromPoly params (Poly m) =
  let 
      termLogs :: [(Monomial, Rational, Double, Double)] 
      termLogs = 
        [ (mono, c, fac, valLog)
        | (mono, c) <- M.toList m
        , c /= 0
        , let (fac, facLog) = evalMonomialLog params mono
        , let cLog = rationalLog (abs c)
        , let valLog = cLog + facLog
        ]

      maxLog = if null termLogs then 0.0 else maximum (map (\(_,_,_,l) -> l) termLogs)
      
      _ = if null termLogs then () else 
          trace ("Numerical SOS: maxLog = " ++ show maxLog ++ ", terms = " ++ show (length termLogs)) ()

      terms = [ (mRes, val) 
              | (mono, c, fac, valLog) <- termLogs
              , let scaledLog = valLog - maxLog
              , scaledLog > -700 
              , let mRes = mono 
              , let valSign = signum (fromRational c) * signum fac
              , let val = valSign * exp scaledLog
              , abs val > 1e-20 
              ]
  in PolyD $ M.fromListWith (+) terms

rationalLog :: Rational -> Double
rationalLog r 
  | r <= 0 = -1e9 -- Should not happen for abs
  | otherwise = 
      let n = numerator r
          d = denominator r
      in integerLogD n - integerLogD d

integerLogD :: Integer -> Double
integerLogD n 
  | n <= 0 = -1e9
  | otherwise = 
      let l2 = integerLog2' n
      in fromIntegral l2 * log 2.0

evalMonomialLog :: M.Map String Double -> Monomial -> (Double, Double)
evalMonomialLog params (Monomial m) =
  let (vars, ps) = M.partitionWithKey (\k _ -> not (M.member k params)) m
      logSum = foldl' (\acc (k, e) -> 
                  let pVal = abs (M.findWithDefault 1.0 k params) 
                      logP = if pVal == 0 then -1e9 else log pVal
                  in acc + fromIntegral e * logP
               ) 0.0 (M.toList ps)
      val = foldl' (\acc (k, e) -> safeMul acc ((M.findWithDefault 1.0 k params) ^ (fromIntegral e :: Int))) 1.0 (M.toList ps)
  in (val, logSum)

-- | Safe fromRational that avoids underflow by clamping tiny values to 0.
safeFromRational :: Rational -> Double
safeFromRational r 
  | r == 0 = 0.0
  | abs (numerator r) * (10^(300::Integer)) < denominator r = 0.0
  | otherwise =
      let d = fromRational r :: Double
      in if isInfinite d || isNaN d then 0.0 else 
         if d /= 0 && abs d < 1e-300 then 0.0 else d

-- | Safe multiplication to avoid underflow
safeMul :: Double -> Double -> Double
safeMul a b =
  let r = a * b
  in if isNaN r || isInfinite r then 0.0
     else if r /= 0 && abs r < 1e-300 then 0.0 else r

evalMonomial :: M.Map String Double -> Monomial -> (Monomial, Double)
evalMonomial params (Monomial m) =
  let (vars, ps) = M.partitionWithKey (\k _ -> not (M.member k params)) m
      fac = foldl' (\acc (k, e) -> safeMul acc ((M.findWithDefault 1.0 k params) ^ (fromIntegral e :: Int))) 1.0 (M.toList ps)
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
  let terms = [ (monomialMul m1 m2, safeMul c1 c2) | (m1, c1) <- M.toList a, (m2, c2) <- M.toList b ]
  in PolyD $ M.fromListWith (+) terms

scaleD :: PolyD -> Double -> PolyD
scaleD (PolyD m) s = PolyD $ M.map (\x -> safeMul x s) m

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
checkSOSNumeric params p = unsafePerformIO $ do
  res <- try $ evaluate $ 
    let pd = fromPoly params p
    in cholesky pd []
  case res of
    Left (Underflow) -> return Nothing
    Left (Overflow) -> return Nothing
    Left _ -> return Nothing
    Right val -> do
      case val of
        Just sqs -> trace ("Numerical SOS: Success! Found " ++ show (length sqs) ++ " squares.") (return val)
        Nothing -> trace "Numerical SOS: Failed (Not SOS)." (return Nothing)

cholesky :: PolyD -> [PolyD] -> Maybe [PolyD]
cholesky p acc
  | isZeroD p = Just (reverse acc)
  | otherwise =
      case getLeadingTermD p of
        Nothing -> Just (reverse acc)
        Just (ltM, ltC) ->
          if ltC < -1e-9 || not (isSquareMono ltM)
          then Nothing 
          else
            let
               m = sqrtMono ltM
               c = ltC
               
               (divisible, _) = partitionD p m
               quotient = divMonomialD divisible m
               
               sqrtC = sqrt (max 0 c) 
               inv2SqrtC = if sqrtC < 1e-9 then 0 else 1.0 / (2.0 * sqrtC)
               
               ltPart = PolyD (M.singleton m sqrtC)
               
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

isSquareMono :: Monomial -> Bool
isSquareMono (Monomial m) = all even (M.elems m)

sqrtMono :: Monomial -> Monomial
sqrtMono (Monomial m) = Monomial (M.map (`div` 2) m)

isDivisible :: Monomial -> Monomial -> Bool
isDivisible (Monomial a) (Monomial b) = M.isSubmapOfBy (<=) b a

-- =============================================
-- 4. Reconstruction (Double -> Rational[s])
-- =============================================

reconstructPoly :: String -> Double -> PolyD -> Poly
reconstructPoly paramName paramVal (PolyD m) =
  let polyList = [ liftTerm mono c | (mono, c) <- M.toList m ]
  in foldl' polyAdd polyZero polyList
  where
    liftTerm mono c =
      let (a, b) = findLinearRel c paramVal
          pA = polyFromMonomial mono a
          sMono = monomialMul mono (Monomial (M.singleton paramName 1))
          pB = polyFromMonomial sMono b
      in polyAdd pA pB

    findLinearRel :: Double -> Double -> (Rational, Rational)
    findLinearRel val s =
      let range = [-6 .. 6] :: [Integer]
          candidates = [ (toRational b, toRationalApprox (val - fromIntegral b * s)) 
                       | b <- range
                       , let rem = val - fromIntegral b * s
                       , isSimpleRational rem
                       ]
      in case candidates of
           (res:_) -> (snd res, fst res) 
           [] -> (toRationalApprox val, 0)

    isSimpleRational :: Double -> Bool
    isSimpleRational x =
      let r = toRationalApprox x
      in abs (fromRational r - x) < 1e-5 && denominator r < 100

toRationalApprox :: Double -> Rational
toRationalApprox x =
  toRational (round (x * 1000000) :: Integer) / 1000000

polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = Poly (M.singleton m c)