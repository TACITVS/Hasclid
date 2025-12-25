{-# LANGUAGE DeriveGeneric #-}

module BuchbergerOpt
  ( SelectionStrategy(..)
  , MonomialOrder
  , buchbergerOptimized
  , buchbergerWithStrategy
  , buchbergerWithStrategyT
  , reduce
  , sPoly
  , interreduceBasis
  ) where

import Expr
import Timeout
import Error (TimeoutErrorType(..))
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import Data.List (nub, minimumBy, delete, sortBy)
import Data.Ord (comparing)
import Numeric.Natural
import Control.Monad.IO.Class (liftIO)

-- =============================================
-- Term Ordering Type
-- =============================================

-- | A monomial ordering function (e.g., GrevLex, Lex, etc.)
type MonomialOrder = Monomial -> Monomial -> Ordering

-- =============================================
-- Selection Strategies
-- =============================================

data SelectionStrategy
  = NormalStrategy    -- Select by total degree (standard)
  | SugarStrategy     -- Select by "sugar" (degree + homogenization)
  | MinimalStrategy   -- Select minimal degree pairs first
  deriving (Eq, Show)

-- =============================================
-- Critical Pair Data Structure
-- =============================================

-- A critical pair (S-polynomial waiting to be computed)
data CriticalPair = CriticalPair
  { pairPolys :: (Poly, Poly)      -- The two polynomials
  , pairLCM :: Monomial            -- LCM of leading monomials
  , pairDegree :: Natural          -- Total degree of LCM
  } deriving (Show, Eq)

-- =============================================
-- Buchberger's Criteria for Eliminating Pairs
-- =============================================

-- | Criterion 1: Coprime leading monomials
criterion1 :: MonomialOrder -> Poly -> Poly -> Bool
criterion1 ord f g =
  case (getLeadingTermByOrder ord f, getLeadingTermByOrder ord g) of
    (Just (ltF, _), Just (ltG, _)) ->
      monomialGCD ltF ltG == monomialOne
    _ -> False

-- | Criterion 2: Chain Criterion (Buchberger's Product Criterion)
criterion2 :: MonomialOrder -> CriticalPair -> [Poly] -> [CriticalPair] -> Bool
criterion2 ord pair _ _ =
  case pairPolys pair of
    (f, g) ->
      case (getLeadingTermByOrder ord f, getLeadingTermByOrder ord g) of
        (Just (ltF, _), Just (ltG, _)) ->
          let lcm = pairLCM pair
              product = monomialMul ltF ltG
          in lcm == product
        _ -> False

-- =============================================
-- Critical Pair Creation and Selection
-- =============================================

-- Create a critical pair from two polynomials
makeCriticalPair :: MonomialOrder -> Poly -> Poly -> Maybe CriticalPair
makeCriticalPair ord f g =
  case (getLeadingTermByOrder ord f, getLeadingTermByOrder ord g) of
    (Just (ltF, _), Just (ltG, _)) ->
      let lcm = monomialLCM ltF ltG
          deg = monomialDegree lcm
      in Just $ CriticalPair (f, g) lcm deg
    _ -> Nothing

monomialDegree :: Monomial -> Natural
monomialDegree (Monomial m) = M.foldl (+) 0 m

-- | Check if a polynomial is safe to add to basis (prevent memory explosion)
-- Set extremely large limits for "Commercial Quality" performance
isSafePoly :: Poly -> Bool
isSafePoly (Poly m)
  | M.null m = True
  | otherwise =
      let numTerms = M.size m
          maxDeg = maximum (0 : map (monomialDegree . fst) (M.toList m))
      in numTerms < 50000 && maxDeg < 50

-- Select next pair based on strategy
selectPair :: SelectionStrategy -> [CriticalPair] -> Maybe (CriticalPair, [CriticalPair])
selectPair _ [] = Nothing
selectPair strategy pairs =
  let selected = case strategy of
        NormalStrategy  -> minimumBy (comparing pairDegree) pairs
        SugarStrategy   -> minimumBy (comparing pairDegree) pairs
        MinimalStrategy -> minimumBy (comparing pairDegree) pairs
      remaining = filter (/= selected) pairs
  in Just (selected, remaining)

-- =============================================
-- Optimized Buchberger Algorithm
-- =============================================

-- | Optimized Buchberger with selection strategy and criteria
-- Returns a REDUCED Groebner Basis.
buchbergerWithStrategy :: MonomialOrder -> SelectionStrategy -> [Poly] -> [Poly]
buchbergerWithStrategy ord strategy polys =
  let initial = filter (/= polyZero) polys
      -- Check if any initial polynomials are already too large
      _ = if any (not . isSafePoly) initial
          then error "Initial basis contains polynomials that are too large"
          else ()
      initialPairs = generatePairs ord initial
      rawBasis = go ord initial initialPairs []
  in interreduceBasis ord rawBasis
  where
    go :: MonomialOrder -> [Poly] -> [CriticalPair] -> [CriticalPair] -> [Poly]
    go _ basis [] _ = basis  -- No more pairs, done!
    go cmp basis pairs processed =
      case selectPair strategy pairs of
        Nothing -> basis
        Just (pair, remaining) ->
          let (f, g) = pairPolys pair
          in
            -- Apply Buchberger criteria
            if criterion1 cmp f g || criterion2 cmp pair basis processed
            then
              -- Skip this pair (criteria say it's useless)
              go cmp basis remaining (pair : processed)
            else
              -- Compute S-polynomial and reduce
              let s = sPoly cmp f g
              in if not (isSafePoly s)
                 then
                   -- S-polynomial too large, skip this pair
                   go cmp basis remaining (pair : processed)
                 else
                   let r = reduce cmp s basis
                   in if r == polyZero
                      then
                        -- Remainder is zero, continue
                        go cmp basis remaining (pair : processed)
                      else if not (isSafePoly r)
                      then
                        -- Polynomial too large, skip to prevent memory exhaustion
                        go cmp basis remaining (pair : processed)
                      else
                        -- Non-zero remainder, add to basis and generate new pairs
                        let newBasis = nub (r : basis)
                            newPairs = [(r, b) | b <- basis, b /= r]
                            newCriticalPairs = concatMap (\(a,b) -> maybe [] (:[]) (makeCriticalPair cmp a b)) newPairs
                        in go cmp newBasis (remaining ++ newCriticalPairs) (pair : processed)

-- | Inter-reduce a Groebner basis to make it minimal and unique.
-- 1. Minimal: No leading term divides another.
-- 2. Reduced: Every polynomial is reduced modulo all others.
interreduceBasis :: MonomialOrder -> [Poly] -> [Poly]
interreduceBasis ord polys =
  let minimal = makeMinimal ord (filter (/= polyZero) polys)
  in map (\p -> reduce ord p (delete p minimal)) minimal

-- | Step 1 of inter-reduction: remove polynomials whose LT is divisible by another's LT
makeMinimal :: MonomialOrder -> [Poly] -> [Poly]
makeMinimal ord polys =
  let withLT = map (\p -> (fst (getLeadingTermByOrderUnsafe ord p), p)) polys
      -- Sort by degree (ascending) so we keep smaller reducers
      sorted = sortBy (comparing (monomialDegree . fst)) withLT
      minimalWithLT = foldl (\acc (lt, p) ->
        if any (\(ltAcc, _) -> monomialDiv lt ltAcc /= Nothing) acc
        then acc -- divisible by a smaller LT, skip
        else (lt, p) : acc) [] sorted
  in map snd minimalWithLT

getLeadingTermByOrderUnsafe :: MonomialOrder -> Poly -> (Monomial, Rational)
getLeadingTermByOrderUnsafe ord p =
  case getLeadingTermByOrder ord p of
    Just res -> res
    Nothing -> error "Leading term requested from zero polynomial"

generatePairs :: MonomialOrder -> [Poly] -> [CriticalPair]
generatePairs cmp bs =
  concatMap (\(f, g) -> maybe [] (:[]) (makeCriticalPair cmp f g))
            [(f, g) | f <- bs, g <- bs, f /= g]

-- | Timeout-aware version of Buchberger algorithm
buchbergerWithStrategyT :: MonomialOrder -> SelectionStrategy -> [Poly] -> TimeoutM [Poly]
buchbergerWithStrategyT ord strategy polys =
  let initial = filter (/= polyZero) polys
      initialPairs = generatePairs ord initial
  in go ord initial initialPairs []
  where
    go :: MonomialOrder -> [Poly] -> [CriticalPair] -> [CriticalPair] -> TimeoutM [Poly]
    go _ basis [] _ = return (interreduceBasis ord basis)
    go cmp basis pairs processed = do
      throwTimeoutError BuchbergerTimeout  -- Throws if timeout exceeded
      case selectPair strategy pairs of
          Nothing -> return (interreduceBasis ord basis)
          Just (pair, remaining) ->
            let (f, g) = pairPolys pair
            in
              if criterion1 cmp f g || criterion2 cmp pair basis processed
              then go cmp basis remaining (pair : processed)
              else
                let s = sPoly cmp f g
                    r = reduce cmp s basis
                in
                  if r == polyZero
                  then go cmp basis remaining (pair : processed)
                  else
                    let newBasis = nub (r : basis)
                        newPairs = [(r, b) | b <- basis, b /= r]
                        newCriticalPairs = concatMap (\(a,b) -> maybe [] (:[]) (makeCriticalPair cmp a b)) newPairs
                    in go cmp newBasis (remaining ++ newCriticalPairs) (pair : processed)

-- | Optimized Buchberger with default strategy (Normal) and Lex order
buchbergerOptimized :: [Poly] -> [Poly]
buchbergerOptimized = buchbergerWithStrategy compare NormalStrategy

-- =============================================
-- Polynomial Reduction & S-Poly
-- =============================================

-- 1. Multivariate Polynomial Reduction (Division)
reduce :: MonomialOrder -> Poly -> [Poly] -> Poly
reduce ord p fs
  | p == polyZero = polyZero
  | otherwise = case findDivisor ord p fs of
      Just (f, mQuot, cQuot) ->
          let subTerm = polyMul (polyMul f (Poly (M.singleton mQuot 1))) (polyFromConst cQuot)
          in reduce ord (polySub p subTerm) fs
      Nothing ->
          case getLeadingTermByOrder ord p of
            Just (ltM, ltC) ->
              let rest = polySub p (Poly (M.singleton ltM ltC))
                  reducedRest = reduce ord rest fs
              in polyAdd (Poly (M.singleton ltM ltC)) reducedRest
            Nothing -> p

  where
    findDivisor :: MonomialOrder -> Poly -> [Poly] -> Maybe (Poly, Monomial, Rational)
    findDivisor cmp poly divisors =
      case getLeadingTermByOrder cmp poly of
        Nothing -> Nothing
        Just (ltP, cP) ->
            let candidates = [ (f, mDiv, cP / cF)
                             | f <- divisors
                             , Just (ltF, cF) <- [getLeadingTermByOrder cmp f]
                             , Just mDiv <- [monomialDiv ltP ltF]
                             ]
            in case candidates of
                 (c:_) -> Just c
                 []    -> Nothing

-- 2. S-Polynomial
sPoly :: MonomialOrder -> Poly -> Poly -> Poly
sPoly ord f g =
  case (getLeadingTermByOrder ord f, getLeadingTermByOrder ord g) of
    (Just (ltF, cF), Just (ltG, cG)) ->
      let lcmM = monomialLCM ltF ltG
      in case (monomialDiv lcmM ltF, monomialDiv lcmM ltG) of
           (Just mF, Just mG) ->
             let factF = polyMul (Poly (M.singleton mF 1)) (polyFromConst (1 / cF))
                 factG = polyMul (Poly (M.singleton mG 1)) (polyFromConst (1 / cG))
                 term1 = polyMul factF f
                 term2 = polyMul factG g
             in polySub term1 term2
           _ -> polyZero
    _ -> polyZero
