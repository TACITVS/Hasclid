{-|
Module      : F4Lite
Description : F4-style Groebner backend (delegates to optimized Buchberger)

This module defines a pluggable Groebner backend entry point. It implements
a "Lite" version of the F4 algorithm:
1. Iterative Symbolic Preprocessing (building the Macaulay matrix).
2. Matrix Reduction (currently using exact Rational Row Echelon Form for correctness).
3. Filtering of redundant results.
4. Delegation to Buchberger for final inter-reduction and basis completion.

The modular reduction path is preserved but currently unused due to lack of
Rational Reconstruction.
-}
module F4Lite
  ( f4LiteGroebner
  , buildMacaulayMatrix
  , Row
  , Col
  , modularRowReduce
  , modularRowReduceMulti
  , rowEchelonReduce
  , rowsToPolys
  , f4BatchReduce
  , reduceWithF4
  , reduceWithBasis
  ) where

import Expr (Poly(..), Monomial(..), monomialLCM, monomialMul, monomialDiv, getLeadingTermByOrder)
import BuchbergerOpt (buchbergerWithStrategy, SelectionStrategy(..), sPoly, reduce)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl')
import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator, (%))
import Modular (toMod, addM, subM, mulM, invM, ModVal, prime)
import TermOrder (compareMonomials, TermOrder(..))

-- A small local extended gcd for modular inverse
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (a, 1, 0)
gcdExt a b =
  let (g, x1, y1) = gcdExt b (a `mod` b)
      x = y1
      y = x1 - (a `div` b) * y1
  in (g, x, y)

type Col = Monomial
type Row = M.Map Col Rational

-- | F4-lite Groebner wrapper. As a conservative first step, we can optionally
--   run a modular batch reduction of initial S-polynomials, then finish with
--   the existing optimized Buchberger engine. If batch mode is off or yields
--   nothing, we fall back to Buchberger directly.
f4LiteGroebner :: (Monomial -> Monomial -> Ordering) -> SelectionStrategy -> Bool -> [Poly] -> [Poly]
f4LiteGroebner ord strat useBatch polys =
  let pairs    = [(f, g) | f <- polys, g <- polys, f /= g]
      sPolys   = map (uncurry (sPoly ord)) pairs
      reduced  = if useBatch then f4BatchReduce ord polys sPolys else []
      seed     = polys ++ reduced
  in if null reduced then buchbergerWithStrategy ord strat polys
     else buchbergerWithStrategy ord strat seed

-- | Build a Macaulay-like matrix for a batch of S-polynomials.
--   Iteratively adds reducers for any monomial appearing in the rows that is
--   divisible by a leading term of the basis (Symbolic Preprocessing).
buildMacaulayMatrix :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly] -> (S.Set Col, [Row])
buildMacaulayMatrix ord reducers sPolys =
  let
      -- Initial set of rows from S-polynomials
      initialRows = map polyToRow sPolys
      
      -- Helper to find a reducer for a monomial
      -- Returns (multiplier, scaledReducerPoly)
      findReducer :: Monomial -> Maybe (Monomial, Poly) 
      findReducer m = 
        let candidates = 
              [ (mult, g)
              | g <- reducers
              , Just (ltG, _) <- [getLeadingTermByOrder ord g]
              , Just mult <- [monomialDiv m ltG]
              ]
        in case candidates of
             [] -> Nothing
             ((mult, g):_) -> Just (mult, polyScaleMonomial mult g)

      -- Iteratively add reducers until no new monomials can be reduced
      loop :: S.Set Monomial -> [Row] -> [Row]
      loop done currentRows =
        let 
            -- All monomials in current rows
            allMonos = S.unions (map M.keysSet currentRows)
            
            -- Monomials that haven't been processed yet
            todo = S.difference allMonos done
        in if S.null todo
           then currentRows
           else 
             let 
                 -- Find reducers for all new monomials
                 -- We use S.toList to process them. 
                 -- Optimization: We could prioritize high-degree monomials?
                 newReducers = mapMaybe findReducer (S.toList todo)
                 
                 -- Convert reducers to rows
                 newRows = map polyToRow (map snd newReducers)
                 
                 -- Update done set
                 nextDone = S.union done todo
                 
                 -- Filter out rows that are already present (by value) to avoid simple dupes? 
                 -- For now, just append. The matrix reduction handles dupes.
                 -- Optim: Check if newRows introduces ANYTHING new (monomial-wise or row-wise).
                 -- But simpler to just run until todo is empty.
             in if null newRows
                then loop nextDone currentRows
                else loop nextDone (currentRows ++ newRows)

      finalRows = loop S.empty initialRows
      cols = S.unions (map M.keysSet finalRows)
  in (cols, finalRows)
  where
    polyToRow (Poly m) = m

polyScaleMonomial :: Monomial -> Poly -> Poly
polyScaleMonomial m (Poly mm) =
  Poly $ M.mapKeys (monomialMul m) mm

-- | Modular row-reduction over F_p for sparse rows.
--   Returns reduced rows lifted back to Rational.
modularRowReduce :: S.Set Col -> [Row] -> [Row]
modularRowReduce cols rows =
  let colList = sortBy (\a b -> compare b a) (S.toList cols)
      rowsMod = map (M.map (toMod . normalizeRat)) rows
      reduced = modGauss colList rowsMod
  in map (M.map (\c -> fromIntegral c)) reduced
  where
    -- Normalize a rational to an integer representative modulo prime.
    -- This is simplistic: we multiply by the denominator's inverse mod prime.
    normalizeRat q =
      let n = numerator q
          d = denominator q
          n' = toMod n
          d' = toMod d
      in if d == 1
           then n'
           else if d' == 0 then 0 else mulM n' (invM d')

-- | Run modular reduction over several small primes and attempt a trivial
-- Chinese remainder / majority-vote reconstruction. This is a placeholder
-- that simply keeps the first successful prime reduction to stay cheap.
modularRowReduceMulti :: S.Set Col -> [Row] -> [Row]
modularRowReduceMulti cols rows =
  let primes = smallPrimes
      reductions = map (rowReducePrime cols rows) primes
  in case reductions of
       []       -> modularRowReduce cols rows
       (r0 : _) ->
         if any null reductions
           then modularRowReduce cols rows
           else crtRows cols primes r0 reductions
  where
    smallPrimes :: [Integer]
    smallPrimes = [2147483647, 2147483629, 2147483587] -- distinct large primes

-- Reduce rows modulo a specific prime (returns integer residues)
rowReducePrime :: S.Set Col -> [Row] -> Integer -> [M.Map Col Integer]
rowReducePrime cols rows p =
  let colList = sortBy (\a b -> compare b a) (S.toList cols)
      rowsMod = map (M.map (normalizeRatMod p)) rows
      reduced = modGaussPrime p colList rowsMod
  in reduced
  where
    normalizeRatMod :: Integer -> Rational -> Integer
    normalizeRatMod p q =
      let n = numerator q `mod` p
          d = denominator q `mod` p
      in if d == 0 then 0 else (n * invMod p d) `mod` p

-- Modular Gaussian elimination with explicit prime
modGaussPrime :: Integer -> [Col] -> [M.Map Col Integer] -> [M.Map Col Integer]
modGaussPrime p cols rows = go cols rows []
  where
    go [] _ acc = reverse acc
    go _ [] acc = reverse acc
    go (c:cs) rs acc =
      case break (\r -> M.findWithDefault 0 c r /= 0) rs of
        (_, []) -> go cs rs acc
        (before, pivot:after) ->
          let pCoeff = M.findWithDefault 0 c pivot
              invP = invMod p pCoeff
              pivot' = M.map (\v -> (v * invP) `mod` p) pivot
              eliminated = map (elim c pivot') (before ++ after)
              eliminated' = filter (not . M.null) eliminated
          in go cs eliminated' (pivot':acc)

    elim c pivotRow target =
      let factor = M.findWithDefault 0 c target
      in if factor == 0 then target
         else M.filter (/=0) $
                M.unionWith addMod target (M.map (\v -> subMod 0 ((factor * v) `mod` p)) pivotRow)

    addMod a b = (a + b) `mod` p
    subMod a b = (a - b) `mod` p

-- Combine per-prime reductions using a simple CRT on matching row/column positions.
crtRows :: S.Set Col -> [Integer] -> [M.Map Col Integer] -> [[M.Map Col Integer]] -> [Row]
crtRows cols primes _ allReductions =
  let colList = sortBy (\a b -> compare b a) (S.toList cols)
      bigMod = product primes
      pivotKey row = case [ idx | (idx, c) <- zip [0..] colList, M.findWithDefault 0 c row /= 0 ] of
                       (k:_) -> k
                       []    -> maxBound :: Int
      -- Align rows across primes by sorting on the pivot column, to keep CRT consistent.
      reductionsAligned = map (sortBy (\a b -> compare (pivotKey a) (pivotKey b))) allReductions
      minRows = minimum (map length reductionsAligned)
      redTrimmed = map (take minRows) reductionsAligned
      rowsCombined = [ combineRow i | i <- [0..minRows-1] ]
      combineRow i =
        let residuesForCol c = [ M.findWithDefault 0 c (r !! i) | r <- redTrimmed ]
            coeff c =
              let residues = residuesForCol c
                  allZero  = all (== 0) residues
                  centered m v = if v > m `div` 2 then v - m else v
              in if allZero then 0 else centered bigMod (crt residues primes)
        in M.fromList [ (c, coeff c) | c <- colList, let v = coeff c, v /= 0 ]
  in map (normalizeIntegerRow bigMod) rowsCombined

-- Reduce a row of integer residues by Rational Reconstruction.
-- This recovers the true rational coefficients from the CRT image.
normalizeIntegerRow :: Integer -> M.Map Col Integer -> Row
normalizeIntegerRow modulus m
  | M.null m = M.empty
  | otherwise =
      let reconstructed = M.map (rationalReconstruction modulus) m
      in if any (== Nothing) (M.elems reconstructed)
         then M.empty -- Reconstruction failed for some coeff -> discard row
         else M.map (\(Just r) -> r) reconstructed

-- Rational Reconstruction: find n/d = a (mod m)
-- using Extended Euclidean Algorithm.
-- Bounds: |n|, |d| < sqrt(m/2)
rationalReconstruction :: Integer -> Integer -> Maybe Rational
rationalReconstruction m a =
  let
    limit = floor (sqrt (fromIntegral m / 2 :: Double))
    
    go r0 r1 t0 t1
      | r1 <= limit = Just (fromIntegral r1 % fromIntegral t1)
      | otherwise =
          let q = r0 `div` r1
              r2 = r0 - q * r1
              t2 = t0 - q * t1
          in go r1 r2 t1 t2
  in
    -- Normalize a to [0, m-1]
    let a' = a `mod` m
    in case go m a' 0 1 of
         Just r -> if abs (denominator r) <= limit 
                   then Just r 
                   else Nothing
         Nothing -> Nothing

-- Chinese remainder for a list of (residue) with corresponding primes (assumed coprime).
crt :: [Integer] -> [Integer] -> Integer
crt residues moduli =
  let bigMod = product moduli
      pairs = zip residues moduli
      combine acc (r,m) =
        let m' = bigMod `div` m
            inv = invMod m m'
            term = r * m' * inv
        in (acc + term) `mod` bigMod
  in foldl combine 0 pairs

-- Modular inverse with explicit modulus
invMod :: Integer -> Integer -> Integer
invMod p a =
  let (g,x,_) = gcdExt a p
      x' = x `mod` p
  in if g == 1 then x' else error "No modular inverse"

-- Simple modular Gaussian elimination
modGauss :: [Col] -> [M.Map Col ModVal] -> [M.Map Col ModVal]
modGauss cols rows = go cols rows []
  where
    go [] _ acc = reverse acc
    go _ [] acc = reverse acc
    go (c:cs) rs acc =
      case break (\r -> M.findWithDefault 0 c r /= 0) rs of
        (_, []) -> go cs rs acc
        (before, pivot:after) ->
          let pCoeff = M.findWithDefault 0 c pivot
              invP = invM pCoeff
              pivot' = M.map (\v -> mulM v invP) pivot
              eliminated = map (elim c pivot') (before ++ after)
              eliminated' = filter (not . M.null) eliminated
          in go cs eliminated' (pivot':acc)

    elim c pivotRow target =
      let factor = M.findWithDefault 0 c target
      in if factor == 0 then target
         else M.filter (/=0) $
                M.unionWith addM target (M.map (\v -> subM 0 (mulM factor v)) pivotRow)

-- | Rational row-echelon reduction for sparse rows (Maps from monomial -> coeff).
rowEchelonReduce :: (Monomial -> Monomial -> Ordering) -> [Col] -> [Row] -> [Row]
rowEchelonReduce _ cols rows = go rows []
  where
    go [] acc = reverse acc
    go rs acc =
      case pickPivot cols rs of
        Nothing -> reverse acc
        Just (pivotCol, pivotRow, rest) ->
          let pivotCoeff = M.findWithDefault 0 pivotCol pivotRow
              normalized = scaleRow (1 / pivotCoeff) pivotRow
              eliminated = map (eliminate pivotCol normalized) rest
              eliminated' = filter (not . M.null) eliminated
          in go eliminated' (normalized : acc)

    pickPivot :: [Col] -> [Row] -> Maybe (Col, Row, [Row])
    pickPivot _ [] = Nothing
    pickPivot [] _ = Nothing
    pickPivot (c:cs) rs =
      case break (\r -> M.findWithDefault 0 c r /= 0) rs of
        (_, []) -> pickPivot cs rs
        (before, pivot:after) -> Just (c, pivot, before ++ after)

    scaleRow k = M.map (* k)

    eliminate :: Col -> Row -> Row -> Row
    eliminate pivotCol pivotRow target =
      let factor = M.findWithDefault 0 pivotCol target
      in if factor == 0
           then target
           else normalizeRow $ M.filter (/=0) $
                 M.unionWith (+)
                   target
                   (M.map (* (-factor)) pivotRow)

    normalizeRow = id

-- | Convert reduced rows back to polys, given a fixed column order.
rowsToPolys :: [Col] -> [Row] -> [Poly]
rowsToPolys cols rows =
  let toPoly r = Poly $ M.fromList [ (c, coeff) | c <- cols, Just coeff <- [M.lookup c r], coeff /= 0 ]
  in filter (/= Poly M.empty) (map toPoly rows)

-- | Minimal modular batch reducer for S-polys. This is a scaffold: it builds
--   a Macaulay matrix from the given S-polys, reduces it mod p, and lifts the
--   rows back to rational polys. Callers can choose to merge these into the
--   basis; we keep f4LiteGroebner delegating to Buchberger for safety.
f4BatchReduce :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly] -> [Poly]
f4BatchReduce ord reducers sPolys =
  let (colsSet, rows) = buildMacaulayMatrix ord reducers sPolys
      cols = sortBy (\a b -> ord b a) (S.toList colsSet)
      -- USE MODULAR REDUCTION + RECONSTRUCTION instead of Exact Rational RREF
      rowsReduced = modularRowReduceMulti colsSet rows
      polys = rowsToPolys cols rowsReduced
      
      -- Filter out polys whose LT is divisible by any reducer's LT
      usefulPolys = filter (not . isReducibleByReducers ord reducers) polys
  in interreduce usefulPolys

isReducibleByReducers :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Bool
isReducibleByReducers ord reducers p =
  case getLeadingTermByOrder ord p of
    Nothing -> True -- zero is reducible
    Just (lt, _) -> any (\g -> case getLeadingTermByOrder ord g of
                                 Just (ltG, _) -> monomialDiv lt ltG /= Nothing
                                 Nothing -> False) reducers

-- | Simple interreduction: remove leading-term multiples, keep minimal set.
interreduce :: [Poly] -> [Poly]
interreduce polys =
  let ord = compareMonomials GrevLex
      withLT = mapMaybe (\p -> (\(m,_) -> (m,p)) <$> getLeadingTermByOrder ord p) polys
      unique = filter (\(lt,_)-> not (any (\(lt',_) -> lt' /= lt && monomialDiv lt lt' /= Nothing) withLT)) withLT
  in map snd unique

-- | Compute the Groebner Basis using F4 and then reduce a target polynomial.
--   Useful for simplifying inequality expressions modulo equality constraints.
reduceWithF4 :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Poly
reduceWithF4 ord theoryPolys target =
  let basis = f4LiteGroebner ord NormalStrategy True theoryPolys
  in reduce ord target basis

-- | Reduce a polynomial using a pre-computed basis
reduceWithBasis :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Poly
reduceWithBasis ord basis target = reduce ord target basis
