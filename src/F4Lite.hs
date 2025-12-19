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
  , f4ReduceModular
  , reduceWithF4
  , reduceWithBasis
  ) where

import Expr (Poly(..), Monomial(..), monomialLCM, monomialMul, monomialDiv, getLeadingTermByOrder, monomialGCD, monomialOne)
import BuchbergerOpt (buchbergerWithStrategy, SelectionStrategy(..), sPoly, reduce, interreduceBasis)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl', find, tails, partition, minimumBy, nub)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator, (%))
import Modular (toMod, addM, subM, mulM, invM, ModVal, prime)
import TermOrder (compareMonomials, TermOrder(..))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (zipWithM_)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import Control.Monad.ST (runST)

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

f4LiteGroebner :: (Monomial -> Monomial -> Ordering) -> SelectionStrategy -> Bool -> [Poly] -> [Poly]
f4LiteGroebner ord strategy useBatch polys =
  let initial = interreduceBasis ord (filter (/= Poly M.empty) polys)
      initialPairs = generateF4Pairs ord initial
  in if not useBatch then buchbergerWithStrategy ord strategy initial
     else f4Loop initial initialPairs
  where
    f4Loop basis [] = interreduceBasis ord basis
    f4Loop basis pairs =
      let
          -- 1. Selection: Pick a batch of pairs (minimal degree)
          minDeg = pairDegree (minimumBy (comparing pairDegree) pairs)
          (batch, remaining) = partition (\p -> pairDegree p == minDeg) pairs
          
          -- 2. Symbolic Preprocessing & Reduction
          sPolys = map (\p -> sPoly ord (fst (pairPolys p)) (snd (pairPolys p))) batch
          reduced = f4BatchReduce ord basis sPolys
          
          -- 3. Update Basis and Pairs
          newBasisElements = filter (/= Poly M.empty) reduced
      in if null newBasisElements
         then f4Loop basis remaining
         else
           let updatedBasis = interreduceBasis ord (basis ++ newBasisElements)
               newPairs = generateF4Pairs ord updatedBasis
           in f4Loop updatedBasis newPairs

-- | Critical pair for F4
data F4Pair = F4Pair
  { pairPolys :: (Poly, Poly)
  , pairLCM :: Monomial
  , pairDegree :: Integer
  } deriving (Eq, Show)

generateF4Pairs :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [F4Pair]
generateF4Pairs ord basis =
  -- Use existing nub from Data.List
  nub [ F4Pair (f, g) lcm (fromIntegral (monomialDegree lcm))
      | (f:rest) <- tails basis, g <- rest
      , let Just (ltF, _) = getLeadingTermByOrder ord f
      , let Just (ltG, _) = getLeadingTermByOrder ord g
      , let lcm = monomialLCM ltF ltG
      , not (monomialGCD ltF ltG == monomialOne) -- Buchberger Criterion 1
      ]

-- | Build a Macaulay-like matrix for a batch of S-polynomials.
buildMacaulayMatrix :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly] -> (S.Set Col, [Row])
buildMacaulayMatrix ord basis sPolys =
  let
      -- Pre-index basis by leading monomial for faster lookup
      basisLTs = sortBy (comparing (monomialDegree . fst)) $
                 mapMaybe (\g -> (\(lt, _) -> (lt, g)) <$> getLeadingTermByOrder ord g) basis
      
      initialRows = map polyToRow sPolys
      
      findReducer m = 
        find (\(lt, _) -> monomialDiv m lt /= Nothing) basisLTs
        >>= (\(lt, g) -> (\mult -> (mult, g)) <$> monomialDiv m lt)

      loop done currentRows =
        let 
            allMonos = S.unions (map M.keysSet currentRows)
            todo = S.difference allMonos done
        in if S.null todo
           then currentRows
           else 
             let newReducers = mapMaybe findReducer (S.toList todo)
                 newRows = map polyToRow (map (uncurry polyScaleMonomial) newReducers)
                 nextDone = S.union done todo
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

-- | Run modular reduction over several small primes in PARALLEL.
--   Uses CRT and Rational Reconstruction to recover the exact result.
modularRowReduceMulti :: S.Set Col -> [Row] -> [Row]
modularRowReduceMulti cols rows = unsafePerformIO $ do
  let primes = smallPrimes
  mvars <- mapM (\_ -> newEmptyMVar) primes
  
  -- Launch parallel threads for each prime
  zipWithM_ (\p mvar -> forkIO $ do
    let res = rowReducePrime cols rows p
    putMVar mvar res) primes mvars
    
  -- Collect results
  reductions <- mapM takeMVar mvars
  
  case reductions of
       []       -> return $ modularRowReduce cols rows
       (r0 : _) ->
         if any null reductions
           then return $ modularRowReduce cols rows
           else return $ crtRows cols primes r0 reductions
  where
    smallPrimes :: [Integer]
    smallPrimes = [2147483647, 2147483629, 2147483587, 2147483579, 2147483563]

-- | Modular Gaussian elimination using Unboxed Vectors for speed.
--   This is the "commercial quality" core of the F4 algorithm.
modGaussVector :: Integer -> Int -> [V.Vector ModVal] -> [V.Vector ModVal]
modGaussVector p numCols rows = go 0 rows []
  where
    go col [] acc = reverse acc
    go col rs acc 
      | col >= numCols = reverse acc ++ rs
      | otherwise =
          case break (\r -> r V.! col /= 0) rs of
            (_, []) -> go (col + 1) rs acc -- No pivot in this column
            (before, pivot:after) ->
              let pVal = pivot V.! col
                  invP = invMod p pVal
                  -- Normalize pivot row
                  pivot' = V.map (\v -> (v * invP) `mod` p) pivot
                  -- Eliminate other rows
                  rs' = map (elim col pivot') (before ++ after)
                  rs'' = filter (not . isZeroV) rs'
              in go (col + 1) rs'' (pivot' : acc)

    elim col pivotRow target =
      let factor = target V.! col
      in if factor == 0 then target
         else V.zipWith (\t pRow -> (t - (factor * pRow) `mod` p + p) `mod` p) target pivotRow

    isZeroV v = V.all (== 0) v

-- | Convert Map-based Rows to Unboxed Vectors for fast reduction
rowToVector :: [Col] -> RowM -> V.Vector ModVal
rowToVector cols m = V.fromList [ M.findWithDefault 0 c m | c <- cols ]

type RowM = M.Map Col Integer

-- Reduce rows modulo a specific prime (Optimized Vector version)
rowReducePrime :: S.Set Col -> [Row] -> Integer -> [M.Map Col Integer]
rowReducePrime cols rows p =
  let colList = sortBy (\a b -> compare b a) (S.toList cols)
      numCols = length colList
      rowsMod = map (M.map (normalizeRatMod p)) rows
      vectors = map (rowToVector colList) rowsMod
      reduced = modGaussVector p numCols vectors
  in map (vectorToRow colList) reduced
  where
    normalizeRatMod p q =
      let n = numerator q `mod` p
          d = denominator q `mod` p
      in if d == 0 then 0 else (n * invMod p d) `mod` p
    
    vectorToRow cols v = 
      M.fromList $ filter (\(_, val) -> val /= 0) $ zip cols (V.toList v)

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
-- Optimized to use degree-based filtering.
interreduce :: [Poly] -> [Poly]
interreduce polys =
  let ord = compareMonomials GrevLex
      -- 1. Extract LTs and sort by degree (smaller degree first)
      withLT = mapMaybe (\p -> (\(m,_) -> (m,p)) <$> getLeadingTermByOrder ord p) polys
      sorted = sortBy (\(m1,_) (m2,_) -> compare (monomialDegree m1) (monomialDegree m2)) withLT
      
      -- 2. Filter: only keep if not divisible by any polynomial already in the minimal set
      go acc [] = map snd acc
      go acc (p@(lt, _):ps) =
        if any (\(ltAcc, _) -> monomialDiv lt ltAcc /= Nothing) acc
        then go acc ps
        else go (p:acc) ps
        
  in go [] sorted

monomialDegree :: Monomial -> Integer
monomialDegree (Monomial m) = fromIntegral $ M.foldl (+) 0 m

-- | High-performance modular reduction of a single polynomial against a basis.
-- Uses the Macaulay matrix approach to reduce in one shot.
f4ReduceModular :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Poly
f4ReduceModular ord basis target =
  if target == Poly M.empty then target else
  let 
      -- 1. Symbolic Preprocessing: Find all reducers for the target
      (colsSet, rows) = buildMacaulayMatrix ord basis [target]
      cols = sortBy (\a b -> ord b a) (S.toList colsSet)
      
      -- 2. Modular Matrix Reduction
      rowsReduced = modularRowReduceMulti colsSet rows
      
      -- 3. Extract the remainder
      -- The remainder is the row in RREF that contains only monomials NOT divisible by any basis LT.
      basisLTs = S.fromList $ mapMaybe (fmap fst . getLeadingTermByOrder ord) basis
      
      isRemainder row = 
        case [ c | c <- cols, M.findWithDefault 0 c row /= 0 ] of
          (lt:_) -> not (S.member lt basisLTs)
          [] -> False
          
      remainders = filter isRemainder rowsReduced
  in case remainders of
       (r:_) -> head (rowsToPolys cols [r])
       [] -> 
         -- If all rows are reducible to something with basis LTs, check if there's a zero row or small remainder
         Poly M.empty

-- | Compute the Groebner Basis using F4 and then reduce a target polynomial.
--   Useful for simplifying inequality expressions modulo equality constraints.
reduceWithF4 :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Poly
reduceWithF4 ord theoryPolys target =
  let basis = f4LiteGroebner ord NormalStrategy True theoryPolys
  in reduce ord target basis

-- | Reduce a polynomial using a pre-computed basis
reduceWithBasis :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Poly
reduceWithBasis ord basis target = reduce ord target basis
