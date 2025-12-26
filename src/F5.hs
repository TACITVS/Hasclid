module F5
  (
    f5Groebner,
    f5GroebnerBounded,
    f5Solve,
    f5SolveBounded,
    Signature (..),
    LabeledPoly (..),
    compareSignatures,
    isSyzygy,
    isRewritable,
    Rules,
    polyFromMonomial,  -- Re-exported from Polynomial
    polyIsZero,
    -- Memory bounds
    F5Config (..),
    F5Result (..),
    defaultF5Config,
    conservativeF5Config,
    polyComplexity,
    polyMaxCoeffBits
  )
where

import Expr (Poly (..), Monomial (..), monomialLCM, monomialMul, monomialDiv, getLeadingTermByOrder, monomialGCD, monomialOne, polyAdd, polySub, polyMul, getLeadingTerm, polyScale, polyZero, polyNeg, polyPrimitive)
import Polynomial (fromMonomial)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl', nub, partition, minimumBy, find)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import TermOrder (TermOrder (..), compareMonomials)
import Data.Ratio (numerator, denominator)

-- =============================================================================
-- Memory Bound Configuration
-- =============================================================================

-- | Configuration for memory-bounded F5 computation
data F5Config = F5Config
  { f5MaxDegree :: !Int           -- ^ Maximum allowed polynomial degree
  , f5MaxTerms :: !Int            -- ^ Maximum terms in any polynomial
  , f5MaxBasisSize :: !Int        -- ^ Maximum basis elements before abort
  , f5MaxCoeffBits :: !Int        -- ^ Maximum bits in coefficient numerator/denominator
  , f5MaxIterations :: !Int       -- ^ Maximum loop iterations
  , f5NormalizeEveryN :: !Int     -- ^ Normalize coefficients every N reductions
  } deriving (Eq, Show)

-- | Default configuration (generous limits)
defaultF5Config :: F5Config
defaultF5Config = F5Config
  { f5MaxDegree = 50
  , f5MaxTerms = 10000
  , f5MaxBasisSize = 500
  , f5MaxCoeffBits = 1000
  , f5MaxIterations = 100000
  , f5NormalizeEveryN = 5
  }

-- | Conservative configuration for problematic inputs
conservativeF5Config :: F5Config
conservativeF5Config = F5Config
  { f5MaxDegree = 20
  , f5MaxTerms = 2000
  , f5MaxBasisSize = 100
  , f5MaxCoeffBits = 500
  , f5MaxIterations = 10000
  , f5NormalizeEveryN = 3
  }

-- | Result of bounded F5 computation
data F5Result = F5Result
  { f5Basis :: [Poly]             -- ^ Computed basis (may be partial)
  , f5Completed :: Bool           -- ^ True if computation completed normally
  , f5AbortReason :: Maybe String -- ^ Reason for early termination
  , f5Stats :: F5Stats            -- ^ Statistics
  } deriving (Eq, Show)

-- | Statistics from F5 computation
data F5Stats = F5Stats
  { f5Iterations :: !Int
  , f5MaxDegSeen :: !Int
  , f5MaxTermsSeen :: !Int
  , f5ReductionsPerformed :: !Int
  } deriving (Eq, Show)

emptyStats :: F5Stats
emptyStats = F5Stats 0 0 0 0

-- | Measure polynomial complexity (total degree)
polyDegree :: Poly -> Int
polyDegree (Poly m)
  | M.null m = 0
  | otherwise = maximum $ map monomialDegree (M.keys m)

-- | Count terms in polynomial
polyTermCount :: Poly -> Int
polyTermCount (Poly m) = M.size m

-- | Get maximum bits in any coefficient
polyMaxCoeffBits :: Poly -> Int
polyMaxCoeffBits (Poly m)
  | M.null m = 0
  | otherwise = maximum $ map coeffBits (M.elems m)
  where
    coeffBits r = max (intBits (abs (numerator r))) (intBits (abs (denominator r)))
    intBits 0 = 0
    intBits n = ceiling (logBase 2 (fromIntegral (abs n) + 1) :: Double)

-- | Overall complexity metric
polyComplexity :: Poly -> (Int, Int, Int)  -- (degree, terms, coeff bits)
polyComplexity p = (polyDegree p, polyTermCount p, polyMaxCoeffBits p)

-- | Check if polynomial exceeds bounds
exceedsBounds :: F5Config -> Poly -> Maybe String
exceedsBounds cfg p
  | deg > f5MaxDegree cfg = Just $ "Degree " ++ show deg ++ " exceeds limit " ++ show (f5MaxDegree cfg)
  | terms > f5MaxTerms cfg = Just $ "Term count " ++ show terms ++ " exceeds limit " ++ show (f5MaxTerms cfg)
  | bits > f5MaxCoeffBits cfg = Just $ "Coefficient size " ++ show bits ++ " bits exceeds limit " ++ show (f5MaxCoeffBits cfg)
  | otherwise = Nothing
  where
    deg = polyDegree p
    terms = polyTermCount p
    bits = polyMaxCoeffBits p

-- =============================================================================
-- Types
-- =============================================================================

data Signature
  = Signature
    { sigIdx :: !Int,
      sigMult :: !Monomial
    }
  deriving (Eq, Show)

compareSignatures :: (Monomial -> Monomial -> Ordering) -> Signature -> Signature -> Ordering
compareSignatures termOrd (Signature i1 m1) (Signature i2 m2) =
  case compare i1 i2 of
    LT -> LT
    GT -> GT
    EQ -> termOrd m1 m2

data LabeledPoly
  = LabeledPoly
    { lpPoly :: !Poly,
      lpSig :: !Signature,
      lpLead :: !(Maybe (Monomial, Rational)),
      lpNum :: !Int,
      lpIsHypothesis :: !Bool
    }

instance Eq LabeledPoly where
  a == b = lpNum a == lpNum b

instance Show LabeledPoly where
  show lp = "LP(" ++ show (lpSig lp) ++ ", LT=" ++ show (fmap fst (lpLead lp)) ++ ")"

type Rules = M.Map Int [Monomial]

data Task
  = Task
    { taskSig :: !Signature,
      taskLPoly1 :: !LabeledPoly,
      taskLPoly2 :: !LabeledPoly,
      taskMult :: !Monomial,
      taskDeg :: !Int
    }
  deriving (Show)

-- =============================================================================
-- F5 Algorithm (Incremental)
-- =============================================================================

f5Groebner :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly]
f5Groebner termOrd polys =
  let
    inputs = zip [1 ..] polys
    (finalBasis, _) = foldl' (f5Augment termOrd) ([], M.empty) inputs
    rawPolys = map lpPoly finalBasis
    -- Filter zeros and inter-reduce
    nonZeros = filter (not . polyIsZero) rawPolys
   in interreduce termOrd nonZeros

-- | Minimal Inter-reduction
interreduce :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly]
interreduce ord polys =
  let
    -- 1. Sort by degree/term order
    sorted = sortBy (\p1 p2 -> ord (fst (fromMaybe (monomialOne,0) (getLeadingTermByOrder ord p1))) (fst (fromMaybe (monomialOne,0) (getLeadingTermByOrder ord p2)))) polys

    -- 2. Filter redundant elements (LT divisible by other LTs)
    filterRedundant [] acc = reverse acc
    filterRedundant (p : ps) acc =
      let ltP = fst (fromMaybe (monomialOne, 0) (getLeadingTermByOrder ord p))
          isReducible = any (\g -> let ltG = fst (fromMaybe (monomialOne,0) (getLeadingTermByOrder ord g)) 
                                   in isJust (monomialDiv ltP ltG)) (acc ++ ps)
       in if isReducible
            then filterRedundant ps acc
            else filterRedundant ps (p : acc)

    minimal = filterRedundant sorted []
   in minimal

f5Solve :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly]
f5Solve = f5Groebner

-- | Memory-bounded Gröbner basis computation
f5GroebnerBounded :: F5Config -> (Monomial -> Monomial -> Ordering) -> [Poly] -> F5Result
f5GroebnerBounded cfg termOrd polys =
  let
    inputs = zip [1 ..] polys
    (finalBasis, _, stats, abortReason) =
      foldl' (f5AugmentBounded cfg termOrd) ([], M.empty, emptyStats, Nothing) inputs
    rawPolys = map lpPoly finalBasis
    nonZeros = filter (not . polyIsZero) rawPolys
    result = interreduce termOrd nonZeros
  in F5Result
     { f5Basis = result
     , f5Completed = case abortReason of Nothing -> True; _ -> False
     , f5AbortReason = abortReason
     , f5Stats = stats
     }

-- | Memory-bounded solve alias
f5SolveBounded :: F5Config -> (Monomial -> Monomial -> Ordering) -> [Poly] -> F5Result
f5SolveBounded = f5GroebnerBounded

-- | Bounded augmentation step
f5AugmentBounded :: F5Config
                 -> (Monomial -> Monomial -> Ordering)
                 -> ([LabeledPoly], Rules, F5Stats, Maybe String)
                 -> (Int, Poly)
                 -> ([LabeledPoly], Rules, F5Stats, Maybe String)
f5AugmentBounded cfg termOrd (prevBasis, rules, stats, Just reason) _ =
  -- Already aborted, pass through
  (prevBasis, rules, stats, Just reason)
f5AugmentBounded cfg termOrd (prevBasis, rules, stats, Nothing) (idx, f) =
  -- Check basis size limit
  if length prevBasis >= f5MaxBasisSize cfg
  then (prevBasis, rules, stats, Just $ "Basis size limit " ++ show (f5MaxBasisSize cfg) ++ " reached")
  else
    let
      fNorm = polyPrimitive f
      -- Check input polynomial bounds
      inputCheck = exceedsBounds cfg fNorm
    in case inputCheck of
      Just reason -> (prevBasis, rules, stats, Just $ "Input polynomial: " ++ reason)
      Nothing ->
        let
          newSig = Signature idx monomialOne
          newLP = mkLabeledPoly termOrd fNorm newSig idx True
          currentBasis = prevBasis ++ [newLP]
          newTasks = concatMap (\g -> generatePairTask termOrd newLP g) prevBasis
          (completedBasis, updatedRules, newStats, abortReason) =
            f5LoopBounded cfg termOrd currentBasis rules stats newTasks
        in (completedBasis, updatedRules, newStats, abortReason)

-- | Bounded main loop with early termination
f5LoopBounded :: F5Config
              -> (Monomial -> Monomial -> Ordering)
              -> [LabeledPoly]
              -> Rules
              -> F5Stats
              -> [Task]
              -> ([LabeledPoly], Rules, F5Stats, Maybe String)
f5LoopBounded _ _ basis rules stats [] = (basis, rules, stats, Nothing)
f5LoopBounded cfg termOrd basis rules stats tasks
  -- Check iteration limit
  | f5Iterations stats >= f5MaxIterations cfg =
      (basis, rules, stats, Just $ "Iteration limit " ++ show (f5MaxIterations cfg) ++ " reached")
  -- Check basis size limit
  | length basis >= f5MaxBasisSize cfg =
      (basis, rules, stats, Just $ "Basis size limit " ++ show (f5MaxBasisSize cfg) ++ " reached")
  | otherwise =
      let
        minDeg = minimum (map taskDeg tasks)
        (batch, remaining) = partition (\t -> taskDeg t == minDeg) tasks
        sortedBatch = sortBy (\t1 t2 -> compareSignatures termOrd (taskSig t1) (taskSig t2)) batch
        validTasks = filter (isTaskValid termOrd basis rules) sortedBatch

        -- Process batch with bounds checking
        (newPolys, newRules, batchAbort) = processBatchBounded cfg termOrd basis rules validTasks

        newStats = stats
          { f5Iterations = f5Iterations stats + 1
          , f5MaxDegSeen = max (f5MaxDegSeen stats) minDeg
          , f5ReductionsPerformed = f5ReductionsPerformed stats + length validTasks
          }

      in case batchAbort of
        Just reason -> (basis ++ newPolys, newRules, newStats, Just reason)
        Nothing ->
          let
            newBasis = basis ++ newPolys
            newTasks = concatMap (\p -> concatMap (\g -> generatePairTask termOrd p g) newBasis) newPolys
            allTasks = remaining ++ newTasks

            -- Update max terms seen
            maxTerms = if null newPolys then f5MaxTermsSeen newStats
                       else max (f5MaxTermsSeen newStats) (maximum (map (polyTermCount . lpPoly) newPolys))
            finalStats = newStats { f5MaxTermsSeen = maxTerms }

          in if null newPolys && M.null (diffRules rules newRules)
               then f5LoopBounded cfg termOrd newBasis newRules finalStats remaining
               else f5LoopBounded cfg termOrd newBasis newRules finalStats allTasks

-- | Bounded batch processing
processBatchBounded :: F5Config
                    -> (Monomial -> Monomial -> Ordering)
                    -> [LabeledPoly]
                    -> Rules
                    -> [Task]
                    -> ([LabeledPoly], Rules, Maybe String)
processBatchBounded cfg termOrd basis rules tasks =
  foldl' reduceOneBounded ([], rules, Nothing) tasks
  where
    reduceOneBounded (accPolys, accRules, Just reason) _ = (accPolys, accRules, Just reason)
    reduceOneBounded (accPolys, accRules, Nothing) task =
      let
        p = polyPrimitive (polyScaleMonomial (taskMult task) (lpPoly (taskLPoly2 task)))
        sig = taskSig task

        -- Check polynomial bounds before reduction
        preCheck = exceedsBounds cfg p
      in case preCheck of
        Just reason -> (accPolys, accRules, Just $ "Pre-reduction: " ++ reason)
        Nothing ->
          let
            (reducedP, reducedSig, _) = f5TopReductionBounded cfg termOrd (basis ++ accPolys) p sig
            finalP = polyPrimitive reducedP

            -- Check result bounds
            postCheck = exceedsBounds cfg finalP
          in case postCheck of
            Just reason -> (accPolys, accRules, Just $ "Post-reduction: " ++ reason)
            Nothing ->
              if polyIsZero finalP
              then
                let idx = sigIdx sig
                    m = sigMult sig
                    updRules = M.insertWith (++) idx [m] accRules
                in (accPolys, updRules, Nothing)
              else
                let newID = length basis + length accPolys + 1
                    newLP = mkLabeledPoly termOrd finalP reducedSig newID False
                in (accPolys ++ [newLP], accRules, Nothing)

-- | Bounded top reduction with periodic normalization
f5TopReductionBounded :: F5Config
                      -> (Monomial -> Monomial -> Ordering)
                      -> [LabeledPoly]
                      -> Poly
                      -> Signature
                      -> (Poly, Signature, Bool)
f5TopReductionBounded cfg termOrd basis p0 sig0 = go p0 0
  where
    go p !stepCount
      | polyIsZero p = (p, sig0, True)
      | otherwise =
          case getLeadingTermByOrder termOrd p of
            Nothing -> (p, sig0, True)
            Just (ltP, cP) ->
              case findReducer ltP basis of
                Nothing -> (p, sig0, False)
                Just (g, u) ->
                  let sigG = lpSig g
                      sigRed = Signature (sigIdx sigG) (monomialMul u (sigMult sigG))
                  in if compareSignatures termOrd sigRed sig0 == LT
                     then
                       let (_, cG) = fromMaybe (monomialOne, 1) (lpLead g)
                           scale = cP / cG
                           reducerPoly = polyScaleMonomial u (lpPoly g)
                           subPoly = polyScale scale reducerPoly
                           newP = polySub p subPoly
                           -- Periodic normalization to prevent coefficient explosion
                           newStep = stepCount + 1
                           normalizedP = if newStep `mod` f5NormalizeEveryN cfg == 0
                                         then polyPrimitive newP
                                         else newP
                       in go normalizedP newStep
                     else (p, sig0, False)

    findReducer m candidates =
      let possible = find (\g ->
                          let (lm, _) = fromMaybe (monomialOne, 1) (lpLead g)
                          in isJust (monomialDiv m lm)
                      ) candidates
      in case possible of
           Just g ->
             let (lm, _) = fromMaybe (monomialOne, 1) (lpLead g)
                 Just u = monomialDiv m lm
             in Just (g, u)
           Nothing -> Nothing

f5Augment :: (Monomial -> Monomial -> Ordering)
          -> ([LabeledPoly], Rules)
          -> (Int, Poly)
          -> ([LabeledPoly], Rules)
f5Augment termOrd (prevBasis, rules) (idx, f) =
  let
    -- Normalize input to avoid coefficient explosion from start
    fNorm = polyPrimitive f
    newSig = Signature idx monomialOne
    newLP = mkLabeledPoly termOrd fNorm newSig idx True

    currentBasis = prevBasis ++ [newLP]

    newTasks = concatMap (\g -> generatePairTask termOrd newLP g) prevBasis

    (completedBasis, updatedRules) = f5Loop termOrd currentBasis rules newTasks
   in (completedBasis, updatedRules)

f5Loop :: (Monomial -> Monomial -> Ordering)
       -> [LabeledPoly]
       -> Rules
       -> [Task]
       -> ([LabeledPoly], Rules)
f5Loop _ basis rules [] = (basis, rules)
f5Loop termOrd basis rules tasks =
  let
    minDeg = minimum (map taskDeg tasks)
    (batch, remaining) = partition (\t -> taskDeg t == minDeg) tasks

    sortedBatch = sortBy (\t1 t2 -> compareSignatures termOrd (taskSig t1) (taskSig t2)) batch

    validTasks = filter (isTaskValid termOrd basis rules) sortedBatch

    (newPolys, newRules) = processBatch termOrd basis rules validTasks

    newBasis = basis ++ newPolys

    newTasks = concatMap (\p -> concatMap (\g -> generatePairTask termOrd p g) newBasis) newPolys

    allTasks = remaining ++ newTasks

   in if null newPolys && M.null (diffRules rules newRules)
        then f5Loop termOrd newBasis newRules remaining
        else f5Loop termOrd newBasis newRules allTasks

diffRules :: Rules -> Rules -> Rules
diffRules old new = M.unionWith (++) new old

-- =============================================================================
-- F5 Reduction (Symbolic, Signature-Aware)
-- =============================================================================

processBatch :: (Monomial -> Monomial -> Ordering)
             -> [LabeledPoly]
             -> Rules
             -> [Task]
             -> ([LabeledPoly], Rules)
processBatch termOrd basis rules tasks = foldl' reduceOne ([], rules) tasks
  where
    reduceOne (accPolys, accRules) task =
      let
        -- Normalize S-poly
        p = polyPrimitive (polyScaleMonomial (taskMult task) (lpPoly (taskLPoly2 task)))
        sig = taskSig task

        -- Reduction
        (reducedP, reducedSig, isZero) = f5TopReduction termOrd (basis ++ accPolys) p sig

        -- Normalize result
        finalP = polyPrimitive reducedP

       in if polyIsZero finalP
            then
              let idx = sigIdx sig
                  m = sigMult sig
                  updRules = M.insertWith (++) idx [m] accRules
               in (accPolys, updRules)
            else
              let newID = length basis + length accPolys + 1
                  newLP = mkLabeledPoly termOrd finalP reducedSig newID False
               in (accPolys ++ [newLP], accRules)

f5TopReduction :: (Monomial -> Monomial -> Ordering)
               -> [LabeledPoly]
               -> Poly
               -> Signature
               -> (Poly, Signature, Bool)
f5TopReduction termOrd basis p0 sig0 = go p0
  where
    go p
      | polyIsZero p = (p, sig0, True)
      | otherwise =
          case getLeadingTermByOrder termOrd p of
            Nothing -> (p, sig0, True)
            Just (ltP, cP) ->
              case findReducer ltP basis of
                Nothing -> (p, sig0, False)
                Just (g, u) ->
                  let sigG = lpSig g
                      sigRed = Signature (sigIdx sigG) (monomialMul u (sigMult sigG))
                   in if compareSignatures termOrd sigRed sig0 == LT
                        then
                          let (ltG, cG) = fromMaybe (monomialOne, 1) (lpLead g)
                              scale = cP / cG
                              reducerPoly = polyScaleMonomial u (lpPoly g)
                              subPoly = polyScale scale reducerPoly
                              newP = polySub p subPoly
                              -- Note: We don't normalize every step to save time, but processBatch normalizes end result.
                           in go newP
                        else (p, sig0, False)

    findReducer m candidates =
      -- Optimized search: check if ANY candidate divides
      -- To optimize: stop early? 'find' does stop early.
      -- Candidates are 'basis'.
      -- We could pre-sort candidates?
      -- Current: linear scan.
            let possible = find (\g ->
                                let (lm, _) = fromMaybe (monomialOne, 1) (lpLead g)
                                 in isJust (monomialDiv m lm)
                            ) candidates
       in case possible of
            Just g ->
              let (lm, _) = fromMaybe (monomialOne, 1) (lpLead g)
                  Just u = monomialDiv m lm
               in Just (g, u)
            Nothing -> Nothing

-- =============================================================================
-- Criteria
-- =============================================================================

isTaskValid :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> Rules -> Task -> Bool
isTaskValid termOrd basis rules task =
  let sig = taskSig task
   in not (isSyzygy termOrd rules sig) && not (isRewritable termOrd basis sig)

isSyzygy :: (Monomial -> Monomial -> Ordering) -> Rules -> Signature -> Bool
isSyzygy _ rules (Signature idx u) =
  case M.lookup idx rules of
    Nothing -> False
    Just monos -> any (\m -> isJust (monomialDiv u m)) monos

isRewritable :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> Signature -> Bool
isRewritable _termOrd basis (Signature idx u) =
  let
    previousBasis = filter (\lp -> sigIdx (lpSig lp) < idx) basis
  in
    any (\g -> case lpLead g of
                   Just (lm, _) -> isJust (monomialDiv u lm)
                   Nothing -> False
          ) previousBasis

-- =============================================================================
-- Helpers
-- =============================================================================

generatePairTask :: (Monomial -> Monomial -> Ordering) -> LabeledPoly -> LabeledPoly -> [Task]
generatePairTask termOrd p1 p2 =
  let
    (lm1, _) = fromMaybe (monomialOne, 1) (lpLead p1)
    (lm2, _) = fromMaybe (monomialOne, 1) (lpLead p2)
    lcm = monomialLCM lm1 lm2

    u1 = fromMaybe monomialOne (monomialDiv lcm lm1)
    u2 = fromMaybe monomialOne (monomialDiv lcm lm2)

    s1 = Signature (sigIdx (lpSig p1)) (monomialMul u1 (sigMult (lpSig p1)))
    s2 = Signature (sigIdx (lpSig p2)) (monomialMul u2 (sigMult (lpSig p2)))

    cmp = compareSignatures termOrd s1 s2
   in case cmp of
        GT -> [Task s1 p2 p1 u1 (monomialDegree lcm)]
        LT -> [Task s2 p1 p2 u2 (monomialDegree lcm)]
        EQ -> []

mkLabeledPoly :: (Monomial -> Monomial -> Ordering) -> Poly -> Signature -> Int -> Bool -> LabeledPoly
mkLabeledPoly ord p s n hypo =
  LabeledPoly
    {
      lpPoly = p,
      lpSig = s,
      lpLead = getLeadingTermByOrder ord p,
      lpNum = n,
      lpIsHypothesis = hypo
    }

polyIsZero :: Poly -> Bool
polyIsZero p = p == polyZero

-- | Alias for fromMonomial from Polynomial module (for backwards compatibility)
polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial = fromMonomial

polyScaleMonomial :: Monomial -> Poly -> Poly
polyScaleMonomial m (Poly mm) =
  Poly $ M.mapKeys (monomialMul m) mm

monomialDegree :: Monomial -> Int
monomialDegree (Monomial m) = fromIntegral $ sum (M.elems m)
