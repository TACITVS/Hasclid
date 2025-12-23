{-# LANGUAGE BangPatterns #-}
module F5
  ( f5Groebner
  , f5Solve
  )
where

import Expr (Poly(..), Monomial(..), monomialLCM, monomialMul, monomialDiv, getLeadingTermByOrder, monomialGCD, monomialOne, polyAdd, polySub, polyMul, getLeadingTerm, polyScale, polyZero)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl', nub, partition, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import qualified F4Lite as F4 (modularRowReduceMulti, Row, Col, rowsToPolys, buildMacaulayMatrix)
import TermOrder (TermOrder(..), compareMonomials)
import Debug.Trace (trace)

-- =============================================================================
-- Types
-- =============================================================================

data Signature = Signature 
  { sigIdx  :: !Int       
  , sigMult :: !Monomial  
  }
  deriving (Eq, Show)

compareSignatures :: (Monomial -> Monomial -> Ordering) -> Signature -> Signature -> Ordering
compareSignatures termOrd (Signature i1 m1) (Signature i2 m2) =
  case compare i2 i1 of 
    EQ -> termOrd m1 m2
    ord -> ord

data LabeledPoly = LabeledPoly
  { lpPoly :: !Poly
  , lpSig  :: !Signature
  , lpLead :: !(Maybe (Monomial, Rational)) 
  , lpNum  :: !Int 
  }

instance Eq LabeledPoly where
  a == b = lpNum a == lpNum b

instance Show LabeledPoly where
  show lp = "LP(" ++ show (lpSig lp) ++ ", " ++ show (length (case lpPoly lp of Poly m -> M.toList m)) ++ " terms)"

data Task = Task
  { taskSig    :: !Signature    
  , taskLPoly1 :: !LabeledPoly  
  , taskLPoly2 :: !LabeledPoly  
  , taskMult   :: !Monomial     
  , taskDeg    :: !Int          
  }
  deriving (Show)

-- =============================================================================
-- F5 Algorithm
-- =============================================================================

f5Groebner :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly]
f5Groebner termOrd polys =
  let
    inputs = zip [1..] polys
    finalBasis = foldl' (f5Augment termOrd) [] inputs
  in
    map lpPoly finalBasis

f5Solve :: (Monomial -> Monomial -> Ordering) -> [Poly] -> [Poly]
f5Solve = f5Groebner

f5Augment :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> (Int, Poly) -> [LabeledPoly]
f5Augment termOrd currentBasis (idx, f) =
  let
    newSig = Signature idx monomialOne
    newLP = mkLabeledPoly termOrd f newSig idx
    initialBasis = currentBasis ++ [newLP]
    newTasks = generateTasks termOrd newLP currentBasis
    completedBasis = f5Loop termOrd initialBasis newTasks
  in
    completedBasis

f5Loop :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> [Task] -> [LabeledPoly]
f5Loop _ basis [] = basis
f5Loop termOrd basis tasks =
  let
    minDeg = minimum (map taskDeg tasks)
    (batch, remaining) = partition (\t -> taskDeg t == minDeg) tasks
    
    validTasks = filter (isTaskValid termOrd basis) batch
    
    (newPolys, _) = reduceTasks termOrd basis validTasks
    
    generatedTasks = concatMap (\p -> generateTasks termOrd p basis) newPolys
    allTasks = remaining ++ generatedTasks
    newBasis = basis ++ newPolys
  in
    if null newPolys
    then f5Loop termOrd newBasis remaining
    else f5Loop termOrd newBasis allTasks

-- =============================================================================
-- Helpers
-- =============================================================================

polyIsZero :: Poly -> Bool
polyIsZero p = p == polyZero

polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = if c == 0 then polyZero else Poly (M.singleton m c)

mkLabeledPoly :: (Monomial -> Monomial -> Ordering) -> Poly -> Signature -> Int -> LabeledPoly
mkLabeledPoly ord p s n =
  LabeledPoly 
    { lpPoly = p
    , lpSig = s
    , lpLead = getLeadingTermByOrder ord p
    , lpNum = n
    }

generateTasks :: (Monomial -> Monomial -> Ordering) -> LabeledPoly -> [LabeledPoly] -> [Task]
generateTasks termOrd p1 polys =
  [ makeTask termOrd p1 p2 
  | p2 <- polys
  , let m1 = fromMaybe monomialOne (fst <$> lpLead p1)
  , let m2 = fromMaybe monomialOne (fst <$> lpLead p2)
  , let lcm = monomialLCM m1 m2
  , not (monomialGCD m1 m2 == monomialOne) 
  ]

makeTask :: (Monomial -> Monomial -> Ordering) -> LabeledPoly -> LabeledPoly -> Task
makeTask termOrd p1 p2 =
  let
    (lm1, _) = fromMaybe (monomialOne, 0) (lpLead p1)
    (lm2, _) = fromMaybe (monomialOne, 0) (lpLead p2)
    lcm = monomialLCM lm1 lm2
    u1 = fromMaybe monomialOne (monomialDiv lcm lm1)
    u2 = fromMaybe monomialOne (monomialDiv lcm lm2)
    s1 = scaleSig u1 (lpSig p1)
    s2 = scaleSig u2 (lpSig p2)
    
    cmp = compareSignatures termOrd s1 s2
    (target, reducer, mult, sig) = case cmp of
       GT -> (p1, p2, u1, s1) 
       LT -> (p2, p1, u2, s2)
       EQ -> (p1, p2, u1, s1) 
  in
    Task 
      { taskSig = sig
      , taskLPoly1 = reducer 
      , taskLPoly2 = target
      , taskMult = mult
      , taskDeg = fromIntegral (monomialDegree lcm)
      }

scaleSig :: Monomial -> Signature -> Signature
scaleSig m (Signature i sm) = Signature i (monomialMul m sm)

isTaskValid :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> Task -> Bool
isTaskValid _ord _basis _task = True 

reduceTasks :: (Monomial -> Monomial -> Ordering) -> [LabeledPoly] -> [Task] -> ([LabeledPoly], [Signature])
reduceTasks termOrd basis tasks =
  let
    taskPolys = map (\t -> polyScaleMonomial (taskMult t) (lpPoly (taskLPoly2 t))) tasks
    (colsSet, rows) = F4.buildMacaulayMatrix termOrd (map lpPoly basis) taskPolys
    cols = sortBy (\a b -> termOrd b a) (S.toList colsSet)
    
    reducedRows = F4.modularRowReduceMulti colsSet rows
    reducedPolys = F4.rowsToPolys cols reducedRows
    
    useful = filter (not . isReducibleByReducers termOrd (map lpPoly basis)) reducedPolys
    
    baseIdx = length basis + 1
    newLPs = zipWith (\p i -> mkLabeledPoly termOrd p (Signature 0 monomialOne) (baseIdx + i)) useful [0..]
    
  in (newLPs, [])

polyScaleMonomial :: Monomial -> Poly -> Poly
polyScaleMonomial m (Poly mm) =
  Poly $ M.mapKeys (monomialMul m) mm

monomialDegree :: Monomial -> Int
monomialDegree (Monomial m) = fromIntegral $ sum (M.elems m)

isReducibleByReducers :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Bool
isReducibleByReducers ord reducers p =
  case getLeadingTermByOrder ord p of
    Nothing -> True
    Just (lt, _) -> any (\g -> case getLeadingTermByOrder ord g of
                                 Just (ltG, _) -> isJust (monomialDiv lt ltG)
                                 Nothing -> False) reducers
