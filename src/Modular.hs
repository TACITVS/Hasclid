{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Modular
  ( probSolve
  , prime
  , ModVal
  , toMod
  , addM
  , subM
  , mulM
  , divM
  , invM
  ) where

import Expr
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Ratio (numerator, denominator)
import System.Random (mkStdGen, randomRs)

-- =============================================
-- 1. Modular Arithmetic (Field F_p)
-- =============================================

-- Large prime that fits in Int32, so (a*b) fits in Int64
-- Mersenne 31: 2^31 - 1
prime :: Integer
prime = 2147483647

type ModVal = Integer

toMod :: Integer -> ModVal
toMod n = n `mod` prime

addM :: ModVal -> ModVal -> ModVal
addM a b = (a + b) `mod` prime

subM :: ModVal -> ModVal -> ModVal
subM a b = (a - b + prime) `mod` prime

mulM :: ModVal -> ModVal -> ModVal
mulM a b = (a * b) `mod` prime

-- Modular Inverse (Extended Euclidean)
invM :: ModVal -> ModVal
invM a = 
  let (g, x, _) = gcdExt a prime
  in if g == 1 then (x `mod` prime + prime) `mod` prime else error "No modular inverse"

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (a, 1, 0)
gcdExt a b =
  let (g, x1, y1) = gcdExt b (a `mod` b)
      x = y1
      y = x1 - (a `div` b) * y1
  in (g, x, y)

divM :: ModVal -> ModVal -> ModVal
divM a b = mulM a (invM b)

-- =============================================
-- 2. Modular Polynomials
-- =============================================

-- Polynomial with Modular coefficients
-- We reuse Expr.Monomial for keys
newtype PolyM = PolyM (M.Map Monomial ModVal) deriving (Eq, Show)

polyMZero :: PolyM
polyMZero = PolyM M.empty

polyMFromConst :: ModVal -> PolyM
polyMFromConst c 
  | c == 0 = polyMZero
  | otherwise = PolyM (M.singleton (Monomial M.empty) c)

polyMAdd :: PolyM -> PolyM -> PolyM
polyMAdd (PolyM p1) (PolyM p2) =
  PolyM $ M.filter (/= 0) $ M.unionWith addM p1 p2

polyMSub :: PolyM -> PolyM -> PolyM
polyMSub (PolyM p1) (PolyM p2) =
  let p2' = M.map (\c -> (prime - c) `mod` prime) p2
  in PolyM $ M.filter (/= 0) $ M.unionWith addM p1 p2'

polyMMul :: PolyM -> PolyM -> PolyM
polyMMul (PolyM p1) (PolyM p2) =
  let terms = [ (monomialMul m1 m2, mulM c1 c2) 
              | (m1, c1) <- M.toList p1
              , (m2, c2) <- M.toList p2 ]
  in PolyM $ M.filter (/= 0) $ M.fromListWith addM terms

polyMMulConst :: PolyM -> ModVal -> PolyM
polyMMulConst (PolyM p) c =
  if c == 0 then polyMZero
  else PolyM $ M.map (\v -> mulM v c) p

-- Leading Term (Lexicographic by default Monomial Ord)
getLM :: PolyM -> Maybe (Monomial, ModVal)
getLM (PolyM m)
  | M.null m = Nothing
  | otherwise = M.lookupMax m

-- Helper: LCM of monomials
lcmM :: Monomial -> Monomial -> Monomial
lcmM (Monomial m1) (Monomial m2) =
  Monomial $ M.unionWith max m1 m2

-- Helper: Division of monomials (Just m1/m2 if divisible)
divMonomial :: Monomial -> Monomial -> Maybe Monomial
divMonomial (Monomial m1) (Monomial m2)
  | M.isSubmapOfBy (<=) m2 m1 = 
      Just $ Monomial (M.differenceWith (\v1 v2 -> if v1==v2 then Nothing else Just (v1-v2)) m1 m2)
  | otherwise = Nothing

-- =============================================
-- 3. Modular Buchberger (Groebner Basis)
-- =============================================

-- S-Polynomial in F_p
sPolyM :: PolyM -> PolyM -> PolyM
sPolyM f g =
  case (getLM f, getLM g) of
    (Just (lmF, cfF), Just (lmG, cfG)) ->
      let gamma = lcmM lmF lmG
      in case (divMonomial gamma lmF, divMonomial gamma lmG) of
           (Just mF, Just mG) ->
             let term1 = polyMMulConst (polyMMul (PolyM (M.singleton mF 1)) f) cfG
                 term2 = polyMMulConst (polyMMul (PolyM (M.singleton mG 1)) g) cfF
             in polyMSub term1 term2
           _ -> polyMZero
    _ -> polyMZero

-- Reduction f mod G
reduceM :: PolyM -> [PolyM] -> PolyM
reduceM p divs
  | p == polyMZero = polyMZero
  | otherwise      = go p
  where
    go q =
      case getLM q of
        Nothing -> polyMZero
        Just (lmP, cfP) ->
          case firstReducer lmP of
            Nothing -> q
            Just (g, lmG, cfG) ->
              case divMonomial lmP lmG of
                Nothing    -> q
                Just mQuot ->
                  let cQuot  = divM cfP cfG
                      subTerm = polyMMulConst (polyMMul (PolyM (M.singleton mQuot 1)) g) cQuot
                  in go (polyMSub q subTerm)

    firstReducer lmP =
      case [ (g, lmG, cfG)
           | g <- divs
           , Just (lmG, cfG) <- [getLM g]
           , Just _ <- [divMonomial lmP lmG]
           ] of
        []    -> Nothing
        (r:_) -> Just r

buchbergerM :: [PolyM] -> [PolyM]
buchbergerM polys =
  let initial = filter (/= polyMZero) polys
      pairs = [ (f,g) | f <- initial, g <- initial, f /= g ]
  in go initial pairs
  where
    go basis [] = basis
    go basis ((f,g):ps) =
      let s = sPolyM f g
          rem = reduceM s basis
      in if rem == polyMZero
         then go basis ps
         else 
           let newPairs = [ (rem, b) | b <- basis ]
           in go (rem:basis) (ps ++ newPairs)

-- =============================================
-- 4. Probabilistic Solver Logic
-- =============================================

-- | Main Entry Point:
--   1. Instantiate free variables with random values in F_p
--   2. Convert formulas to Modular Polynomials
--   3. Run Buchberger
--   4. Check consistency
probSolve :: Theory -> Formula -> (Bool, String)
probSolve theory goal =
  let
    -- 1. Extract explicit quantified variables
    explicitQVars = case goal of
                      Exists qs _ -> map qvName qs
                      _ -> []
    qSet = S.fromList explicitQVars
    
    -- 2. Extract all equations from theory + goal
    theoryEqs = [ Sub l r | Eq l r <- theory ]
    goalEqs = extractBodyEqs goal
    allEqs = theoryEqs ++ goalEqs
    
    -- 3. Extract all variables
    allVars = S.toList $ foldr (S.union . extractExprVars) S.empty allEqs

    -- 4. Identify Free Variables vs Variables to Solve
    -- If explicit quantifiers exist, everything else is a parameter.
    -- If NO explicit quantifiers, assume EVERYTHING is a variable to solve (consistency check).
    (varsToSolve, params) = 
      if null explicitQVars
      then (allVars, [])
      else (explicitQVars, filter (\v -> not (S.member v qSet)) allVars)

    -- 5. Generate Random Assignment for Parameters ONLY
    seed = 42
    rng = mkStdGen seed
    randVals = randomRs (1, prime - 1) rng
    assignment = M.fromList (zip params randVals)
    
    -- 6. Instantiate and Convert to PolyM
    polysM = map (toPolyM assignment) allEqs
    
    -- 7. Compute Groebner Basis
    basis = buchbergerM polysM
    
    -- 8. Check if 1 is in the Ideal
    isInconsistent = any isConstantNonZero basis
    
  in if isInconsistent
     then (False, "System inconsistent modulo " ++ show prime ++ " (Probabilistic)")
     else (True, "Solution exists modulo " ++ show prime ++ " (Probabilistic)")

extractBodyEqs :: Formula -> [Expr]
extractBodyEqs (Eq l r) = [Sub l r]
extractBodyEqs (And a b) = extractBodyEqs a ++ extractBodyEqs b
extractBodyEqs (Exists _ b) = extractBodyEqs b
extractBodyEqs (Or _ _) = [] -- Cannot handle disjunction in simple GB, treat as consistent
extractBodyEqs (Not _) = []  -- Cannot handle negation, treat as consistent
extractBodyEqs (Forall _ _) = [] -- Cannot handle universal, treat as consistent
extractBodyEqs _ = []

extractExprVars :: Expr -> S.Set String
extractExprVars (Var v) = S.singleton v
extractExprVars (Add a b) = S.union (extractExprVars a) (extractExprVars b)
extractExprVars (Sub a b) = S.union (extractExprVars a) (extractExprVars b)
extractExprVars (Mul a b) = S.union (extractExprVars a) (extractExprVars b)
extractExprVars (Pow a _) = extractExprVars a
extractExprVars (Dist2 a b) = S.fromList ["x"++a, "y"++a, "x"++b, "y"++b] -- Simplify
extractExprVars _ = S.empty -- Simplified for prototype

-- | Convert Expr to PolyM, substituting free variables
toPolyM :: M.Map String ModVal -> Expr -> PolyM
toPolyM assign expr = 
  case expr of
    Var v -> case M.lookup v assign of
               Just val -> polyMFromConst val -- It's a parameter (constant)
               Nothing  -> PolyM (M.singleton (Monomial (M.singleton v 1)) 1) -- It's a variable
    Const r -> 
      let n = numerator r
          d = denominator r
      in polyMFromConst (divM (toMod n) (toMod d))
    Add a b -> polyMAdd (toPolyM assign a) (toPolyM assign b)
    Sub a b -> polyMSub (toPolyM assign a) (toPolyM assign b)
    Mul a b -> polyMMul (toPolyM assign a) (toPolyM assign b)
    Pow a n -> polyMPow (toPolyM assign a) (fromIntegral n)
    -- Handle geometry primitives by expansion (simplified for this module)
    Dist2 a b -> 
      let xa = toPolyM assign (Var ("x"++a)); ya = toPolyM assign (Var ("y"++a))
          xb = toPolyM assign (Var ("x"++b)); yb = toPolyM assign (Var ("y"++b))
          dx = polyMSub xa xb
          dy = polyMSub ya yb
      in polyMAdd (polyMMul dx dx) (polyMMul dy dy)
    -- Fallback for other geometry
    _ -> polyMZero -- Should expand others too, but Dist2 is main one for circle

polyMPow :: PolyM -> Int -> PolyM
polyMPow _ 0 = polyMFromConst 1
polyMPow p 1 = p
polyMPow p n 
  | even n = let h = polyMPow p (n `div` 2) in polyMMul h h
  | otherwise = polyMMul p (polyMPow p (n - 1))

isConstantNonZero :: PolyM -> Bool
isConstantNonZero (PolyM m) =
  case M.toList m of
    [(Monomial vars, _)] -> M.null vars
    _ -> False
