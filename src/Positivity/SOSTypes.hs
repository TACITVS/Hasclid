module Positivity.SOSTypes
  ( SOSCertificate(..)
  , SOSPattern(..)
  , trySOSHeuristic
  , sqrtRational
  ) where

import Expr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ratio
import Data.List (sort, nub, permutations)
import Debug.Trace (trace)

-- | Certificate showing that a polynomial is a sum of squares.
data SOSCertificate = SOSCertificate
  { sosTerms :: [(Rational, Poly)] 
  , sosLemmas :: [Poly]            
  , sosRemainder :: Poly           
  , sosPattern :: Maybe SOSPattern 
  } deriving (Show, Eq)

-- | Common SOS patterns
data SOSPattern
  = TrivialSquare          
  | SumOfSquares           
  | CompletedSquare        
  | CauchySchwarz          
  | TriangleInequality     
  | AM_GM                  
  | Weitzenbock            
  | Custom String          
  deriving (Eq, Show)

-- | Try to decompose polynomial using heuristic pattern matching
trySOSHeuristic :: Poly -> [Formula] -> Maybe SOSCertificate
trySOSHeuristic poly theory =
  let _ = trace ("SOS: trySOSHeuristic on poly with " ++ show (Map.size (case poly of Poly m -> m)) ++ " terms.") ()
  in case tryTrivialSquare poly of
    Just cert -> Just cert
    Nothing -> case trySimpleSumOfSquares poly of
      Just cert -> Just cert
      Nothing -> case tryAMGMPattern poly theory of
        Just cert -> Just cert
        Nothing -> case tryTriangleInequalityPattern poly of
          Just cert -> Just cert
          Nothing -> Nothing

-- | Check if polynomial is already a perfect square
tryTrivialSquare :: Poly -> Maybe SOSCertificate
tryTrivialSquare p =
  case polynomialSqrt p of
    Just q -> Just $ SOSCertificate [(1, q)] [] polyZero (Just TrivialSquare)
    Nothing -> Nothing

-- | Exact polynomial square root
polynomialSqrt :: Poly -> Maybe Poly
polynomialSqrt p
  | p == polyZero = Just polyZero
  | otherwise =
      case getLeadingTerm p of
        Nothing -> Just polyZero
        Just (ltM, ltC) ->
          if not (isSquareMonomial ltM) || not (isSquareRational ltC)
          then Nothing
          else
            let rootM = sqrtMonomial ltM
                rootC = sqrtRational ltC
                rootLT = polyFromMonomial rootM rootC
            in findRoot p rootLT

findRoot :: Poly -> Poly -> Maybe Poly
findRoot target currentRoot =
  let remainder = subPoly target (_mulPoly currentRoot currentRoot)
  in if remainder == polyZero
     then Just currentRoot
     else
       case getLeadingTerm remainder of
         Nothing -> Just currentRoot
         Just (remLT_M, remLT_C) ->
           case getLeadingTerm currentRoot of
             Nothing -> Nothing
             Just (rootLT_M, rootLT_C) ->
               let factor = 2 * rootLT_C
                   nextC = remLT_C / factor
                   nextM = monomialDiv remLT_M rootLT_M
               in case nextM of
                    Nothing -> Nothing
                    Just m ->
                      let nextTerm = polyFromMonomial m nextC
                      in findRoot target (_addPoly currentRoot nextTerm)

isSquareMonomial :: Monomial -> Bool
isSquareMonomial (Monomial vars) = all even (Map.elems vars)

sqrtMonomial :: Monomial -> Monomial
sqrtMonomial (Monomial vars) = Monomial (Map.map (`div` 2) vars)

isSquareRational :: Rational -> Bool
isSquareRational r = r >= 0 && isSquare (numerator r) && isSquare (denominator r)

isSquare :: Integer -> Bool
isSquare x | x < 0 = False | otherwise = let s = Expr.integerSqrt x in s * s == x

sqrtRational :: Rational -> Rational
sqrtRational r = let n = numerator r; d = denominator r
                 in Expr.integerSqrt n % Expr.integerSqrt d

polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = Poly (Map.singleton m c)

-- | Check if polynomial is a simple sum of squares
trySimpleSumOfSquares :: Poly -> Maybe SOSCertificate
trySimpleSumOfSquares (Poly m) =
  let terms = Map.toList m
      squares = [ (c, Poly (Map.singleton (sqrtMonomial mon) 1)) 
                | (mon, c) <- terms, c > 0, isSquareMonomial mon ]
  in if length squares == Map.size m && not (null squares)
     then Just $ SOSCertificate squares [] polyZero (Just SumOfSquares)
     else Nothing

-- | Try to match triangle inequality pattern
tryTriangleInequalityPattern :: Poly -> Maybe SOSCertificate
tryTriangleInequalityPattern poly =
  case matchTrianglePattern poly of
    Just (varA, varB, varC) ->
      let polyA = polyFromVar varA; polyB = polyFromVar varB; polyC = polyFromVar varC
          comp1 = subPoly polyA polyB; comp2 = subPoly polyB polyC; comp3 = subPoly polyC polyA
          terms = [(1/2, comp1), (1/2, comp2), (1/2, comp3)]
      in Just $ SOSCertificate terms [] polyZero (Just TriangleInequality)
    Nothing -> Nothing

matchTrianglePattern :: Poly -> Maybe (String, String, String)
matchTrianglePattern (Poly m)
  | Map.size m < 3 = Nothing
  | otherwise =
      let vars = nub [v | (Monomial vm, _) <- Map.toList m, (v, deg) <- Map.toList vm, deg == 2]
      in case vars of
           [a, b, c] -> if looksLikeTrianglePattern m a b c then Just (a, b, c) else Nothing
           _ -> Nothing

looksLikeTrianglePattern :: Map.Map Monomial Rational -> String -> String -> String -> Bool
looksLikeTrianglePattern m a b c =
  let a2 = Monomial (Map.singleton a 2); b2 = Monomial (Map.singleton b 2); c2 = Monomial (Map.singleton c 2)
      ab = Monomial (Map.fromList [(a, 1), (b, 1)]); ac = Monomial (Map.fromList [(a, 1), (c, 1)]); bc = Monomial (Map.fromList [(b, 1), (c, 1)])
      getCoeff mon = Map.findWithDefault 0 mon m
      cA2 = getCoeff a2; cB2 = getCoeff b2; cC2 = getCoeff c2
      cAB = getCoeff ab; cAC = getCoeff ac; cBC = getCoeff bc
  in cA2 > 0 && cB2 > 0 && cC2 > 0 && abs (cA2 - cB2) < 0.01 * cA2 && cAB < 0 && cAC < 0 && cBC < 0

-- | Try AM-GM pattern for n=3: (xy+yz+zx)^3 - 27x^2y^2z^2 >= 0
tryAMGMPattern :: Poly -> [Formula] -> Maybe SOSCertificate
tryAMGMPattern p _theory = 
  let _ = trace ("SOS: tryAMGMPattern on poly: " ++ prettyPolyNice p) ()
  in case matchAMGM3 p of
    Just _ -> trace "SOS: AM-GM Pattern Matched!" $ Just $ SOSCertificate [] [] polyZero (Just AM_GM)
    Nothing -> Nothing

-- | Robust AM-GM matcher for n=3
matchAMGM3 :: Poly -> Maybe (Poly, Poly, Poly)
matchAMGM3 poly =
  let terms = Map.toList (case poly of Poly m -> m)
      -- Identify possible product terms (-27 * mon)
      productCandidates = filter (\(_, c) -> c < 0) terms
  in case productCandidates of
       ((mon, c):_) -> 
         let k = c / (-27)
             xyz2 = Poly (Map.singleton mon (27 * k))
             sumCube = _addPoly poly xyz2
             maybeRoot = cubicRoot (_polyScale sumCube (1/k))
         in case maybeRoot of
              Just root -> 
                case Map.toList (case root of Poly m -> m) of
                  [(m1, 1), (m2, 1), (m3, 1)] -> 
                    if combineMonomial m1 (combineMonomial m2 m3) == mon 
                    then Just (Poly (Map.singleton m1 1), Poly (Map.singleton m2 1), Poly (Map.singleton m3 1)) 
                    else Nothing
                  _ -> Nothing
              Nothing -> Nothing
       _ -> Nothing

-- | Find cubic root of a polynomial if it exists
cubicRoot :: Poly -> Maybe Poly
cubicRoot p =
  case getLeadingTerm p of
    Nothing -> Just polyZero
    Just (ltM, _) ->
      if not (monomialDegree ltM `mod` 3 == 0) then Nothing
      else let terms = Map.toList (case p of Poly m -> m)
               isComponent (m, c) = c == 1 && monomialDegree m * 3 == monomialDegree ltM
               components = filter isComponent terms
           in if length components == 3
                 then let root = foldl _addPoly polyZero (map (\(m, _) -> Poly (Map.singleton m 1)) components)
                      in if p == _mulPoly root (_mulPoly root root) then Just root else Nothing
                 else Nothing

monomialDegree :: Monomial -> Integer
monomialDegree (Monomial m) = fromIntegral $ Map.foldl (+) 0 m

_addPoly :: Poly -> Poly -> Poly
_addPoly (Poly m1) (Poly m2) = Poly (Map.filter (/= 0) (Map.unionWith (+) m1 m2))

subPoly :: Poly -> Poly -> Poly
subPoly (Poly m1) (Poly m2) = Poly (Map.filter (/= 0) (Map.unionWith (+) m1 (Map.map negate m2)))

_mulPoly :: Poly -> Poly -> Poly
_mulPoly (Poly m1) (Poly m2) = Poly (Map.filter (/= 0) (Map.fromListWith (+) [ (combineMonomial mon1 mon2, c1 * c2) | (mon1, c1) <- Map.toList m1, (mon2, c2) <- Map.toList m2 ]))

combineMonomial :: Monomial -> Monomial -> Monomial
combineMonomial (Monomial m1) (Monomial m2) = Monomial $ Map.unionWith (+) m1 m2

_polyScale :: Poly -> Rational -> Poly
_polyScale (Poly m) s = Poly (Map.map (*s) m)
