module Positivity.SOSTypes
  ( SOSCertificate(..)
  , SOSPattern(..)
  , trySOSHeuristic
  , sqrtRational
  ) where

import Expr (Poly(..), Monomial(..), Formula, polyZero, polyFromVar, getLeadingTerm, integerSqrt)
import Polynomial (add, sub, mul, scale, fromMonomial, monomialDegree, monomialMul, monomialDiv)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.List (nub)

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

-- | Try to decompose polynomial using heuristic pattern matching.
-- Attempts patterns in order: trivial square, sum of squares, AM-GM, triangle inequality.
trySOSHeuristic :: Poly -> [Formula] -> Maybe SOSCertificate
trySOSHeuristic poly theory =
  case tryTrivialSquare poly of
    Just cert -> Just cert
    Nothing -> case trySimpleSumOfSquares poly of
      Just cert -> Just cert
      Nothing -> case tryAMGMPattern poly theory of
        Just cert -> Just cert
        Nothing -> tryTriangleInequalityPattern poly

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
                rootLT = fromMonomial rootM rootC
            in findRoot p rootLT

findRoot :: Poly -> Poly -> Maybe Poly
findRoot target currentRoot =
  let remainder = sub target (mul currentRoot currentRoot)
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
                      let nextTerm = fromMonomial m nextC
                      in findRoot target (add currentRoot nextTerm)

isSquareMonomial :: Monomial -> Bool
isSquareMonomial (Monomial vars) = all even (Map.elems vars)

sqrtMonomial :: Monomial -> Monomial
sqrtMonomial (Monomial vars) = Monomial (Map.map (`div` 2) vars)

isSquareRational :: Rational -> Bool
isSquareRational r = r >= 0 && isSquare (numerator r) && isSquare (denominator r)

isSquare :: Integer -> Bool
isSquare x | x < 0 = False | otherwise = let s = integerSqrt x in s * s == x

sqrtRational :: Rational -> Rational
sqrtRational r = let n = numerator r; d = denominator r
                 in integerSqrt n % integerSqrt d

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
          comp1 = sub polyA polyB; comp2 = sub polyB polyC; comp3 = sub polyC polyA
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
  case matchAMGM3 p of
    Just _ -> Just $ SOSCertificate [] [] polyZero (Just AM_GM)
    Nothing -> Nothing

-- | Robust AM-GM matcher for n=3
-- Matches k * ((a+b+c)^3 - 27abc)
-- Note: Expansion of (a+b+c)^3 has a 6abc term, so the coefficient of abc in the final poly is -21k.
matchAMGM3 :: Poly -> Maybe (Poly, Poly, Poly)
matchAMGM3 poly =
  case getLeadingTerm poly of
    Just (ltM, k) ->
      let rootCandidateCube = scale (1/k) poly -- Should be (a+b+c)^3 - 21abc
          -- The product term in rootCandidateCube should be -21abc
          terms = Map.toList (case rootCandidateCube of Poly m -> m)
          productCandidates = filter (\(_, c) -> c == -21) terms
      in case productCandidates of
           ((mon, _):_) ->
             let abc = Poly (Map.singleton mon 1)
                 maybeRoot = cubicRoot (add rootCandidateCube (scale 21 abc))
             in case maybeRoot of
                  Just root ->
                    case Map.toList (case root of Poly m -> m) of
                      [(m1, 1), (m2, 1), (m3, 1)] ->
                        if monomialMul m1 (monomialMul m2 m3) == mon
                        then Just (Poly (Map.singleton m1 1), Poly (Map.singleton m2 1), Poly (Map.singleton m3 1))
                        else Nothing
                      _ -> Nothing
                  Nothing -> Nothing
           _ -> Nothing
    Nothing -> Nothing

-- | Find cubic root of a polynomial if it exists
cubicRoot :: Poly -> Maybe Poly
cubicRoot p =
  case getLeadingTerm p of
    Nothing -> Just polyZero
    Just (ltM, _) ->
      if not (monomialDegree ltM `mod` 3 == 0) then Nothing
      else let terms = Map.toList (case p of Poly m -> m)
               -- A component m_i of the root corresponds to a term m_i^3 in the polynomial
               isCubicComponent (m, c) = c == 1 && (monomialDegree m == monomialDegree ltM) && isCubeMonomial m
               components = filter isCubicComponent terms
           in if not (null components)
                 then let root = foldl add polyZero (map (\(m, _) -> Poly (Map.singleton (cubeRootMonomial m) 1)) components)
                      in if p == mul root (mul root root) then Just root else Nothing
                 else Nothing

isCubeMonomial :: Monomial -> Bool
isCubeMonomial (Monomial vars) = all (\e -> e `mod` 3 == 0) (Map.elems vars)

cubeRootMonomial :: Monomial -> Monomial
cubeRootMonomial (Monomial vars) = Monomial (Map.map (`div` 3) vars)
