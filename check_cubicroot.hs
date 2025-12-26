import qualified Data.Map.Strict as Map
import Data.Ratio
import Numeric.Natural

newtype Monomial = Monomial (Map.Map String Natural) deriving (Eq, Show, Ord)
newtype Poly = Poly (Map.Map Monomial Rational) deriving (Eq, Show, Ord)

monomialDegree :: Monomial -> Integer
monomialDegree (Monomial m) = fromIntegral $ Map.foldl (+) 0 m

getLeadingTerm :: Poly -> Maybe (Monomial, Rational)
getLeadingTerm (Poly m)
  | Map.null m = Nothing
  | otherwise = Just $ Map.lookupMax m

isCubeMonomial :: Monomial -> Bool
isCubeMonomial (Monomial vars) = all (\e -> e `mod` 3 == 0) (Map.elems vars)

cubeRootMonomial :: Monomial -> Monomial
cubeRootMonomial (Monomial vars) = Monomial (Map.map (`div` 3) vars)

_addPoly :: Poly -> Poly -> Poly
_addPoly (Poly m1) (Poly m2) = Poly (Map.filter (/= 0) (Map.unionWith (+) m1 m2))

combineMonomial :: Monomial -> Monomial -> Monomial
combineMonomial (Monomial m1) (Monomial m2) = Monomial $ Map.unionWith (+) m1 m2

_mulPoly :: Poly -> Poly -> Poly
_mulPoly (Poly m1) (Poly m2) = Poly (Map.filter (/= 0) (Map.fromListWith (+) [ (combineMonomial mon1 mon2, c1 * c2) | (mon1, c1) <- Map.toList m1, (mon2, c2) <- Map.toList m2 ]))

cubicRoot :: Poly -> Maybe Poly
cubicRoot p =
  case getLeadingTerm p of
    Nothing -> Just (Poly Map.empty)
    Just (ltM, _) ->
      if not (monomialDegree ltM `mod` 3 == 0) then Nothing
      else let terms = Map.toList (case p of Poly m -> m)
               isCubicComponent (m, c) = c == 1 && (monomialDegree m == monomialDegree ltM) && isCubeMonomial m
               components = filter isCubicComponent terms
           in if not (null components)
                 then let root = foldl _addPoly (Poly Map.empty) (map (\(m, _) -> Poly (Map.singleton (cubeRootMonomial m) 1)) components)
                      in if p == _mulPoly root (_mulPoly root root) then Just root else Nothing
                 else Nothing

main = do
  let a = Monomial (Map.singleton "a" 1)
      b = Monomial (Map.singleton "b" 1)
      c = Monomial (Map.singleton "c" 1)
      root = Poly (Map.fromList [(a, 1), (b, 1), (c, 1)])
      p = _mulPoly root (_mulPoly root root)
  print p
  print (cubicRoot p)
