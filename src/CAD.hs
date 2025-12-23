module CAD
  ( discriminant
  , toRecursive
  , resultant
  , psc
  , leadingCoeff
  , allCoeffs
  , completeProjection
  , mcCallumProjection
  ) where

import Expr
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

-- | Recursive Polynomial: Coefficients are themselves Polys
type RecPoly = [Poly] 

-- =============================================
-- 1. Recursive Structure (Flat -> Recursive)
-- =============================================

toRecursive :: Poly -> String -> RecPoly
toRecursive (Poly m) var = 
    let maxDeg = maximum (0 : [ M.findWithDefault 0 var vars | (Monomial vars, _) <- M.toList m ])
        extractTerm k (Monomial vars, coeff) =
            if M.findWithDefault 0 var vars == fromIntegral k
            then Just (Monomial (M.delete var vars), coeff)
            else Nothing
        getCoeff k = 
            let terms = mapMaybe (extractTerm k) (M.toList m)
            in if null terms then polyZero else Poly (M.fromList terms)
    in [ getCoeff k | k <- [0..fromIntegral maxDeg] ]

-- =============================================
-- 2. Polynomial Arithmetic on Poly Coefficients
-- =============================================

degRec :: RecPoly -> Int
degRec p = length (normalizeRec p) - 1

normalizeRec :: RecPoly -> RecPoly
normalizeRec = dropWhileEnd (== polyZero)

lcRec :: RecPoly -> Poly
lcRec p = case normalizeRec p of
    [] -> polyZero
    xs -> last xs

scaleRec :: Poly -> RecPoly -> RecPoly
scaleRec s = map (polyMul s)

shiftRec :: Int -> RecPoly -> RecPoly
shiftRec k p = replicate k polyZero ++ p

addRec :: RecPoly -> RecPoly -> RecPoly
addRec [] ys = ys
addRec xs [] = xs
addRec (x:xs) (y:ys) = polyAdd x y : addRec xs ys

negRec :: RecPoly -> RecPoly
negRec = map polyNeg

subRec :: RecPoly -> RecPoly -> RecPoly
subRec xs ys = addRec xs (negRec ys)

-- =============================================
-- 3. Pseudo-Division
-- =============================================

pseudoRem :: RecPoly -> RecPoly -> RecPoly
pseudoRem f g = normalizeRec (go f)
  where
    gNorm = normalizeRec g
    dg = degRec gNorm
    l = lcRec gNorm
    go currentF
      | degRec currentF < dg = currentF
      | otherwise = 
          let degCurr = degRec currentF
              lcCurr  = lcRec currentF
              fScaled = scaleRec l currentF
              term    = shiftRec (degCurr - dg) (scaleRec lcCurr gNorm)
              nextF   = subRec fScaled term
          in go (normalizeRec nextF)

-- =============================================
-- 4. Resultant Algorithm (Subresultant)
-- =============================================

resultant :: Poly -> Poly -> String -> Poly
resultant f g var = 
    let _ = trace ("CAD: Resultant w.r.t " ++ var) ()
        rf = toRecursive f var
        rg = toRecursive g var
    in subresultantPRS rf rg

subresultantPRS :: RecPoly -> RecPoly -> Poly
subresultantPRS f g = 
  let f' = normalizeRec f
      g' = normalizeRec g
      _ = trace ("CAD: subresultantPRS degrees " ++ show (degRec f') ++ ", " ++ show (degRec g')) ()
  in if length f' < length g' then subresultantPRS g' f'
     else go f' g' (polyFromConst 1) (polyFromConst 1)
  where
    go r0 r1 g h
      | normalizeRec r1 == [] = polyZero
      | degRec r1 == 0 = 
          let d0 = degRec r0
              l1 = lcRec r1
          in polyPow l1 (fromIntegral d0)
      | otherwise =
          let d0 = degRec r0
              d1 = degRec r1
              l1 = lcRec r1
              delta = fromIntegral (d0 - d1)
              r_prem = pseudoRem r0 r1
              divisor = polyMul g (polyPow h delta)
              r2 = map (\c -> polyDivExact c divisor) r_prem
              g_next = l1
              h_next = if delta == 1 then polyDivExact (polyPow g_next delta) (polyFromConst 1)
                       else polyDivExact (polyPow g_next delta) (polyPow h (fromIntegral (delta - 1)))
          in go r1 r2 g_next h_next

polyDivExact :: Poly -> Poly -> Poly
polyDivExact p1 p2
  | p1 == polyZero = polyZero
  | p2 == polyFromConst 1 = p1
  | otherwise =
      case getLeadingTermByOrder compare p2 of
        Nothing -> error "Division by zero polynomial"
        Just (ltDiv, cDiv) -> 
          let go remainder acc
                | remainder == polyZero = Right acc
                | otherwise =
                    case getLeadingTermByOrder compare remainder of
                      Nothing -> Right acc
                      Just (ltRem, cRem) ->
                        case monomialDiv ltRem ltDiv of
                          Nothing -> Left remainder
                          Just m ->
                            let qCoeff = cRem / cDiv
                                qPoly = polyFromMonomial m qCoeff
                                newRemainder = polySub remainder (polyMul qPoly p2)
                            in go newRemainder (polyAdd acc qPoly)
          in case go p1 polyZero of
               Right q -> q
               Left _ -> p1

polyFromMonomial :: Monomial -> Rational -> Poly
polyFromMonomial m c = Poly (M.singleton m c)

discriminant :: Poly -> String -> Poly
discriminant f var = 
    let _ = trace ("CAD: Discriminant w.r.t " ++ var) ()
        fRec = toRecursive f var
        fPrime = derivRec fRec
        res = subresultantPRS fRec fPrime
    in res

derivRec :: RecPoly -> RecPoly
derivRec [] = []
derivRec (_:xs) = zipWith (\pow coeff -> polyMul (polyFromConst (fromIntegral pow)) coeff) [1 :: Int ..] xs

-- =============================================
-- 5. Principal Subresultant Coefficients (PSC)
-- =============================================

psc :: Poly -> Poly -> String -> [Poly]
psc f g var =
  let rf = toRecursive f var
      rg = toRecursive g var
      prsSequence = subresultantPRSSequence rf rg
  in filter (/= polyZero) prsSequence

subresultantPRSSequence :: RecPoly -> RecPoly -> [Poly]
subresultantPRSSequence f g = go f g []
  where
    go f' g' acc
      | normalizeRec f' == [] = reverse acc
      | normalizeRec g' == [] = reverse (lcRec f' : acc)
      | degRec g' == 0 = reverse (polyPow (lcRec g') (fromIntegral (degRec f')) : acc)
      | otherwise =
          let r = pseudoRem f' g'
              lc_r = if normalizeRec r == [] then polyZero else lcRec r
          in go g' r (lc_r : acc)

-- =============================================
-- 6. Coefficient Projection
-- =============================================

leadingCoeff :: Poly -> String -> Poly
leadingCoeff f var =
  let coeffs = toRecursive f var
  in if null coeffs then polyZero else last coeffs

allCoeffs :: Poly -> String -> [Poly]
allCoeffs f var = filter (/= polyZero) (toRecursive f var)

-- =============================================
-- 7. Collins' Complete Projection
-- =============================================

completeProjection :: [Poly] -> String -> [Poly]
completeProjection polys var =
  let relevantPolys = filter (dependsOn var) polys
      discriminants = [ discriminant p var | p <- relevantPolys, polyDegreeIn p var >= 2 ]
      resultants = [ resultant p q var | p <- relevantPolys, q <- relevantPolys, p /= q ]
      pscPolys = concat [ psc p q var | p <- relevantPolys, q <- relevantPolys, p /= q ]
      leadingCoeffs = [ leadingCoeff p var | p <- relevantPolys ]
      allCoeffPolys = concat [ allCoeffs p var | p <- relevantPolys ]
      allProjected = discriminants ++ resultants ++ pscPolys ++
                     leadingCoeffs ++ allCoeffPolys
  in nub (filter (/= polyZero) allProjected)

-- =============================================
-- 8. McCallum's Optimized Projection
-- =============================================

mcCallumProjection :: [Poly] -> String -> [Poly]
mcCallumProjection polys var =
  let _ = trace ("CAD: mcCallumProjection on " ++ show (length polys) ++ " polys, var: " ++ var) ()
      relevantPolys = filter (dependsOn var) polys
      leadingCoeffs = [ leadingCoeff p var | p <- relevantPolys ]
      discriminants = [ discriminant p var | p <- relevantPolys, polyDegreeIn p var >= 2 ]
      resultants = [ resultant p q var
                   | (i, p) <- zip [0 :: Int ..] relevantPolys
                   , (j, q) <- zip [0 :: Int ..] relevantPolys
                   , i < j  
                   ]
      allProjected = leadingCoeffs ++ discriminants ++ resultants
  in nub (filter (/= polyZero) allProjected)

dependsOn :: String -> Poly -> Bool
dependsOn var p = polyDegreeIn p var > 0

polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  maximum (0 : [ fromIntegral (M.findWithDefault 0 var vars)
               | (Monomial vars, _) <- M.toList m ])

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
