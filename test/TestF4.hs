module Main where

import Expr
import F4Lite
import BuchbergerOpt (SelectionStrategy(NormalStrategy), sPoly)
import TermOrder (TermOrder(..), compareMonomials)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy)
import Data.Ratio

-- Helper to make monomials
vX, vY, vZ :: String
vX = "x"
vY = "y"
vZ = "z"

-- x > y > z
ord :: Monomial -> Monomial -> Ordering
ord = compareMonomials Lex

-- x^2 + y^2 - 1
p1 :: Poly
p1 = polyAdd (polyAdd (polyPow (polyFromVar vX) 2) (polyPow (polyFromVar vY) 2)) (polyFromConst (-1))

-- x - y
p2 :: Poly
p2 = polyAdd (polyFromVar vX) (polyMul (polyFromVar vY) (polyFromConst (-1)))

isReducibleByReducers :: (Monomial -> Monomial -> Ordering) -> [Poly] -> Poly -> Bool
isReducibleByReducers ord reducers p =
  case getLeadingTermByOrder ord p of
    Nothing -> True -- zero is reducible
    Just (lt, _) -> any (\g -> case getLeadingTermByOrder ord g of
                                 Just (ltG, _) -> monomialDiv lt ltG /= Nothing
                                 Nothing -> False) reducers

main :: IO ()
main = do
  putStrLn "Testing F4Lite..."
  let polys = [p1, p2]
  putStrLn $ "Polys: " ++ show polys
  
  -- Calculate S-polys manually
  let pairs = [(f, g) | f <- polys, g <- polys, f /= g]
  let sPolys = map (uncurry (sPoly ord)) pairs
  putStrLn $ "S-Polys: " ++ show sPolys

  -- Manual Breakdown
  putStrLn "--- Manual Breakdown ---"
  let (colsSet, rows) = buildMacaulayMatrix ord polys sPolys
  let cols = sortBy (\a b -> ord b a) (S.toList colsSet)
  
  putStrLn $ "Cols: " ++ show cols
  putStrLn $ "Rows (Count): " ++ show (length rows)
  
  let rowsReduced = rowEchelonReduce ord cols rows
  putStrLn $ "Rows Reduced (Count): " ++ show (length rowsReduced)
  
  let polysReduced = rowsToPolys cols rowsReduced
  putStrLn "Polys Reduced:"
  mapM_ print polysReduced
  
  let useful = filter (not . isReducibleByReducers ord polys) polysReduced
  putStrLn "Useful Polys:"
  mapM_ print useful
  
  -- Manually trigger f4LiteGroebner with batch=True
  putStrLn "--- f4LiteGroebner ---"
  let result = f4LiteGroebner ord NormalStrategy True polys
  
  putStrLn "Resulting Basis:"
  mapM_ print result
  
  putStrLn "Done."
