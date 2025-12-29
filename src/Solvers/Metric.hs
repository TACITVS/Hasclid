module Solvers.Metric
  ( solveMetric
  ) where

import Expr
import qualified Data.Map.Strict as M
import Data.List (nub)
import Data.Maybe (mapMaybe)

-- | Solve metric inequalities using Triangle Inequality Graph Search
-- Goal: Prove dist(A,B) <= X
-- Method: Find path A -> ... -> B with length L such that L <= X
solveMetric :: [Formula] -> Formula -> (Bool, String)
solveMetric theory goal =
  case extractDistGoal goal of
    Just (a, b, targetExpr, isUpperBound) -> 
        -- If isUpperBound (dist(A,B) <= target), we look for a path P such that len(P) <= target
        if isUpperBound 
        then searchPath a b targetExpr theory
        else (False, "Metric solver only supports upper bound proofs (triangle inequality)")
    Nothing -> (False, "Not a distance inequality")

extractDistGoal :: Formula -> Maybe (String, String, Expr, Bool)
extractDistGoal (Le (Sqrt (Dist2 a b)) e) = Just (a, b, e, True)
extractDistGoal (Ge e (Sqrt (Dist2 a b))) = Just (a, b, e, True)
-- Squared form requires care with signs, assuming positive for now
extractDistGoal (Le (Dist2 a b) e) = Just (a, b, Sqrt e, True) -- Approximate mapping
extractDistGoal (Ge e (Dist2 a b)) = Just (a, b, Sqrt e, True)
extractDistGoal _ = Nothing

searchPath :: String -> String -> Expr -> [Formula] -> (Bool, String)
searchPath start end target theory =
  let
    edges = buildGraph theory
    -- Simple BFS/DFS to find paths
    -- Limit depth to avoid explosion
    paths = findPaths start end edges 3
    
    -- Check if any path <= target
    validPath = findPathLe paths target
  in case validPath of
       Just p -> (True, "Proved by Triangle Inequality: " ++ showPath p)
       Nothing -> (False, "No path found satisfying bound")

type Graph = M.Map String [(String, Expr)]

buildGraph :: [Formula] -> Graph
buildGraph formulas = foldr addEdge M.empty formulas
  where
    addEdge (Eq (Sqrt (Dist2 a b)) e) g = addSymEdge a b e g
    addEdge (Eq e (Sqrt (Dist2 a b))) g = addSymEdge a b e g
    -- Handle squared dists if e is square?
    addEdge _ g = g
    
    addSymEdge u v w g = 
      let g' = M.insertWith (++) u [(v, w)] g
      in M.insertWith (++) v [(u, w)] g'

findPaths :: String -> String -> Graph -> Int -> [[(String, String, Expr)]]
findPaths curr target graph depth
  | depth == 0 = []
  | curr == target = [[]]
  | otherwise =
      case M.lookup curr graph of
        Nothing -> []
        Just neighbors ->
          [ (curr, next, w) : path
          | (next, w) <- neighbors
          , path <- findPaths next target graph (depth - 1)
          ]

findPathLe :: [[(String, String, Expr)]] -> Expr -> Maybe [(String, String, Expr)]
findPathLe paths limit =
  let 
    check p = 
      let len = foldr (Add . (\(_,_,w) -> w)) (Const 0) p
          -- Check len <= limit
          -- Use simple constant checking or maybe Modular?
          -- For now, just symbolic equality check if simpler
          diff = simplifyExpr (Sub limit len)
      in isNonNegative diff
  in case filter check paths of
       (p:_) -> Just p
       [] -> Nothing

isNonNegative :: Expr -> Bool
isNonNegative (Const c) = c >= 0
isNonNegative (IntConst i) = i >= 0
isNonNegative _ = False -- Weak check

showPath :: [(String, String, Expr)] -> String
showPath [] = ""
showPath ((u,v,_):xs) = u ++ " -> " ++ showPath' v xs
  where showPath' lastP [] = lastP
        showPath' _ ((_,next,_):ys) = next ++ " -> " ++ showPath' next ys
