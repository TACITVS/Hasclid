module Solvers.Interval
  ( solveInterval
  ) where

import Expr
import Interval
import qualified Data.Map.Strict as M
import Data.List (foldl')

type Box = M.Map String Interval

-- | Main entry point for Interval Solver
solveInterval :: [Formula] -> Formula -> (Bool, String)
solveInterval theory goal =
  let
    vars = varsInFormula goal ++ concatMap varsInFormula theory
    -- Initial Box: [-1000, 1000] (configurable)
    initBox = M.fromList [ (v, I (-1000) 1000) | v <- vars ]
    
    -- Negate Goal: We want to show (Theory AND Not Goal) is UNSAT
    negGoal = case goal of
                Ge l r -> Lt l r
                Gt l r -> Le l r
                Le l r -> Gt l r
                Lt l r -> Ge l r
                Eq l r -> Not (Eq l r)
                _ -> Not goal
                
    -- Combine Theory and NegGoal into a single set of constraints
    constraints = theory ++ [negGoal]
    
    -- Run Propagation
    finalBox = propagateFixedPoint initBox constraints 20
    
    -- Check if Box is empty (Unsatisfiable)
    isUnsat = isBoxEmpty finalBox
    
  in if isUnsat
     then (True, "Verified by Interval Propagation (UNSAT)")
     else (False, "Potential Counter-example (Box not empty)")

isBoxEmpty :: Box -> Bool
isBoxEmpty box = any isEmpty (M.elems box)
  where 
    isEmpty (I l u) = l > u

-- | Run propagation until fixed point or limit
propagateFixedPoint :: Box -> [Formula] -> Int -> Box
propagateFixedPoint box constraints limit
  | limit <= 0 = box
  | isBoxEmpty box = box
  | otherwise =
      let newBox = foldl' refine box constraints
      in if newBox == box 
         then box -- Fixed point reached
         else propagateFixedPoint newBox constraints (limit - 1)

-- | Refine box based on a single constraint
refine :: Box -> Formula -> Box
refine box f =
  case f of
    -- x >= c
    Ge (Var x) (Const c) -> updateBox box x (I (fromRationalI_Double c) (1/0))
    Gt (Var x) (Const c) -> updateBox box x (I (fromRationalI_Double c) (1/0)) -- Open/Closed ignored for now
    
    -- x <= c
    Le (Var x) (Const c) -> updateBox box x (I (-1/0) (fromRationalI_Double c))
    Lt (Var x) (Const c) -> updateBox box x (I (-1/0) (fromRationalI_Double c))
    
    -- x >= y
    Ge (Var x) (Var y) ->
      let I xl xu = get box x
          I yl yu = get box y
          newX = I (max xl yl) xu
          newY = I yl (min yu xu)
      in updateBox (updateBox box x newX) y newY
      
    -- Simple arithmetic propagation (x + y >= c)
    -- We can implement full HC4 (tree walking) but let's start with top-level patterns
    -- For x >= 1, we handle it.
    -- For sin(x) <= 1, we handle it.
    Le (Sin (Var x)) (Const c) -> refineInverseSin box x (I (-1) (fromRationalI_Double c))
    Ge (Sin (Var x)) (Const c) -> refineInverseSin box x (I (fromRationalI_Double c) 1)
    
    -- General recursive evaluation for pruning
    -- If evaluating f on box yields False, empty the box
    _ -> if not (isPossiblyTrue box f) 
         then emptyBox box -- Pruned entire box!
         else box

updateBox :: Box -> String -> Interval -> Box
updateBox box var newInterval =
  let current = get box var
      intersection = intersect current newInterval
  in case intersection of
       Just i -> M.insert var i box
       Nothing -> M.insert var emptyInterval box

get :: Box -> String -> Interval
get box v = M.findWithDefault fullRange v box

emptyInterval :: Interval
emptyInterval = I 1 0 -- l > u implies empty

emptyBox :: Box -> Box
emptyBox box = M.map (const emptyInterval) box

fromRationalI_Double :: Rational -> Double
fromRationalI_Double r = fromRational r

-- | Refine x based on sin(x) \in Y
refineInverseSin :: Box -> String -> Interval -> Box
refineInverseSin box x yRange =
  let 
    xRange = get box x
    -- Naive: just clamp to domain of asin if yRange is subset of [-1, 1]
    -- But sin is periodic.
    -- For now, if x is small [-1000, 1000], we don't prune much unless yRange is disjoint from [-1, 1].
    validY = intersect yRange (I (-1) 1)
  in case validY of
       Nothing -> emptyBox box -- Impossible sin value
       Just _ -> box -- Logic for periodic inverse is complex, skipping for MVP

-- | Evaluation Logic (Reused)
eval :: Box -> Expr -> Interval
eval box (Var v) = M.findWithDefault fullRange v box
eval box (Const r) = fromRationalI r
eval box (Add a b) = addI (eval box a) (eval box b)
eval box (Sub a b) = subI (eval box a) (eval box b)
eval box (Mul a b) = mulI (eval box a) (eval box b)
eval box (Div a b) = divI (eval box a) (eval box b)
eval box (Pow a n) = powI (eval box a) (fromIntegral n)
eval box (Sqrt a)  = powI (eval box a) 0 
eval box (Sin a)   = sinI (eval box a)
eval box (Cos a)   = cosI (eval box a)
eval box (Tan a)   = tanI (eval box a)
eval box (Asin a)  = asinI (eval box a)
eval box (Acos a)  = acosI (eval box a)
eval box (Atan a)  = atanI (eval box a)
eval _ _ = fullRange 

isPossiblyTrue :: Box -> Formula -> Bool
isPossiblyTrue box f =
  case f of
    Ge l r -> let I _ maxL = eval box l
                  I minR _ = eval box r
              in maxL >= minR
    Gt l r -> let I _ maxL = eval box l
                  I minR _ = eval box r
              in maxL > minR
    Le l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL <= maxR
    Lt l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL < maxR
    Eq l r -> let I minL maxL = eval box l
                  I minR maxR = eval box r
              in maxL >= minR && minL <= maxR
    Not sub -> not (isDefinitelyTrue box sub)
    _ -> True

isDefinitelyTrue :: Box -> Formula -> Bool
isDefinitelyTrue box f =
  case f of
    Ge l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL >= maxR
    Gt l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL > maxR
    Le l r -> let I _ maxL = eval box l
                  I minR _ = eval box r
              in maxL <= minR
    Lt l r -> let I _ maxL = eval box l
                  I minR _ = eval box r
              in maxL < minR
    Eq _ _ -> False
    Not sub -> not (isPossiblyTrue box sub)
    _ -> False
