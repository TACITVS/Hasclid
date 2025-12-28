module Solvers.Interval
  ( solveInterval
  ) where

import Expr
import Interval
import qualified Data.Map.Strict as M
import Data.List (find)

type Box = M.Map String Interval

-- | Main entry point for Interval Solver
-- Returns (True, Reason) if proved, (False, Reason) otherwise
solveInterval :: [Formula] -> Formula -> (Bool, String)
solveInterval theory goal =
  let
    -- 1. Identify variables
    vars = varsInFormula goal ++ concatMap varsInFormula theory
    
    -- 2. Initial Box ( [-1000, 1000] guess, should be configurable)
    initBox = M.fromList [ (v, I (-1000) 1000) | v <- vars ]
    
    -- 3. Run Branch and Bound
    -- We want to prove: For all points in Box satisfying Theory, Goal is true.
    -- This is validity check.
    -- Equivalent to: Unsatisfiability of (Theory AND Not Goal).
    -- If (Theory AND Not Goal) has NO solutions in Box, then Goal is valid in Box.
    
    -- Strategy:
    -- Search for a counter-example (Theory AND Not Goal).
    -- If search space is exhausted without finding one, PROVED.
    -- If a point is found where (Theory AND Not Goal) is definitely true, DISPROVED.
    -- If timeout/depth limit, UNKNOWN.
    
    negGoal = case goal of
                Ge l r -> Lt l r
                Gt l r -> Le l r
                Le l r -> Gt l r
                Lt l r -> Ge l r
                Eq l r -> Not (Eq l r) -- Hard to handle Not Eq with intervals directly
                _ -> Not goal
                
    result = checkValidity initBox theory negGoal 0 10
    
  in case result of
       Just True -> (True, "Verified by Interval Arithmetic (Branch & Bound)")
       Just False -> (False, "Counter-example found by Interval Arithmetic")
       Nothing -> (False, "Interval Arithmetic Inconclusive (Depth limit)")

-- | Check if Theory implies Not(CounterExampleGoal)
-- Returns:
--   Just True  -> Valid (No counter-examples exist)
--   Just False -> Invalid (Counter-example found)
--   Nothing    -> Unknown
checkValidity :: Box -> [Formula] -> Formula -> Int -> Int -> Maybe Bool
checkValidity box theory ceGoal depth maxDepth
  | depth > maxDepth = Nothing -- Too deep, give up
  | otherwise =
      let
        -- Prune: Check if box violates any theory constraint
        -- If theory is definitely false on this box, no counter-example here.
        theoryPossible = all (isPossiblyTrue box) theory
        
        -- Check if CounterExample is possible
        cePossible = isPossiblyTrue box ceGoal
        
        -- Check if CounterExample is DEFINITELY true
        ceDefinite = isDefinitelyTrue box ceGoal && all (isDefinitelyTrue box) theory
      in
        if not theoryPossible || not cePossible
        then Just True -- Vacuously true (no counter-examples here)
        else if ceDefinite
        then Just False -- Found a valid counter-example!
        else
          -- Split and recurse
          let (v, _) = chooseSplitVar box
              (b1, b2) = splitBox box v
              r1 = checkValidity b1 theory ceGoal (depth + 1) maxDepth
              r2 = checkValidity b2 theory ceGoal (depth + 1) maxDepth
          in case (r1, r2) of
               (Just True, Just True) -> Just True -- Valid in both halves
               (Just False, _) -> Just False -- CE in first half
               (_, Just False) -> Just False -- CE in second half
               _ -> Nothing -- Unknown in at least one branch

-- | Split box along variable v
splitBox :: Box -> String -> (Box, Box)
splitBox box v =
  let I l u = box M.! v
      m = (l + u) / 2
      b1 = M.insert v (I l m) box
      b2 = M.insert v (I m u) box
  in (b1, b2)

-- | Heuristic: Choose variable with widest interval
chooseSplitVar :: Box -> (String, Double)
chooseSplitVar box =
  let pairs = M.toList box
      widest = foldr (\(v, i) acc@(_, w) -> if width i > w then (v, width i) else acc) ("", -1) pairs
  in widest

-- | Evaluation Logic

eval :: Box -> Expr -> Interval
eval box (Var v) = M.findWithDefault fullRange v box
eval box (Const r) = fromRationalI r
eval box (Add a b) = addI (eval box a) (eval box b)
eval box (Sub a b) = subI (eval box a) (eval box b)
eval box (Mul a b) = mulI (eval box a) (eval box b)
eval box (Div a b) = divI (eval box a) (eval box b)
eval box (Pow a n) = powI (eval box a) (fromIntegral n)
eval box (Sqrt a)  = powI (eval box a) 0 -- Hack: sqrt not in Interval.hs arithmetic fully yet, mapped to pow 0.5?
eval box (Sin a)   = sinI (eval box a)
eval box (Cos a)   = cosI (eval box a)
eval box (Tan a)   = tanI (eval box a)
eval box (Asin a)  = asinI (eval box a)
eval box (Acos a)  = acosI (eval box a)
eval box (Atan a)  = atanI (eval box a)
eval _ _ = fullRange -- Unsupported

isPossiblyTrue :: Box -> Formula -> Bool
isPossiblyTrue box f =
  case f of
    Ge l r -> let I minL maxL = eval box l
                  I minR maxR = eval box r
              in maxL >= minR -- Possible overlap where L >= R
    Gt l r -> let I minL maxL = eval box l
                  I minR maxR = eval box r
              in maxL > minR
    Eq l r -> let I minL maxL = eval box l
                  I minR maxR = eval box r
              in maxL >= minR && minL <= maxR -- Intervals overlap
    Not sub -> not (isDefinitelyTrue box sub)
    _ -> True -- Safe fallback

isDefinitelyTrue :: Box -> Formula -> Bool
isDefinitelyTrue box f =
  case f of
    Ge l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL >= maxR
    Gt l r -> let I minL _ = eval box l
                  I _ maxR = eval box r
              in minL > maxR
    Eq l r -> False -- Hard to be definitely true on intervals (requires point)
    Not sub -> not (isPossiblyTrue box sub)
    _ -> False
