{-# LANGUAGE OverloadedStrings #-}
-- Debug script to understand CAD decomposition of xy + 1 > 0

import Expr
import CADLift
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  putStrLn "=== CAD Decomposition Debug for xy + 1 > 0 ==="
  putStrLn ""

  -- Build polynomial: xy + 1
  let x = polyFromVar "x"
      y = polyFromVar "y"
      xy = polyMul x y
      one = polyFromConst 1
      xyPlus1 = polyAdd xy one

  putStrLn "Polynomial: xy + 1"
  putStrLn $ "  " ++ prettyPoly xyPlus1
  putStrLn ""

  -- Decompose with CAD
  let vars = ["x", "y"]  -- Order matters!
      polys = [xyPlus1]
      cells = cadDecompose polys vars

  putStrLn $ "Number of cells generated: " ++ show (length cells)
  putStrLn ""

  -- Examine each cell
  putStrLn "Cell Details:"
  putStrLn "============="
  mapM_ (printCell polys) (zip [1..] cells)

  putStrLn ""
  putStrLn "=== Inequality Check ==="
  let result = evaluateInequalityCAD [] xyPlus1 vars
  putStrLn $ "evaluateInequalityCAD result: " ++ show result
  putStrLn ""

  -- Check if all cells satisfy the inequality
  let satisfying = filter (cellSatisfiesInequality xyPlus1) cells
      failing = filter (not . cellSatisfiesInequality xyPlus1) cells

  putStrLn $ "Cells where xy+1 > 0: " ++ show (length satisfying)
  putStrLn $ "Cells where xy+1 <= 0: " ++ show (length failing)
  putStrLn ""

  when (not $ null failing) $ do
    putStrLn "COUNTEREXAMPLE CELLS (where xy+1 <= 0):"
    mapM_ (printCell polys) (zip [1..] failing)

printCell :: [Poly] -> (Int, (CADCell, SignAssignment)) -> IO ()
printCell polys (num, (cell, signs)) = do
  putStrLn $ "Cell #" ++ show num ++ ":"
  putStrLn $ "  Type: " ++ show (cellType cell)
  putStrLn $ "  Sample point: " ++ show (M.toList $ samplePoint cell)
  putStrLn $ "  Signs: "
  mapM_ (\(p, s) -> putStrLn $ "    " ++ prettyPoly p ++ " -> " ++ show s) (M.toList signs)

  -- Evaluate polynomial at sample point
  let sample = samplePoint cell
  putStrLn $ "  Evaluation at sample:"
  mapM_ (\p -> do
    let evaluated = evaluatePoly sample p
    putStrLn $ "    " ++ prettyPoly p ++ " = " ++ prettyPoly evaluated
    ) polys
  putStrLn ""

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()
