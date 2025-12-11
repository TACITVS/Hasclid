module Main where
import AreaMethod
import Data.Ratio

main :: IO ()
main = do
  putStrLn "Testing Area Method: Midpoint Property (AM^2 - MB^2 = 0)"
  
  -- Construction: M = Midpoint(A, B)
  let steps = [PointMid "M" "A" "B"]
  
  -- Goal: AM^2 - MB^2
  let goal = G_Sub (G_Dist2 "A" "M") (G_Dist2 "M" "B")
  
  let (res, msg) = proveArea steps goal
  putStrLn $ "Result: " ++ show res
  putStrLn $ "Message: " ++ msg
  
  if res then putStrLn "Test PASSED" else putStrLn "Test FAILED"
