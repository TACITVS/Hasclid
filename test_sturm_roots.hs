-- Test Sturm root isolation for debugging
import Sturm
import Data.Ratio

main :: IO ()
main = do
    putStrLn "Testing Sturm isolateRoots on various polynomials:"
    putStrLn ""

    -- Test 1: x^2 - 1 (roots at ±1)
    let p1 = [-1, 0, 1] :: [Rational]  -- -1 + 0x + 1x^2
    putStrLn "Test 1: x^2 - 1"
    putStrLn $ "Coefficients: " ++ show p1
    let roots1 = isolateRoots p1
    putStrLn $ "Intervals: " ++ show roots1
    let midpoints1 = [ (lo + hi) / 2 | (lo, hi) <- roots1 ]
    putStrLn $ "Midpoints: " ++ show midpoints1
    putStrLn ""

    -- Test 2: x^3 - x (roots at -1, 0, 1)
    let p2 = [0, -1, 0, 1] :: [Rational]  -- 0 - 1x + 0x^2 + 1x^3
    putStrLn "Test 2: x^3 - x"
    putStrLn $ "Coefficients: " ++ show p2
    let roots2 = isolateRoots p2
    putStrLn $ "Intervals: " ++ show roots2
    let midpoints2 = [ (lo + hi) / 2 | (lo, hi) <- roots2 ]
    putStrLn $ "Midpoints: " ++ show midpoints2
    putStrLn ""

    -- Test 3: x^4 - 5x^2 + 4 (roots at ±1, ±2)
    let p3 = [4, 0, -5, 0, 1] :: [Rational]  -- 4 + 0x - 5x^2 + 0x^3 + 1x^4
    putStrLn "Test 3: x^4 - 5x^2 + 4"
    putStrLn $ "Coefficients: " ++ show p3
    let roots3 = isolateRoots p3
    putStrLn $ "Intervals: " ++ show roots3
    let midpoints3 = [ (lo + hi) / 2 | (lo, hi) <- roots3 ]
    putStrLn $ "Midpoints: " ++ show midpoints3
