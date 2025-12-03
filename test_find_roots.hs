-- Test findRootsIn directly
import CADLift
import Expr
import Data.Ratio
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    putStrLn "Testing findRootsIn:"
    putStrLn ""

    -- Test 1: x^2 - 1
    let p1 = polySub (polyPow (polyFromVar "x") 2) (polyFromConst 1)
    putStrLn "Test 1: x^2 - 1"
    let roots1 = findRootsIn "x" p1
    putStrLn $ "Roots: " ++ show roots1
    putStrLn ""

    -- Test 2: x^3 - x
    let p2 = polySub (polyPow (polyFromVar "x") 3) (polyFromVar "x")
    putStrLn "Test 2: x^3 - x"
    let roots2 = findRootsIn "x" p2
    putStrLn $ "Roots: " ++ show roots2
    putStrLn ""

    -- Test 3: x^4 - 5x^2 + 4
    let p3 = polyAdd
               (polySub (polyPow (polyFromVar "x") 4)
                        (polyMul (polyFromConst 5) (polyPow (polyFromVar "x") 2)))
               (polyFromConst 4)
    putStrLn "Test 3: x^4 - 5x^2 + 4"
    let roots3 = findRootsIn "x" p3
    putStrLn $ "Roots: " ++ show roots3
