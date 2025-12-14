module Main where

import System.Environment (getArgs)
import System.CPUTime
import System.Exit
import Control.Monad (forM_)
import Text.Printf (printf)
import System.Process (callProcess, readProcessWithExitCode)
import Data.List (sort)

-- | List of benchmark files in order
benchmarks :: [String]
benchmarks = 
  ["examples/stress_suite/01_Apollonius.euclid"
  ,"examples/stress_suite/02_Varignon.euclid"
  ,"examples/stress_suite/03_Orthocenter.euclid"
  ,"examples/stress_suite/04_NinePointCircle.euclid"
  ,"examples/stress_suite/05_CauchySchwarz.euclid"
  ,"examples/stress_suite/06_TriangleInequality.euclid"
  ,"examples/stress_suite/07_Ptolemy.euclid"
  ,"examples/stress_suite/08_Euler_d2.euclid"
  ,"examples/stress_suite/09_Weitzenbock.euclid"
  ,"examples/stress_suite/10_ErdosMordell_Rb.euclid"
  ]

main :: IO ()
main = do
  printf "==============================================================\n"
  printf "HASCLID STRESS TEST SUITE (10 Levels)\n"
  printf "Timeout: 300s per test\n"
  printf "==============================================================\n"
  
  results <- mapM runTest benchmarks
  
  printf "\n==============================================================\n"
  printf "SUMMARY\n"
  printf "==============================================================\n"
  forM_ (zip benchmarks results) $ \(file, (passed, time, output)) -> do
    let status = if passed then "PASS" else "FAIL"
    printf "% -50s [%s] %.2fs\n" file status time

runTest :: String -> IO (Bool, Double, String)
runTest file = do
  start <- getCPUTime
  -- We use the installed prover executable
  -- Assuming it's in the path or dist-newstyle
  -- Using local copy in root
  let proverCmd = "C:\\Users\\baian\\Haskell\\Prover\\prover.exe"
  
  printf "Running %s... " file
  
  -- Run with timeout is handled by the prover itself via :set-timeout?
  -- Or we can just measure time here.
  -- Haskell's readProcess doesn't easily support timeout.
  -- We rely on the internal timeout of the prover if possible, or just wait.
  -- Since we set :set-timeout 300 in the hardest file, it should be fine.
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode proverCmd [file] ""
  
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  let passed = exitCode == ExitSuccess && "PROVED" `isInfixOf` stdout
  
  printf "%s (%.2fs)\n" (if passed then "PASS" else "FAIL") diff
  return (passed, diff, stdout)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails [] = [[]]
    tails xss@(_:xs) = xss : tails xs
