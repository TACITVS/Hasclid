module Main where

import System.Environment (getArgs)
import System.CPUTime
import Data.List (isPrefixOf, sort)
import Control.Monad (forM_)
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Project imports
import Expr (Theory, Formula, Expr(..))
import Parser (parseFormulaWithMacros, MacroMap)
import SolverRouter (autoSolve, SolverOptions(..), defaultSolverOptions, GroebnerBackend(..), AutoSolveResult(..), executeSolver, SolverChoice(UseGroebner))
import ProblemAnalyzer (analyzeProblem, ProblemProfile)
import ReplSupport (consumeBalancedScript, stripComment)

-- Simple parser state
data ParseState = ParseState
  { theory :: Theory
  , goal :: Maybe Formula
  , macros :: MacroMap
  }

initialState :: ParseState
initialState = ParseState [] Nothing M.empty

-- Process a single file
runBenchmark :: FilePath -> IO ()
runBenchmark path = do
  content <- readFile path
  let linesOfFile = lines content
  
  -- Pre-process using ReplSupport to handle multi-line commands
  let (finalState, _) = foldl processChunk (initialState, linesOfFile) [1..length linesOfFile] -- hacky iteration limit
  
  case goal finalState of
    Nothing -> printf "Skipping %s: No goal found.\n" path
    Just g -> do
      let t = theory finalState
      printf "Benchmarking %s ...\n" path
      
      let profile = analyzeProblem t g
      
      -- Run Buchberger (Default)
      let optsBuch = defaultSolverOptions { groebnerBackend = BuchbergerBackend, useOptimizedGroebner = True }
      (timeB, resB) <- timeIt $ return $ autoSolve optsBuch M.empty t g
      
      -- Run F4
      let optsF4 = defaultSolverOptions { groebnerBackend = F4Backend }
      (timeF4, resF4) <- timeIt $ return $ autoSolve optsF4 M.empty t g
      
      printf "  Buchberger: %s [%s] (%.6fs)\n" (showResult resB) (show (selectedSolver resB)) timeB
      printf "  F4        : %s [%s] (%.6fs)\n" (showResult resF4) (show (selectedSolver resF4)) timeF4
      
      let speedup = if timeF4 > 0 then timeB / timeF4 else 0
      printf "  Speedup   : %.2fx\n" speedup
      putStrLn ""

processChunk :: (ParseState, [String]) -> Int -> (ParseState, [String])
processChunk (st, []) _ = (st, [])
processChunk (st, lines) _ =
  let (cmd, rest) = consumeBalancedScript lines
  in (processCommand st cmd, rest)

processCommand :: ParseState -> String -> ParseState
processCommand st rawCmd =
  let cmd = stripComment rawCmd
      cleanCmd = dropWhile (== ' ') cmd
      cleanCmd' = filter (/= '\n') cleanCmd -- simple flattening for now
  in
  if null cleanCmd then st
  else if ":assume" `isPrefixOf` cleanCmd then
      let rest = drop 7 cleanCmd
      in case parseFormulaWithMacros (macros st) S.empty rest of
           Right f -> st { theory = theory st ++ [f] }
           Left _ -> st
  else if ":prove" `isPrefixOf` cleanCmd || ":auto" `isPrefixOf` cleanCmd then
       let rest = drop (if ":prove" `isPrefixOf` cleanCmd then 6 else 5) cleanCmd
       in case parseFormulaWithMacros (macros st) S.empty rest of
            Right f -> st { goal = Just f }
            Left _ -> st
  else if ":macro" `isPrefixOf` cleanCmd then
      st -- TODO: Support macros
  else if "(" `isPrefixOf` cleanCmd then
      -- Implicit goal
      case parseFormulaWithMacros (macros st) S.empty cleanCmd of
           Right f -> st { goal = Just f }
           Left _ -> st
  else st

showResult :: AutoSolveResult -> String
showResult res = if isProved res then "PROVED" else "FAILED"

timeIt :: IO a -> IO (Double, a)
timeIt action = do
  start <- getCPUTime
  ret <- action
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  return (diff, ret)

main :: IO ()
main = do
  -- List of hard problems
  let problems = [ "examples/napoleon.euclid"
                 , "examples/erdos_mordell_lemma_Rb.euclid"
                 , "examples/morley.euclid"
                 ]
  
  mapM_ runBenchmark problems
