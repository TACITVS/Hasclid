{-# LANGUAGE DeriveGeneric #-}

module Main where

import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyFormula, prettyPoly, prettyPolyNice, simplifyExpr, Theory, polyZero, toUnivariate, polyFromConst)
import Parser (parseFormulaPrefix, parseFormulaWithRest, parseFormulaWithMacros, parseFormulaWithRestAndMacros, SExpr(..), parseSExpr, tokenizePrefix, MacroMap)
import Prover (proveTheory, proveTheoryWithCache, proveTheoryWithOptions, buildSubMap, toPolySub, evaluatePoly, ProofTrace, formatProofTrace, buchberger, IntSolveOptions(..))
import BuchbergerOpt (SelectionStrategy(..), buchbergerOptimized, buchbergerWithStrategy)
import CounterExample (findCounterExample, formatCounterExample)
import CAD (discriminant, toRecursive)
import CADLift (cadDecompose, CADCell(..), formatCADCells, evaluateInequalityCAD)
import Sturm (isolateRoots, samplePoints, evalPoly)
import Wu (wuProve, wuProveWithTrace, formatWuTrace)
import SolverRouter (autoSolve, autoSolveWithTrace, formatAutoSolveResult, isProved, proofReason, selectedSolver, AutoSolveResult(..), SolverOptions(..), defaultSolverOptions)
import Error (ProverError(..), formatError)
import Validation (validateTheory, formatWarnings)
import Cache (GroebnerCache, emptyCache, clearCache, getCacheStats, formatCacheStats)
import TermOrder (TermOrder, defaultTermOrder, parseTermOrder, showTermOrder)

import System.IO (hFlush, stdout, stdin, hIsEOF)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit, toLower)
import qualified Data.Map.Strict as M
import qualified Control.Exception as CE
import System.Timeout (timeout)

-- =============================================
-- 1. ENTRY POINT
-- =============================================

main :: IO ()
main = do
  putStr "==================================================\n"
  putStrLn "   Hasclid v9.0 - Multi-Solver System"
  putStrLn "   Intelligent routing: Wu/Grobner/CAD + Router"
  putStrLn "   Type :help for commands."
  putStrLn "=================================================="
  repl initialState

-- =============================================
-- 2. REPL State
-- =============================================

data REPLState = REPLState
  { theory :: Theory
  , history :: [String]
  , lemmas :: Theory
  , verbose :: Bool
  , autoSimplify :: Bool
  , groebnerCache :: GroebnerCache
  , termOrder :: TermOrder
  , useOptimizedBuchberger :: Bool
  , selectionStrategy :: SelectionStrategy
  , macros :: MacroMap
  , solverTimeout :: Int  -- Timeout in seconds (default 30)
  , lastTimeoutSeconds :: Maybe Int -- Tracks last timeout to allow automatic retry escalation
  , solverOptions :: SolverOptions
  }

initialState :: REPLState
initialState = REPLState [] [] [] False True emptyCache defaultTermOrder False NormalStrategy M.empty 30 Nothing defaultSolverOptions

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r

-- =============================================
-- 3. Helpers
-- =============================================

serializeLemma :: Formula -> String
serializeLemma (Eq l r) = "(= " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
serializeLemma (Ge l r) = "(>= " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
serializeLemma (Gt l r) = "(> " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"

saveLemmasToFile :: FilePath -> Theory -> IO ()
saveLemmasToFile filename lemmaList = do
  let header = "-- Lemma Library\n-- Saved: " ++ show (length lemmaList) ++ " lemmas\n-- File: " ++ filename ++ "\n\n"
  let content = unlines [ serializeLemma lemma | lemma <- reverse lemmaList ]
  writeFile filename (header ++ content)

loadLemmasFromFile :: FilePath -> IO (Either String Theory)
loadLemmasFromFile filename = do
  result <- tryIO (readFile filename)
  case result of
    Left err -> return $ Left $ "Could not read file: " ++ show err
    Right content -> do
      let linesOfFile = filter (not . null) $ filter (not . ("--" `isPrefixOf`)) $ map stripComment $ lines content
      let parseResults = map parseFormulaPrefix linesOfFile
      let (errors, formulas) = partitionEithers parseResults
      if null errors
        then return $ Right formulas
        else return $ Left $ "Parse errors in lemma file:\n" ++ unlines (map formatError errors)
  where
    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (either left right) ([], [])
      where
        left  a (l, r) = (a:l, r)
        right b (l, r) = (l, b:r)

parseCoord :: String -> Expr
parseCoord s
  | all (\c -> isDigit c || c == '-' || c == '/') s && any isDigit s =
      let rat = if '/' `elem` s
                then let (n, rest) = span (/= '/') s
                     in read n % read (drop 1 rest)
                else fromInteger (read s) % 1
      in Const rat
  | otherwise = case s of
      ('-':rest) -> Mul (Const (-1)) (Var rest)
      _          -> Var s

stripComment :: String -> String
stripComment [] = []
stripComment ('-':'-':_) = []
stripComment (c:cs) = c : stripComment cs

tryIO :: IO a -> IO (Either IOError a)
tryIO action = (Right <$> action) `CE.catch` (\e -> return (Left e))

runWithTimeout :: Int -> IO a -> IO (Maybe a)
runWithTimeout seconds computation = do
  let microseconds = seconds * 1000000
  timeout microseconds computation

-- =============================================
-- 4. Command Processor
-- =============================================

processLine :: REPLState -> String -> IO (REPLState, String)
processLine state rawInput = do
  let input = stripComment rawInput
  let newHist = if null input || ":history" `isPrefixOf` input
                then history state
                else input : history state
  let stateWithHist = state { history = newHist }

  if null (filter (/= ' ') input) then return (state, "")
  else case words input of
    (":clean":_) -> return (stateWithHist, "[screen clear skipped]")
    (":cls":_)   -> return (stateWithHist, "[screen clear skipped]")

    (":history":_) -> do
      let h = reverse (history state)
          formatted = unlines $ zipWith (\i s -> show i ++ ". " ++ s) [1..] h
      return (state, "Session History:\n" ++ formatted)

    (":reset":_) ->
      return (stateWithHist { theory = [], groebnerCache = clearCache (groebnerCache state) }
             , "Active Theory reset (Lemmas preserved, Cache cleared).")
    (":soft-reset":_) ->
      return (stateWithHist { theory = [] }
             , "Active Theory reset (Lemmas preserved, Cache preserved).")
    (":clear":_) ->
      return (initialState { history = newHist }, "Full System Reset.")

    (":verbose":_) -> do
      let newVerbose = not (verbose state)
          msg = if newVerbose
                then "Verbose mode ON: Will show detailed proof explanations"
                else "Verbose mode OFF: Will show only proof results"
      return (stateWithHist { verbose = newVerbose }, msg)

    (":auto-simplify":_) -> do
      let newAutoSimplify = not (autoSimplify state)
          msg = if newAutoSimplify
                then "Auto-simplification ON: Expressions will be simplified automatically"
                else "Auto-simplification OFF: Expressions will be shown in raw form"
      return (stateWithHist { autoSimplify = newAutoSimplify }, msg)

    (":bruteforce":onOff:_) -> do
      let newFlag = map toLower onOff == "on"
          opts = solverOptions stateWithHist
          newOpts = opts { intOptions = (intOptions opts) { allowBruteForce = newFlag } }
          msg = if newFlag
                  then "Bounded brute-force search for integer goals: ON"
                  else "Bounded brute-force search for integer goals: OFF"
      return (stateWithHist { solverOptions = newOpts }, msg)

    (":set-timeout":timeStr:_) ->
      if all isDigit timeStr
      then
        let seconds = read timeStr :: Int
        in if seconds < 1
           then return (stateWithHist, "Timeout must be at least 1 second")
           else return (stateWithHist { solverTimeout = seconds, lastTimeoutSeconds = Nothing }
                       , "Solver timeout set to: " ++ show seconds ++ " seconds")
      else return (stateWithHist, "Usage: :set-timeout <seconds> (e.g., :set-timeout 60)")

    (":show-timeout":_) -> return (stateWithHist, "Current solver timeout: " ++ show (solverTimeout state) ++ " seconds")

    (":cache-stats":_) ->
      return (stateWithHist, formatCacheStats (getCacheStats (groebnerCache state)))

    (":clear-cache":_) ->
      return (stateWithHist { groebnerCache = emptyCache }, "Groebner cache cleared.")

    (":set-order":orderStr:_) ->
      case parseTermOrder orderStr of
        Just order -> return (stateWithHist { termOrder = order }, "Term ordering set to: " ++ showTermOrder order)
        Nothing    -> return (stateWithHist, "Invalid term ordering. Supported: grevlex, lex, gradedlex.")

    (":show-order":_) ->
      return (stateWithHist, "Current term ordering: " ++ showTermOrder (termOrder state))

    (":optimize":onOff:_) ->
      let newFlag = map toLower onOff == "on"
      in return (stateWithHist { useOptimizedBuchberger = newFlag }
               , "Buchberger optimization " ++ if newFlag then "ON" else "OFF")

    (":set-strategy":name:_) ->
      case map toLower name of
        "normal" -> return (stateWithHist { selectionStrategy = NormalStrategy }, "Selection strategy set to Normal")
        "sugar"  -> return (stateWithHist { selectionStrategy = SugarStrategy }, "Selection strategy set to Sugar")
        "minimal" -> return (stateWithHist { selectionStrategy = MinimalStrategy }, "Selection strategy set to Minimal")
        _        -> return (stateWithHist, "Unknown strategy. Options: normal | sugar | minimal")

    (":list":_) -> do
      let th = theory state
      let formatted = prettyTheory th
      return (stateWithHist, "Active Theory:\n" ++ formatted)

    (":list-lemmas":_) -> do
      let formatted = prettyTheory (lemmas state)
      return (stateWithHist, "Stored Lemmas:\n" ++ formatted)

    (":save-lemmas":filename:_) -> do
      saveLemmasToFile filename (lemmas state)
      return (stateWithHist, "Lemmas saved to " ++ filename)

    (":load-lemmas":filename:_) -> do
      loadResult <- loadLemmasFromFile filename
      case loadResult of
        Left err -> return (stateWithHist, err)
        Right lemmasLoaded -> return (stateWithHist { lemmas = lemmasLoaded }, "Lemmas loaded from " ++ filename)

    (":help":_) -> return (stateWithHist, unlines
      [ "Commands:"
      , "  :reset                  Reset theory (clear assumptions)"
      , "  :soft-reset             Reset theory (keep cache/lemmas)"
      , "  :clear                  Full reset"
      , "  :list                   Show current theory"
      , "  :list-lemmas            Show stored lemmas"
      , "  :save-lemmas <file>     Save lemmas to file"
      , "  :load-lemmas <file>     Load lemmas from file"
      , "  :verbose                Toggle verbose mode"
      , "  :auto-simplify          Toggle automatic expression simplification"
      , "  :bruteforce on|off      Toggle bounded brute-force fallback for integer goals"
      , "  :set-timeout <seconds>  Set solver timeout (default: 30)"
      , "  :show-timeout           Show current timeout setting"
      , "  :set-order <order>      Set term ordering (grevlex|lex|gradedlex)"
      , "  :show-order             Show current term ordering"
      , "  :optimize on|off        Toggle Buchberger optimization"
      , "  :set-strategy name      Set selection strategy (normal|sugar|minimal)"
      , "  :cache-stats            Show Groebner cache statistics"
      , "  :clear-cache            Clear Groebner cache"
      , "  :point A x y [z]        Define a point"
      , "  :assume (= lhs rhs)     Add assumption"
      , "  :lemma (= lhs rhs)      Save a lemma"
      , "  :prove (= lhs rhs)      Prove equality"
      , "  :wu (= lhs rhs)         Wu's method"
      , "  :auto (= lhs rhs)       Automatic solver"
      , "  :solve <file>           Solve each formula in file"
      , "  :load <file>            Load and execute commands from file"
      , "  :history                Show session history"
      , "  :quit/:q                Exit"
      ])

    (":point":name:xStr:yStr:zStr:_) -> do
      let exprX = parseCoord xStr
          exprY = parseCoord yStr
          exprZ = parseCoord zStr
          asmX = Eq (Var ("x" ++ name)) exprX
          asmY = Eq (Var ("y" ++ name)) exprY
          asmZ = Eq (Var ("z" ++ name)) exprZ
      return (stateWithHist { theory = asmX : asmY : asmZ : theory state }
             , "Defined 3D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ", " ++ zStr ++ ")")

    (":point":name:xStr:yStr:_) -> do
      let exprX = parseCoord xStr
          exprY = parseCoord yStr
          exprZ = Const 0
          asmX = Eq (Var ("x" ++ name)) exprX
          asmY = Eq (Var ("y" ++ name)) exprY
          asmZ = Eq (Var ("z" ++ name)) exprZ
      return (stateWithHist { theory = asmX : asmY : asmZ : theory state }
             , "Defined 2D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ")")

    (":point":_) -> return (stateWithHist, "Usage: :point A x y z  OR  :point A x y")

    (":assume":_) ->
      let str = drop 8 input
      in case parseFormulaWithMacros (macros state) str of
           Right f -> return (stateWithHist { theory = f : theory state }, "Assumed: " ++ prettyFormula f)
           Left err -> return (stateWithHist, formatError err)

    (":lemma":_) ->
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) str of
           Right f -> return (stateWithHist { lemmas = f : lemmas state }, "Lemma saved: " ++ prettyFormula f)
           Left err -> return (stateWithHist, formatError err)

    (":prove":_) ->
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> return (stateWithHist, formatError err)
           Right formula -> do
             let fullContext = theory state ++ lemmas state
             let (proved, reason, trace, cache') = proveTheoryWithOptions buchberger (Just (groebnerCache state)) fullContext formula
             let msg = (if proved then "RESULT: PROVED\n" else "RESULT: NOT PROVED\n")
                       ++ reason ++
                       (if verbose state then "\n\n" ++ formatProofTrace trace else "")
             return (stateWithHist { groebnerCache = maybe (groebnerCache state) id cache' }, msg)

    (":wu":_) ->
      let str = drop 4 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> return (stateWithHist, formatError err)
           Right formula -> case formula of
             Eq _ _ -> do
               let fullContext = theory state ++ lemmas state
               let (isProved, reason) = wuProve fullContext formula
               let trace = wuProveWithTrace fullContext formula
               let msg = if isProved
                         then "WU'S METHOD: PROVED\n" ++ reason ++ (if verbose state then "\n\n" ++ formatWuTrace trace else "")
                         else "WU'S METHOD: NOT PROVED\n" ++ reason ++ (if verbose state then "\n\n" ++ formatWuTrace trace else "")
               return (stateWithHist, msg)
             _ -> return (stateWithHist, "ERROR: Wu's method only supports equality goals (not inequalities)\nUsage: :wu (= expr1 expr2)")

    (":auto":_) ->
      let str = drop 6 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> return (stateWithHist, formatError err)
           Right formula -> do
             let fullContext = theory state ++ lemmas state
             let current = solverTimeout state
             maybeResult <- runWithTimeout current $ CE.evaluate (autoSolve (solverOptions state) fullContext formula)
             case maybeResult of
               Nothing -> do
                 let bumped = max (current + 1) (current * 2)
                 putStrLn ("⏱️  TIMEOUT: Proof attempt exceeded " ++ show current ++ " seconds. Retrying automatically with " ++ show bumped ++ "s... (use :set-timeout to override)")
                 retry <- runWithTimeout bumped $ CE.evaluate (autoSolve (solverOptions state) fullContext formula)
                 case retry of
                   Nothing ->
                     let failMsg = "⏱️  TIMEOUT: Second attempt also timed out at " ++ show bumped ++ "s. Simplify the problem or increase timeout manually."
                     in return (stateWithHist { solverTimeout = bumped, lastTimeoutSeconds = Just current }, failMsg)
                   Just result2 ->
                     let resMsg = formatAutoSolveResult result2 (verbose state) ++
                                  "\n(Note: timeout auto-increased from " ++ show current ++ "s to " ++ show bumped ++ "s after a previous timeout.)"
                     in return (stateWithHist { solverTimeout = bumped, lastTimeoutSeconds = Just current }, resMsg)
               Just result ->
                 let msg = formatAutoSolveResult result (verbose state)
                 in return (stateWithHist, msg)

    (":solve":filename:_) -> do
      content <- readFile filename
      let formulas = lines content
      results <- mapM (processFormulaLine stateWithHist) formulas
      let msgs = unlines results
      return (stateWithHist, msgs)

    (":load":filename:_) -> do
      content <- readFile filename
      st' <- processScriptStreaming stateWithHist content
      return (st', "File loaded: " ++ filename)

    (":q":_) -> return (stateWithHist, "Goodbye.")
    (":quit":_) -> return (stateWithHist, "Goodbye.")

    -- If it is not a command, try to parse as a formula and auto-solve
    _ -> case parseFormulaWithMacros (macros state) input of
           Right formula -> do
             let fullContext = theory state ++ lemmas state
                 current = solverTimeout state
             maybeResult <- runWithTimeout current $ CE.evaluate (autoSolve (solverOptions state) fullContext formula)
             case maybeResult of
               Nothing -> return (stateWithHist, "⏱️  TIMEOUT: exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
               Just result ->
                 let msg = formatAutoSolveResult result (verbose state)
                 in return (stateWithHist, msg)
           Left err -> return (stateWithHist, formatError err)

-- =============================================
-- 5. REPL Loop
-- =============================================

repl :: REPLState -> IO ()
repl state = do
  putStr "Euclid> "
  hFlush stdout
  isEOF <- hIsEOF stdin
  if isEOF
    then putStrLn "\n[End of input]"
    else do
      eof <- CE.try getLine :: IO (Either CE.IOException String)
      case eof of
        Left _ -> putStrLn "\n[End of input]"
        Right input -> do
          if input `elem` ["exit", "quit", ":q"] then putStrLn "Goodbye."
          else do
            result <- tryIO (processLine state input)
            case result of
              Left err -> do
                putStrLn $ "Error: " ++ show err
                repl state
              Right (newState, msg) -> do
                if null msg then return () else putStrLn msg
                putStrLn ""
                repl newState
      `CE.catch` (\e -> do
          let err = show (e :: CE.SomeException)
          putStrLn $ "\n!!! CRITICAL ERROR !!!"
          putStrLn $ "The system encountered an unexpected error: " ++ err
          putStrLn "The REPL state has been preserved. You may continue, but investigate the last command."
          putStrLn ""
          repl state)

-- =============================================
-- 6. Helpers for :solve
-- =============================================

processFormulaLine :: REPLState -> String -> IO String
processFormulaLine state line =
  let trimmed = dropWhile (== ' ') line
  in if null trimmed || isCommentLine trimmed
       then return ""
       else case parseFormulaPrefix trimmed of
              Left err -> return $ formatError err
              Right formula -> do
                let fullContext = theory state ++ lemmas state
                let result = autoSolve (solverOptions state) fullContext formula
                return (formatAutoSolveResult result (verbose state))

-- Streaming script processing: prints each result as it is produced
processScriptStreaming :: REPLState -> String -> IO REPLState
processScriptStreaming state content = do
  let cmds = lines content
  foldM step state cmds
  where
    step st cmd = do
      let trimmed = dropWhile (== ' ') cmd
      (st', msg) <- case trimmed of
                      "" -> return (st, "")
                      _ | isCommentLine trimmed -> return (st, "")
                      (c:_) | c /= ':' -> do
                        -- Treat as formula line: auto-solve with timeout
                        case parseFormulaPrefix trimmed of
                          Right formula -> do
                            let fullContext = theory st ++ lemmas st
                                current = solverTimeout st
                            maybeRes <- runWithTimeout current $ CE.evaluate (autoSolve (solverOptions st) fullContext formula)
                            case maybeRes of
                              Nothing -> return (st, "⏱️  TIMEOUT: exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                              Just res -> return (st, formatAutoSolveResult res (verbose st))
                          Left err -> return (st, formatError err)
                      _ -> processLine st cmd
      if null msg then return () else putStrLn msg
      return st'

isCommentLine :: String -> Bool
isCommentLine l =
  ("--" `isPrefixOf` l) || ("//" `isPrefixOf` l) || ("#" `isPrefixOf` l) || (";" `isPrefixOf` l)
