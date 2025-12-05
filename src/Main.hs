{-# LANGUAGE DeriveGeneric #-}

module Main where

import Expr (Formula(Eq, Ge, Gt), Expr(..), Poly, prettyExpr, prettyFormula, prettyPoly, prettyPolyNice, simplifyExpr, Theory, polyZero, toUnivariate, polyFromConst)
import Parser (parseFormulaPrefix, parseFormulaWithRest, parseFormulaWithMacros, parseFormulaWithRestAndMacros, SExpr(..), parseSExpr, tokenizePrefix, MacroMap)
import Prover (proveTheory, proveTheoryWithCache, proveTheoryWithOptions, buildSubMap, toPolySub, evaluatePoly, ProofTrace, formatProofTrace, buchberger, IntSolveOptions(..))
import BuchbergerOpt (SelectionStrategy(..), buchbergerWithStrategy)
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

import System.IO (hFlush, stdout, stdin, hIsEOF, hIsTerminalDevice)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit, toLower)
import qualified Data.Map.Strict as M
import qualified Control.Exception as CE
import System.Timeout (timeout)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)

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
  let env = defaultEnv
  repl env (initialState env)

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

data REPLEnv = REPLEnv
  { envTermOrder :: TermOrder
  , envUseOptimized :: Bool
  , envSelectionStrategy :: SelectionStrategy
  , envSolverOptions :: SolverOptions
  } deriving (Show, Eq)

defaultEnv :: REPLEnv
defaultEnv = REPLEnv
  { envTermOrder = defaultTermOrder
  , envUseOptimized = False
  , envSelectionStrategy = NormalStrategy
  , envSolverOptions = defaultSolverOptions
  }

type REPLM = ReaderT REPLEnv (ExceptT ProverError IO)

initialState :: REPLEnv -> REPLState
initialState env =
  REPLState [] [] [] False True emptyCache (envTermOrder env) (envUseOptimized env) (envSelectionStrategy env) M.empty 30 Nothing (envSolverOptions env)

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r

formatHistory :: [String] -> String
formatHistory h =
  "Session History:\n" ++ unlines (zipWith (\i s -> show i ++ ". " ++ s) [1 :: Int ..] (reverse h))

-- =============================================
-- 3. Helpers
-- =============================================

-- Select Gröbner routine and solver options based on current REPL flags
currentSolverOptions :: REPLEnv -> REPLState -> SolverOptions
currentSolverOptions env st =
  (solverOptions st)
    { useOptimizedGroebner = useOptimizedBuchberger st || envUseOptimized env
    , selectionStrategyOpt = selectionStrategy st
    }

chooseBuchberger :: REPLEnv -> REPLState -> ([Poly] -> [Poly])
chooseBuchberger env st =
  if useOptimizedBuchberger st || envUseOptimized env
     then buchbergerWithStrategy (selectionStrategy st)
     else buchberger

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

processLine :: REPLEnv -> REPLState -> String -> IO (REPLState, String)
processLine env st input = do
  res <- runExceptT (runReaderT (processLineM st input) env)
  case res of
    Left err -> pure (st, formatError err)
    Right r -> pure r

processLineM :: REPLState -> String -> REPLM (REPLState, String)
processLineM state rawInput = do
  let input = stripComment rawInput
  let newHist = if null input || ":history" `isPrefixOf` input
                then history state
                else input : history state
  let stateWithHist = state { history = newHist }

  if null (filter (/= ' ') input)
    then pure (state, "")
    else handleCommand state stateWithHist newHist input

handleCommand :: REPLState -> REPLState -> [String] -> String -> REPLM (REPLState, String)
handleCommand state stateWithHist newHist input = do
  env <- ask
  let envAwareInitial = initialState env
  case words input of
    (":clean":_) -> pure (stateWithHist, "[screen clear skipped]")
    (":cls":_)   -> pure (stateWithHist, "[screen clear skipped]")

    (":history":_) ->
      pure (state, formatHistory (history state))

    (":reset":_) ->
      pure (stateWithHist { theory = [], groebnerCache = clearCache (groebnerCache state) }
           , "Active Theory reset (Lemmas preserved, Cache cleared).")
    (":soft-reset":_) ->
      pure (stateWithHist { theory = [] }
           , "Active Theory reset (Lemmas preserved, Cache preserved).")
    (":clear":_) ->
      pure (envAwareInitial { history = newHist }, "Full System Reset.")

    (":verbose":_) -> do
      let newVerbose = not (verbose state)
          msg = if newVerbose
                then "Verbose mode ON: Will show detailed proof explanations"
                else "Verbose mode OFF: Will show only proof results"
      pure (stateWithHist { verbose = newVerbose }, msg)

    (":auto-simplify":_) -> do
      let newAutoSimplify = not (autoSimplify state)
          msg = if newAutoSimplify
                then "Auto-simplification ON: Expressions will be simplified automatically"
                else "Auto-simplification OFF: Expressions will be shown in raw form"
      pure (stateWithHist { autoSimplify = newAutoSimplify }, msg)

    (":bruteforce":onOff:_) -> do
      let newFlag = map toLower onOff == "on"
          opts = solverOptions stateWithHist
          newOpts = opts { intOptions = (intOptions opts) { allowBruteForce = newFlag } }
          msg = if newFlag
                  then "Bounded brute-force search for integer goals: ON"
                  else "Bounded brute-force search for integer goals: OFF"
      pure (stateWithHist { solverOptions = newOpts }, msg)

    (":set-timeout":timeStr:_) ->
      if all isDigit timeStr
      then
        let seconds = read timeStr :: Int
        in if seconds < 1
           then pure (stateWithHist, "Timeout must be at least 1 second")
           else pure (stateWithHist { solverTimeout = seconds, lastTimeoutSeconds = Nothing }
                     , "Solver timeout set to: " ++ show seconds ++ " seconds")
      else pure (stateWithHist, "Usage: :set-timeout <seconds> (e.g., :set-timeout 60)")

    (":show-timeout":_) -> pure (stateWithHist, "Current solver timeout: " ++ show (solverTimeout state) ++ " seconds")

    (":cache-stats":_) ->
      pure (stateWithHist, formatCacheStats (getCacheStats (groebnerCache state)))

    (":clear-cache":_) ->
      pure (stateWithHist { groebnerCache = emptyCache }, "Groebner cache cleared.")

    (":set-order":orderStr:_) ->
      case parseTermOrder orderStr of
        Just order -> pure (stateWithHist { termOrder = order }, "Term ordering set to: " ++ showTermOrder order)
        Nothing    -> pure (stateWithHist, "Invalid term ordering. Supported: grevlex, lex, gradedlex.")

    (":show-order":_) ->
      pure (stateWithHist, "Current term ordering: " ++ showTermOrder (termOrder state))

    (":optimize":onOff:_) ->
      let newFlag = map toLower onOff == "on"
          updatedOpts = (solverOptions stateWithHist) { useOptimizedGroebner = newFlag
                                                     , selectionStrategyOpt = selectionStrategy stateWithHist }
      in pure (stateWithHist { useOptimizedBuchberger = newFlag
                             , solverOptions = updatedOpts }
             , "Buchberger optimization " ++ if newFlag then "ON" else "OFF")

    (":set-strategy":name:_) ->
      case map toLower name of
        "normal" -> 
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = NormalStrategy }
          in pure (stateWithHist { selectionStrategy = NormalStrategy, solverOptions = newOpts }, "Selection strategy set to Normal")
        "sugar"  ->
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = SugarStrategy }
          in pure (stateWithHist { selectionStrategy = SugarStrategy, solverOptions = newOpts }, "Selection strategy set to Sugar")
        "minimal" ->
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = MinimalStrategy }
          in pure (stateWithHist { selectionStrategy = MinimalStrategy, solverOptions = newOpts }, "Selection strategy set to Minimal")
        _        -> pure (stateWithHist, "Unknown strategy. Options: normal | sugar | minimal")

    (":list":_) -> do
      let th = theory state
      let formatted = prettyTheory th
      pure (stateWithHist, "Active Theory:\n" ++ formatted)

    (":list-lemmas":_) -> do
      let formatted = prettyTheory (lemmas state)
      pure (stateWithHist, "Stored Lemmas:\n" ++ formatted)

    (":save-lemmas":filename:_) -> do
      liftIO $ saveLemmasToFile filename (lemmas state)
      pure (stateWithHist, "Lemmas saved to " ++ filename)

    (":load-lemmas":filename:_) -> do
      loadResult <- liftIO $ loadLemmasFromFile filename
      case loadResult of
        Left err -> pure (stateWithHist, err)
        Right lemmasLoaded -> pure (stateWithHist { lemmas = lemmasLoaded }, "Lemmas loaded from " ++ filename)

    (":help":_) -> pure (stateWithHist, unlines
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
      pure (stateWithHist { theory = asmX : asmY : asmZ : theory state }
           , "Defined 3D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ", " ++ zStr ++ ")")

    (":point":name:xStr:yStr:_) -> do
      let exprX = parseCoord xStr
          exprY = parseCoord yStr
          exprZ = Const 0
          asmX = Eq (Var ("x" ++ name)) exprX
          asmY = Eq (Var ("y" ++ name)) exprY
          asmZ = Eq (Var ("z" ++ name)) exprZ
      pure (stateWithHist { theory = asmX : asmY : asmZ : theory state }
           , "Defined 2D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ")")

    (":point":_) -> pure (stateWithHist, "Usage: :point A x y z  OR  :point A x y")

    (":assume":_) ->
      let str = drop 8 input
      in case parseFormulaWithMacros (macros state) str of
           Right f -> pure (stateWithHist { theory = f : theory state }, "Assumed: " ++ prettyFormula f)
           Left err -> pure (stateWithHist, formatError err)

    (":lemma":_) ->
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) str of
           Right f -> pure (stateWithHist { lemmas = f : lemmas state }, "Lemma saved: " ++ prettyFormula f)
           Left err -> pure (stateWithHist, formatError err)

    (":prove":_) ->
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> pure (stateWithHist, formatError err)
           Right formula -> do
             let current = solverTimeout state
                 groebnerFn = chooseBuchberger env stateWithHist
             maybeResult <- liftIO $ runWithTimeout current $ do
               let fullContext = theory state ++ lemmas state
               let (proved, reason, trace, cache') = proveTheoryWithOptions groebnerFn (Just (groebnerCache state)) fullContext formula
               let msg = (if proved then "RESULT: PROVED\n" else "RESULT: NOT PROVED\n")
                         ++ reason ++
                         (if verbose state then "\n\n" ++ formatProofTrace trace else "")
               _ <- CE.evaluate (length msg)
               return (stateWithHist { groebnerCache = maybe (groebnerCache state) id cache' }, msg)
             
             case maybeResult of
               Just res -> pure res
               Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")

    (":wu":_) ->
      let str = drop 4 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> return (stateWithHist, formatError err)
           Right formula -> case formula of
             Eq _ _ -> do
               let current = solverTimeout state
               maybeResult <- liftIO $ runWithTimeout current $ do
                 let fullContext = theory state ++ lemmas state
                 let (isProved, reason) = wuProve fullContext formula
                 let trace = wuProveWithTrace fullContext formula
                 let msg = if isProved
                           then "WU'S METHOD: PROVED\n" ++ reason ++ (if verbose state then "\n\n" ++ formatWuTrace trace else "")
                           else "WU'S METHOD: NOT PROVED\n" ++ reason ++ (if verbose state then "\n\n" ++ formatWuTrace trace else "")
                 _ <- CE.evaluate (length msg)
                 return (stateWithHist, msg)
               
               case maybeResult of
                 Just res -> pure res
                 Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
             _ -> pure (stateWithHist, "ERROR: Wu's method only supports equality goals (not inequalities)\nUsage: :wu (= expr1 expr2)")

    (":auto":_) ->
      let str = drop 6 input
      in case parseFormulaWithMacros (macros state) str of
           Left err -> pure (stateWithHist, formatError err)
           Right formula ->
             let fullContext = theory state ++ lemmas state
                 opts = currentSolverOptions env stateWithHist
                 current = solverTimeout state
             in do
               maybeResult <- liftIO $ runWithTimeout current $ CE.evaluate (autoSolve opts fullContext formula)
               case maybeResult of
                 Nothing -> do
                   let bumped = max (current + 1) (current * 2)
                   liftIO $ putStrLn ("[TIMEOUT] Proof attempt exceeded " ++ show current ++ " seconds. Retrying automatically with " ++ show bumped ++ "s... (use :set-timeout to override)")
                   retry <- liftIO $ runWithTimeout bumped $ CE.evaluate (autoSolve opts fullContext formula)
                   case retry of
                     Nothing ->
                       let failMsg = "[TIMEOUT] Second attempt also timed out at " ++ show bumped ++ "s. Simplify the problem or increase timeout manually."
                       in pure (stateWithHist { solverTimeout = bumped, lastTimeoutSeconds = Just current }, failMsg)
                     Just result2 ->
                       let resMsg = formatAutoSolveResult result2 (verbose state) ++
                                    "\n(Note: timeout auto-increased from " ++ show current ++ "s to " ++ show bumped ++ "s after a previous timeout.)"
                       in pure (stateWithHist { solverTimeout = bumped, lastTimeoutSeconds = Just current }, resMsg)
                 Just result ->
                   let msg = formatAutoSolveResult result (verbose state)
                   in pure (stateWithHist, msg)

    (":solve":filename:_) -> do
      content <- liftIO $ readFile filename
      let formulas = lines content
      results <- liftIO $ mapM (processFormulaLine env stateWithHist) formulas
      let msgs = unlines results
      pure (stateWithHist, msgs)

    (":load":filename:_) -> do
      content <- liftIO $ readFile filename
      st' <- liftIO $ processScriptStreaming env stateWithHist content
      pure (st', "File loaded: " ++ filename)

    (":q":_) -> pure (stateWithHist, "Goodbye.")
    (":quit":_) -> pure (stateWithHist, "Goodbye.")

    -- If it is not a command, try to parse as a formula and auto-solve
    _ -> case parseFormulaWithMacros (macros state) input of
           Right formula ->
             let fullContext = theory state ++ lemmas state
                 current = solverTimeout state
             in do
               maybeResult <- liftIO $ runWithTimeout current $ CE.evaluate (autoSolve (currentSolverOptions env state) fullContext formula)
               case maybeResult of
                 Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                 Just result ->
                   let msg = formatAutoSolveResult result (verbose state)
                   in pure (stateWithHist, msg)
           Left err -> pure (stateWithHist, formatError err)

-- =============================================
-- 5. REPL Loop
-- =============================================

repl :: REPLEnv -> REPLState -> IO ()
repl env state = do
  isTerminal <- hIsTerminalDevice stdin
  if isTerminal then putStr "Euclid> " else return ()
  hFlush stdout
  isEOF <- hIsEOF stdin
  if isEOF
    then putStrLn "\n[End of input]"
    else do
      eof <- CE.try getLine :: IO (Either CE.IOException String)
      case eof of
        Left _ -> putStrLn "\n[End of input]"
        Right input -> do
          if not isTerminal then putStrLn ("> " ++ input) else return ()
          if input `elem` ["exit", "quit", ":q"]
            then putStrLn "Goodbye."
            else do
              result <- tryIO (processLine env state input)
              case result of
                Left err -> do
                  putStrLn $ "Error: " ++ show err
                  repl env state
                Right (newState, msg) -> do
                  unless (null msg) (putStrLn msg)
                  putStrLn ""
                  repl env newState
      `CE.catch` (\e -> do
          let err = show (e :: CE.SomeException)
          putStrLn $ "\n!!! CRITICAL ERROR !!!"
          putStrLn $ "The system encountered an unexpected error: " ++ err
          putStrLn "The REPL state has been preserved. You may continue, but investigate the last command."
          putStrLn ""
          repl env state)

-- =============================================
-- 6. Helpers for :solve
-- =============================================

processFormulaLine :: REPLEnv -> REPLState -> String -> IO String
processFormulaLine env state line =
  let trimmed = dropWhile (== ' ') line
  in if null trimmed || isCommentLine trimmed
       then return ""
       else case parseFormulaPrefix trimmed of
              Left err -> return $ formatError err
              Right formula -> do
                let fullContext = theory state ++ lemmas state
                let result = autoSolve (currentSolverOptions env state) fullContext formula
                return (formatAutoSolveResult result (verbose state))

-- Streaming script processing: prints each result as it is produced
processScriptStreaming :: REPLEnv -> REPLState -> String -> IO REPLState
processScriptStreaming env state content = do
  let cmds = lines content
  foldM step state cmds
  where
    step st cmd = do
      let trimmed = dropWhile (== ' ') cmd
      (st', msg) <- case trimmed of
                      "" -> return (st, "")
                      _ | isCommentLine trimmed -> return (st, trimmed)
                      (c:_) | c /= ':' -> do
                        putStrLn ("> " ++ trimmed)
                        -- Treat as formula line: auto-solve with timeout
                        case parseFormulaPrefix trimmed of
                          Right formula -> do
                            let fullContext = theory st ++ lemmas st
                                current = solverTimeout st
                            maybeRes <- runWithTimeout current $ CE.evaluate (autoSolve (currentSolverOptions env st) fullContext formula)
                            case maybeRes of
                              Nothing -> return (st, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                              Just res -> return (st, formatAutoSolveResult res (verbose st))
                          Left err -> return (st, formatError err)
                      _ -> do
                        putStrLn ("> " ++ trimmed)
                        processLine env st cmd
      if null msg then return () else putStrLn msg
      return st'

isCommentLine :: String -> Bool
isCommentLine l =
  ("--" `isPrefixOf` l) || ("//" `isPrefixOf` l) || ("#" `isPrefixOf` l) || (";" `isPrefixOf` l)
