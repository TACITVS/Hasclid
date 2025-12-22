{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  , repl
  , processLine
  ) where

import Expr (Formula(Eq, Ge, Gt, Le, Lt, And), Expr(..), Poly, prettyExpr, prettyFormula, prettyPoly, prettyPolyNice, simplifyExpr, Theory, polyZero, toUnivariate, polyFromConst)
import AreaMethod (Construction, ConstructStep(..), GeoExpr(..), proveArea, exprToGeoExpr, reduceArea, geoToExpr)
import Parser (parseFormulaPrefix, parseFormulaWithRest, parseFormulaWithMacros, parseFormulaWithRestAndMacros, SExpr(..), parseSExpr, tokenizePrefix, MacroMap)
import IntSolver (IntSolveOptions(..))
import Lagrange (solve4Squares)
import Prover (proveTheory, proveTheoryWithCache, proveTheoryWithOptions, buildSubMap, toPolySub, evaluatePoly, ProofTrace, formatProofTrace, buchberger, proveByInduction, promoteIntVars)
import BuchbergerOpt (SelectionStrategy(..), buchbergerWithStrategy)
import CounterExample (findCounterExample, formatCounterExample)
import CAD (discriminant, toRecursive)
import CADLift (cadDecompose, CADCell(..), formatCADCells, evaluateInequalityCAD)
import qualified ReplSupport as RS
import Sturm (isolateRoots, samplePoints, evalPoly)
import Wu (wuProve, wuProveWithTrace, formatWuTrace)
import Data.Bifunctor (first)
import SolverRouter (autoSolve, autoSolveWithTrace, formatAutoSolveResult, isProved, proofReason, selectedSolver, AutoSolveResult(..), SolverOptions(..), defaultSolverOptions, GroebnerBackend(..), executeSolver, SolverChoice(..))
import Error (ProverError(..), formatError)
import Validation (validateTheory, formatWarnings)
import Cache (GroebnerCache, emptyCache, clearCache, getCacheStats, formatCacheStats)
import TermOrder (TermOrder, defaultTermOrder, parseTermOrder, showTermOrder, compareMonomials)
import Preprocessing (preprocess, preprocessGeometry, preprocessedTheory, preprocessedGoal)
import ProblemAnalyzer (analyzeProblem)
import Geometry.WLOG (detectPoints)
import RationalElim (eliminateRational)
import SqrtElim (eliminateSqrt)

import System.IO (hFlush, stdout, stdin, hIsEOF, hIsTerminalDevice, hSetBuffering, BufferMode(..))
import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit, toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Exception as CE
import Control.Exception (try, ArithException(..))
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
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let env = defaultEnv
  result <- try $ case args of
    [] -> do
      putStr "==================================================\n"
      putStrLn "   Hasclid v9.0 - Multi-Solver System"
      putStrLn "   Intelligent routing: Wu/Grobner/CAD + Router"
      putStrLn "   Type :help for commands."
      putStrLn "=================================================="
      repl env (initialState env)
    (fileName:_) -> do
      content <- readFile fileName
      _ <- processScriptStreaming env (initialState env) content
      return ()
  
  case result of
    Left Underflow -> putStrLn "\n[FATAL ERROR] Arithmetic Underflow caught in main loop."
    Left Overflow  -> putStrLn "\n[FATAL ERROR] Arithmetic Overflow caught in main loop."
    Left e         -> putStrLn $ "\n[FATAL ERROR] Math exception caught in main loop: " ++ show e
    Right _        -> return ()

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
  , construction :: Construction -- Area Method Construction
  , intVars :: S.Set String -- Declared Integer Variables
  , pointSubs :: M.Map String Expr -- Point coordinate substitutions (NEW: prevents theory bloat)
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
  , envUseOptimized = True
  , envSelectionStrategy = SugarStrategy
  , envSolverOptions = defaultSolverOptions
  }

type REPLM = ReaderT REPLEnv (ExceptT ProverError IO)

initialState :: REPLEnv -> REPLState
initialState env = 
  REPLState [] [] [] False True emptyCache (envTermOrder env) (envUseOptimized env) (envSelectionStrategy env) M.empty 30 Nothing (envSolverOptions env) [] S.empty M.empty

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r
    showFormula (Le l r) = prettyExpr l ++ " <= " ++ prettyExpr r
    showFormula (Lt l r) = prettyExpr l ++ " < " ++ prettyExpr r

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

insideTriangleConstraints :: String -> String -> String -> String -> ([Formula], M.Map String Expr)
insideTriangleConstraints p a b c =
  let uName = "ba_u"
      vName = "ba_v"
      wName = "ba_w"
      u = Var uName
      v = Var vName
      w = Var wName
      xA = Var ("x" ++ a); yA = Var ("y" ++ a); zA = Var ("z" ++ a)
      xB = Var ("x" ++ b); yB = Var ("y" ++ b); zB = Var ("z" ++ b)
      xC = Var ("x" ++ c); yC = Var ("y" ++ c); zC = Var ("z" ++ c)
      uExpr = Sub (Const 1) (Add v w)
      exprX = Add (Add (Mul u xA) (Mul v xB)) (Mul w xC)
      exprY = Add (Add (Mul u yA) (Mul v yB)) (Mul w yC)
      exprZ = Add (Add (Mul u zA) (Mul v zB)) (Mul w zC)
      constraints =
        [ Ge u (Const 0)
        , Ge v (Const 0)
        , Ge w (Const 0)
        ]
      subs = M.fromList
        [ ("x" ++ p, exprX)
        , ("y" ++ p, exprY)
        , ("z" ++ p, exprZ)
        , (uName, uExpr)
        ]
  in (constraints, subs)

cadFallbackResult :: REPLEnv -> REPLState -> Theory -> Formula -> AutoSolveResult
cadFallbackResult env st fullContext goal =
  let (contextWithAnd, singleGoal) = convertAndGoal fullContext goal
      preprocessResult = preprocess (pointSubs st) contextWithAnd singleGoal
      theory' = preprocessedTheory preprocessResult
      goal' = preprocessedGoal preprocessResult
      profile = analyzeProblem theory' goal'
      opts = currentSolverOptions env st
      pts = detectPoints (goal : fullContext)
      (proved, reason, trace, varDefs) = executeSolver pts UseCAD opts profile theory' goal'
  in AutoSolveResult
       { selectedSolver = UseCAD
       , solverReason = "CAD fallback after floating underflow in auto solver"
       , problemProfile = profile
       , preprocessedGoalExpr = goal'
       , isProved = proved
       , proofReason = reason
       , proofEvidence = Nothing
       , detailedTrace = trace
       , varDefinitions = varDefs
       }

cadFallbackMessage :: REPLEnv -> REPLState -> Theory -> Formula -> Int -> IO String
cadFallbackMessage env st fullContext goal current = do
  maybeMsg <- runWithTimeout current $ do
    let res = cadFallbackResult env st fullContext goal
        msg = "[NUMERICAL ERROR] Arithmetic Underflow in solver. Falling back to CAD.\n" ++
              formatAutoSolveResult res (verbose st)
    _ <- CE.evaluate (length msg)
    return msg
  case maybeMsg of
    Nothing ->
      pure ("[TIMEOUT] exceeded " ++ show current ++ "s during CAD fallback. Use :set-timeout to increase.")
    Just msg ->
      pure msg

chooseBuchberger :: REPLEnv -> REPLState -> ([Poly] -> [Poly])
chooseBuchberger env st = 
  let ord = compareMonomials (termOrder st)
  in if useOptimizedBuchberger st || envUseOptimized env
       then buchbergerWithStrategy ord (selectionStrategy st)
       else buchberger

-- | Expand geometric formulas into separate coordinate equations
-- This fixes the sum-of-squares encoding issue with Groebner bases
expandGeometricFormula :: Formula -> [Formula]
expandGeometricFormula f = 
  case f of
    (Eq (Midpoint a b m) (Const 0)) -> 
      -- Expand: midpoint(A,B,M) = 0  into three coordinate equations:
      -- 2xM - xA - xB = 0, 2yM - yA - yB = 0, 2zM - zA - zB = 0
      let mkCoord prefix = Var (prefix ++ a)
          mkCoordB prefix = Var (prefix ++ b)
          mkCoordM prefix = Var (prefix ++ m)
          two = Const 2
          makeEq prefix = Eq (Sub (Sub (Mul two (mkCoordM prefix)) (mkCoord prefix)) (mkCoordB prefix)) (Const 0)
      in [makeEq "x", makeEq "y", makeEq "z"]

    (Eq (Parallel a b c d) (Const 0)) -> 
      -- Expand: parallel(A,B,C,D) = 0  into cross product = 0
      -- AB × CD = 0 means each component of cross product is 0
      let xa = Var ("x" ++ a); ya = Var ("y" ++ a); za = Var ("z" ++ a)
          xb = Var ("x" ++ b); yb = Var ("y" ++ b); zb = Var ("z" ++ b)
          xc = Var ("x" ++ c); yc = Var ("y" ++ c); zc = Var ("z" ++ c)
          xd = Var ("x" ++ d); yd = Var ("y" ++ d); zd = Var ("z" ++ d)
          -- Vector AB
          ux = Sub xb xa; uy = Sub yb ya; uz = Sub zb za
          -- Vector CD
          vx = Sub xd xc; vy = Sub yd yc; vz = Sub zd zc
          -- Cross product components: AB × CD = (u × v)
          cx = Sub (Mul uy vz) (Mul uz vy)  -- uy*vz - uz*vy
          cy = Sub (Mul uz vx) (Mul ux vz)  -- uz*vx - ux*vz
          cz = Sub (Mul ux vy) (Mul uy vx)  -- ux*vy - uy*vx
      in [Eq cx (Const 0), Eq cy (Const 0), Eq cz (Const 0)]

    _ -> [f]  -- Other formulas pass through unchanged

-- | Expand geometric formula into conjunction for goals
-- For goals, we need a single formula, so we use And to combine components
expandGeometricGoal :: Formula -> Formula
expandGeometricGoal f = 
  case expandGeometricFormula f of
    [single] -> single  -- No expansion needed
    (first:rest) -> foldl And first rest  -- Combine with And
    [] -> f  -- Should not happen

-- | Flatten And formulas into a list of conjuncts
flattenAnd :: Formula -> [Formula]
flattenAnd (And f1 f2) = flattenAnd f1 ++ flattenAnd f2
flattenAnd f = [f]

-- | Convert And goal to theory constraints + single goal
-- If goal is (And f1 (And f2 f3)), convert to theory [f1, f2] and goal f3
convertAndGoal :: Theory -> Formula -> (Theory, Formula)
convertAndGoal theory goal = 
  case flattenAnd goal of
    [] -> (theory, goal)  -- Shouldn't happen
    [single] -> (theory, single)
    formulas -> (theory ++ init formulas, last formulas)

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
      let linesOfFile = filter (not . null) $ filter (not . ("--" `isPrefixOf`)) $ map RS.stripComment $ lines content
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
  | all (\c -> isDigit c || c == '-' || c == '/' || c == '.') s && any isDigit s = 
      let rat = if '/' `elem` s
                then let (n, rest) = span (/= '/') s
                     in read n % read (drop 1 rest)
                else if '.' `elem` s
                     then -- Parse decimal as exact rational (e.g., "1.25" -> 125/100 -> 5/4)
                          let (isNeg, s') = case s of
                                              ('-':rest) -> (True, rest)
                                              _          -> (False, s)
                              (intPart, _:decPart) = span (/= '.') s'
                              numDigits = length decPart
                              denom = 10 ^ numDigits
                              numer = read (intPart ++ decPart) :: Integer
                              numer' = if isNeg then -numer else numer
                          in numer' % denom
                     else fromInteger (read s) % 1
      in Const rat
  | otherwise = case s of
      ('-':rest) -> Mul (Const (-1)) (Var rest)
      _          -> Var s

-- Read additional lines until parentheses are balanced (after stripping comments)
readMultiline :: String -> IO String
readMultiline firstLine = go firstLine (balance firstLine)
  where
    balance = RS.parenBalance . RS.stripComment
    go acc b
      | b <= 0    = pure acc
      | otherwise = do
          more <- getLine
          let acc' = acc ++ "\n" ++ more
          go acc' (b + balance more)

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
  let input = RS.stripComment rawInput
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
      pure (stateWithHist { theory = [], groebnerCache = clearCache (groebnerCache state), intVars = S.empty, pointSubs = M.empty } 
           , "Active Theory reset (Lemmas preserved, Cache cleared, IntVars cleared, Points cleared).")
    (":soft-reset":_) ->
      pure (stateWithHist { theory = [] } 
           , "Active Theory reset (Lemmas preserved, Cache preserved, IntVars preserved).")
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

    (":declare-int":vars) -> do
      let newIntVars = S.union (intVars state) (S.fromList vars)
      pure (stateWithHist { intVars = newIntVars }, "Declared integer variables: " ++ unwords vars)

    (":clear-macros":_) -> 
      pure (stateWithHist { macros = M.empty }, "All macros cleared.")

    (":list-macros":_) -> 
      let ms = macros stateWithHist
      in if M.null ms
           then pure (stateWithHist, "No macros defined.")
           else pure (stateWithHist, unlines ("Defined Macros:" : map formatMacro (M.toList ms)))

    (":macro":_) -> defineMacroCmd stateWithHist (drop 7 input)
    (":defmacro":_) -> defineMacroCmd stateWithHist (drop 10 input)

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

    (":set-gb-backend":name:_) -> 
      let backend = case map toLower name of
                      "f4"         -> Just F4Backend
                      "buchberger" -> Just BuchbergerBackend
                      _            -> Nothing
      in case backend of
           Nothing -> pure (stateWithHist, "Unknown backend. Options: buchberger | f4")
           Just b  -> 
             let opts = solverOptions stateWithHist
                 newOpts = opts { groebnerBackend = b }
                 msg = "Groebner backend set to: " ++ (if b == F4Backend then "f4" else "buchberger")
             in pure (stateWithHist { solverOptions = newOpts }, msg)

    (":set-f4-batch":flag:_) -> 
      let newFlag = map toLower flag `elem` ["on","true","1","yes"]
          opts = solverOptions stateWithHist
          newOpts = opts { f4UseBatch = newFlag }
          msg = "F4 batch reduction " ++ if newFlag then "ENABLED" else "DISABLED"
      in pure (stateWithHist { solverOptions = newOpts }, msg)

    (":set-strategy":name:_) -> 
      case map toLower name of
        "normal" -> 
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = NormalStrategy }
          in pure (stateWithHist { selectionStrategy = NormalStrategy, solverOptions = newOpts }, "Selection strategy set to Normal")
        "sugar" -> 
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = SugarStrategy }
          in pure (stateWithHist { selectionStrategy = SugarStrategy, solverOptions = newOpts }, "Selection strategy set to Sugar")
        "minimal" -> 
          let newOpts = (solverOptions stateWithHist) { selectionStrategyOpt = MinimalStrategy }
          in pure (stateWithHist { selectionStrategy = MinimalStrategy, solverOptions = newOpts }, "Selection strategy set to Minimal")
        _ -> pure (stateWithHist, "Unknown strategy. Options: normal | sugar | minimal")

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

    (":construct":_) -> do
      let cmdStr = drop 10 input
      case parseConstructionStep cmdStr of 
        Right step -> 
           let newConstr = construction state ++ [step]
           in pure (stateWithHist { construction = newConstr }, "Added construction step: " ++ show step)
        Left err -> pure (stateWithHist, "Parse error: " ++ err)

    (":prove-area":_) -> do
      let goalStr = drop 11 input
      case parseGeoExpr goalStr of 
        Right goal -> 
           let (res, msg) = proveArea (construction state) goal
           in pure (stateWithHist, "Area Method Result: " ++ show res ++ "\n" ++ msg)
        Left err -> pure (stateWithHist, "Parse error: " ++ err)

    (":help":_) -> pure (stateWithHist, unlines
      ["Commands:",
       "  :reset                  Reset theory (clear assumptions)",
       "  :soft-reset             Reset theory (keep cache/lemmas)",
       "  :clear                  Full reset",
       "  :list                   Show current theory",
       "  :list-lemmas            Show stored lemmas",
       "  :save-lemmas <file>     Save lemmas to file",
       "  :load-lemmas <file>     Load lemmas from file",
       "  :verbose                Toggle verbose mode",
       "  :auto-simplify          Toggle automatic expression simplification",
       "  :declare-int v1 v2...   Declare variables as integers",
       "  :bruteforce on|off      Toggle bounded brute-force fallback for integer goals",
       "  :set-timeout <seconds>  Set solver timeout (default: 30)",
       "  :show-timeout           Show current timeout setting",
       "  :set-order <order>      Set term ordering (grevlex|lex|gradedlex)",
       "  :show-order             Show current term ordering",
       "  :optimize on|off        Toggle Buchberger optimization",
       "  :set-gb-backend name    Set Groebner backend (buchberger|f4)",
       "  :set-f4-batch on|off    Toggle F4 modular batch reduction",
       "  :set-strategy name      Set selection strategy (normal|sugar|minimal)",
       "  :cache-stats            Show Groebner cache statistics",
       "  :clear-cache            Clear Groebner cache",
       "  :point A x y [z]        Define a point",
       "  :inside P A B C         Constrain point P inside triangle ABC",
       "  :assume (= lhs rhs)     Add assumption",
       "  :lemma (= lhs rhs)      Save a lemma",
       "  :prove (= lhs rhs)      Prove equality",
       "  :wu (= lhs rhs)         Wu's method",
       "  :auto (= lhs rhs)       Automatic solver",
       "  :solve <file>           Solve each formula in file",
       "  :load <file>            Load and execute commands from file",
       "  :history                Show session history",
       "  :quit/:q                Exit"
      ])

    (":point":name:xStr:yStr:zStr:_) -> do
      let exprX = parseCoord xStr
          exprY = parseCoord yStr
          exprZ = parseCoord zStr
          newSubs = M.fromList [("x" ++ name, exprX), ("y" ++ name, exprY), ("z" ++ name, exprZ)]
          mergedSubs = M.union newSubs (pointSubs state)
      pure (stateWithHist { pointSubs = mergedSubs } 
           , "Defined 3D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ", " ++ zStr ++ ")")

    (":point":name:xStr:yStr:_) -> do
      let exprX = parseCoord xStr
          exprY = parseCoord yStr
          exprZ = Const 0
          newSubs = M.fromList [("x" ++ name, exprX), ("y" ++ name, exprY), ("z" ++ name, exprZ)]
          mergedSubs = M.union newSubs (pointSubs state)
      pure (stateWithHist { pointSubs = mergedSubs } 
           , "Defined 2D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ")")

    (":point":_) -> pure (stateWithHist, "Usage: :point A x y z  OR  :point A x y")

    (":inside":p:a:b:c:_) -> do
      let (constraints, subs) = insideTriangleConstraints p a b c
          mergedSubs = M.union subs (pointSubs state)
          newTheory = constraints ++ theory state
      pure (stateWithHist { theory = newTheory, pointSubs = mergedSubs }
           , "Inside-triangle constraints added for " ++ p ++ " in " ++ a ++ b ++ c ++ " (barycentric ba_u/ba_v/ba_w).")

    (":inside":_) -> pure (stateWithHist, "Usage: :inside P A B C")

    (":assume":_) -> 
      let str = drop 8 input
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Right f -> 
             let expanded = expandGeometricFormula f
                 newTheory = expanded ++ theory state
                 msg = if length expanded > 1 
                       then "Assumed (expanded to " ++ show (length expanded) ++ " constraints): " ++ prettyFormula f
                       else "Assumed: " ++ prettyFormula f
             in pure (stateWithHist { theory = newTheory }, msg)
           Left err -> pure (stateWithHist, formatError err)

    (":lemma":_) -> 
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Right f -> pure (stateWithHist { lemmas = f : lemmas state }, "Lemma saved: " ++ prettyFormula f)
           Left err -> pure (stateWithHist, formatError err)

    (":prove":_) -> 
      let str = drop 7 input
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Left err -> pure (stateWithHist, formatError err)
           Right formula -> do
             let expandedFormula = expandGeometricGoal formula
                 fullContext = theory state ++ lemmas state
                 -- Convert And goal to theory + single goal
                 (contextWithAnd, singleGoal) = convertAndGoal fullContext expandedFormula
                 -- Apply preprocessing (including point substitutions)
                 preprocessResult = preprocess (pointSubs state) contextWithAnd singleGoal
                 theory' = preprocessedTheory preprocessResult
                 goal' = preprocessedGoal preprocessResult
                 current = solverTimeout state
                 groebnerFn = chooseBuchberger env stateWithHist
             maybeResult <- liftIO $ runWithTimeout current $ do
               let (proved, reason, trace, cache') = proveTheoryWithOptions groebnerFn (Just (groebnerCache state)) theory' goal'
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
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Left err -> return (stateWithHist, formatError err)
           Right formula -> do
             let expandedFormula = expandGeometricGoal formula
                 fullContext = theory state ++ lemmas state
                 -- Convert And goal to theory + single goal
                 (contextWithAnd, singleGoal) = convertAndGoal fullContext expandedFormula
             case singleGoal of
               Eq _ _ -> do
                 let -- Apply preprocessing (including point substitutions)
                     preprocessResult = preprocess (pointSubs state) contextWithAnd singleGoal
                     theory' = preprocessedTheory preprocessResult
                     goal' = preprocessedGoal preprocessResult
                     current = solverTimeout state
                 maybeResult <- liftIO $ runWithTimeout current $ do
                   let (isProved, reason) = wuProve theory' goal'
                   let traceResult = wuProveWithTrace theory' goal'
                   let traceStr = case traceResult of 
                                   Left _ -> ""
                                   Right wuTrace -> if verbose state then "\n\n" ++ formatWuTrace wuTrace else ""
                   let msg = if isProved 
                             then "WU'S METHOD: PROVED\n" ++ reason ++ traceStr
                             else "WU'S METHOD: NOT PROVED\n" ++ reason ++ traceStr
                   _ <- CE.evaluate (length msg)
                   return (stateWithHist, msg)

                 case maybeResult of 
                   Just res -> pure res
                   Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
               _ -> pure (stateWithHist, "ERROR: Wu's method only supports equality goals (not inequalities)\nUsage: :wu (= expr1 expr2)")

    (":auto":_) -> 
      let str = drop 6 input
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Left err -> pure (stateWithHist, formatError err)
           Right formula -> 
             let expandedFormula = expandGeometricGoal formula
                 fullContext = theory state ++ lemmas state
                 opts = currentSolverOptions env stateWithHist
                 current = solverTimeout state
             in do
               -- NEW: Try Area Method FIRST if a manual construction exists
               (stateAfterArea, goalForAlgebraic) <- 
                  if not (null (construction state))
                  then case exprToGeoExpr expandedFormula of
                         Just geoGoal -> 
                           let reduced = reduceArea (construction state) geoGoal
                           in case reduced of
                                G_Const 0 -> return (Just (stateWithHist, "Area Method: Reduced to 0"), expandedFormula)
                                _ -> -- Fallback to algebraic solver with reduced expression
                                     let reducedExpr = geoToExpr reduced
                                         finalGoal = case expandedFormula of
                                                       Ge _ _ -> Ge reducedExpr (Const 0)
                                                       Gt _ _ -> Gt reducedExpr (Const 0)
                                                       Le _ _ -> Le reducedExpr (Const 0)
                                                       Lt _ _ -> Lt reducedExpr (Const 0)
                                                       _      -> Eq reducedExpr (Const 0)
                                     in return (Nothing, finalGoal)
                         Nothing -> return (Nothing, expandedFormula)
                 else return (Nothing, expandedFormula)
               
               case stateAfterArea of
                 Just res -> pure res
                 Nothing -> do
                   let isVerbose = verbose stateWithHist
                   -- Catch numerical exceptions (underflow/overflow)
                   eres <- liftIO $ try $ runWithTimeout current $ do
                     let res = autoSolve opts (pointSubs stateWithHist) fullContext goalForAlgebraic
                         msg = formatAutoSolveResult res isVerbose
                     _ <- CE.evaluate (length msg)
                     return msg
                   
                   case eres of
                     Left Underflow -> do
                       msg <- liftIO $ cadFallbackMessage env stateWithHist fullContext goalForAlgebraic current
                       pure (stateWithHist, msg)
                     Left Overflow  -> pure (stateWithHist, "[NUMERICAL ERROR] Arithmetic Overflow in solver.")
                     Left _         -> pure (stateWithHist, "[NUMERICAL ERROR] Math exception in solver.")
                     Right Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                     Right (Just msg) -> pure (stateWithHist, msg)

    (":induction":_) -> 
      let str = drop 11 input
      in case parseFormulaWithMacros (macros state) (intVars state) str of
           Left err -> pure (stateWithHist, formatError err)
           Right formula -> do
             let fullContext = theory state ++ lemmas state
             let (proved, reason, _) = proveByInduction fullContext formula
             let msg = (if proved then "INDUCTION: PROVED\n" else "INDUCTION: NOT PROVED\n") ++ reason
             pure (stateWithHist, msg)

    (":solve":filename:_) -> do
      content <- liftIO $ readFile filename
      let formulas = lines content
      results <- liftIO $ mapM (processFormulaLine env stateWithHist) formulas
      let msgs = unlines results
      pure (stateWithHist, msgs)

    (":lagrange":nStr:_) -> 
      if all isDigit nStr then
        let n = read nStr
            sol = solve4Squares n
        in pure (stateWithHist, "Lagrange sum of 4 squares for " ++ nStr ++ ": " ++ show sol)
      else pure (stateWithHist, "Usage: :lagrange <positive_integer>")

    (":load":filename:_) -> do
      content <- liftIO $ readFile filename
      st' <- liftIO $ processScriptStreaming env stateWithHist content
      pure (st', "File loaded: " ++ filename)

    (":q":_) -> pure (stateWithHist, "Goodbye.")
    (":quit":_) -> pure (stateWithHist, "Goodbye.")

    -- If it is not a command, try to parse as a formula and auto-solve
    _ -> case parseFormulaWithMacros (macros state) (intVars state) input of
           Right formula -> 
             let fullContext = theory state ++ lemmas state
                 current = solverTimeout state
             in do
               let isVerbose = verbose state
               -- Catch numerical exceptions (underflow/overflow)
               eres <- liftIO $ try $ runWithTimeout current $ do
                 let res = autoSolve (currentSolverOptions env state) (pointSubs state) fullContext formula
                     msg = formatAutoSolveResult res isVerbose
                 _ <- CE.evaluate (length msg)
                 return msg
               
               case eres of
                 Left Underflow -> do
                   msg <- liftIO $ cadFallbackMessage env stateWithHist fullContext formula current
                   pure (stateWithHist, msg)
                 Left Overflow  -> pure (stateWithHist, "[NUMERICAL ERROR] Arithmetic Overflow in solver.")
                 Left _         -> pure (stateWithHist, "[NUMERICAL ERROR] Math exception in solver.")
                 Right Nothing -> pure (stateWithHist, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                 Right (Just msg) -> pure (stateWithHist, msg)
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
    else 
      (do eof <- CE.try getLine :: IO (Either CE.IOException String)
          case eof of 
            Left _ -> putStrLn "\n[End of input]"
            Right inputLine -> do
              fullInput <- readMultiline inputLine
              if not isTerminal then putStrLn ("> " ++ fullInput) else return ()
              let toks = words fullInput
                  quitCmd = case toks of 
                              (cmd:_) -> cmd `elem` ["exit", "quit", ":q", ":quit"]
                              _       -> False
              if quitCmd 
                then putStrLn "Goodbye."
                else do 
                  result <- tryIO (processLine env state fullInput)
                  case result of 
                    Left err -> do 
                      putStrLn $ "Error: " ++ show err
                      repl env state
                    Right (newState, msg) -> do 
                      unless (null msg) (putStrLn msg)
                      putStrLn ""
                      repl env newState)
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
      else case parseFormulaWithMacros (macros state) (intVars state) trimmed of 
             Left err -> return $ formatError err
             Right formula -> do 
                let fullContext = theory state ++ lemmas state
                    current = solverTimeout state
                    isVerbose = verbose state
                eres <- try $ do
                  let result = autoSolve (currentSolverOptions env state) (pointSubs state) fullContext formula
                      msg = formatAutoSolveResult result isVerbose
                  _ <- CE.evaluate (length msg)
                  return msg
                case eres of
                  Left Underflow -> cadFallbackMessage env state fullContext formula current
                  Left Overflow  -> return "[NUMERICAL ERROR] Arithmetic Overflow in solver."
                  Left _         -> return "[NUMERICAL ERROR] Math exception in solver."
                  Right msg      -> return msg

-- Streaming script processing: prints each result as it is produced
processScriptStreaming :: REPLEnv -> REPLState -> String -> IO REPLState
processScriptStreaming env state content = go state (lines content)
  where
    go st [] = pure st
    go st cmds = do
      let (cmdRaw, rest) = RS.consumeBalancedScript cmds
          trimmed = dropWhile (== ' ') cmdRaw
      (st', msg) <- case trimmed of 
                      "" -> return (st, "")
                      _ | isCommentLine trimmed -> return (st, trimmed)
                      (c:_) | c /= ':' -> do 
                        putStrLn ("> " ++ trimmed)
                        -- Treat as formula line: auto-solve with timeout
                        case parseFormulaWithMacros (macros st) (intVars st) trimmed of 
                          Right formula -> do 
                            let fullContext = theory st ++ lemmas st
                                current = solverTimeout st
                                isVerbose = verbose st
                            
                            -- Catch numerical exceptions (underflow/overflow)
                            eres <- try $ runWithTimeout current $ do
                              let res = autoSolve (currentSolverOptions env st) (pointSubs st) fullContext formula
                                  msg = formatAutoSolveResult res isVerbose
                              _ <- CE.evaluate (length msg)
                              return msg
                            
                            case eres of
                              Left (Underflow) -> do
                                msg <- cadFallbackMessage env st fullContext formula current
                                return (st, msg)
                              Left (Overflow) -> return (st, "[NUMERICAL ERROR] Arithmetic Overflow in solver.")
                              Left _ -> return (st, "[NUMERICAL ERROR] Math exception in solver.")
                              Right Nothing -> return (st, "[TIMEOUT] exceeded " ++ show current ++ "s. Use :set-timeout to increase.")
                              Right (Just msg) -> return (st, msg)
                          Left err -> return (st, formatError err)
                      _ -> do 
                        putStrLn ("> " ++ trimmed)
                        cmdRes <- try (processLine env st trimmed) :: IO (Either ArithException (REPLState, String))
                        case cmdRes of
                          Left e -> return (st, "[NUMERICAL ERROR] Arithmetic exception: " ++ show e)
                          Right res -> return res
      unless (null msg) (putStrLn msg)
      go st' rest

isCommentLine :: String -> Bool
isCommentLine l = 
  ("--" `isPrefixOf` l) || ("//" `isPrefixOf` l) || ("#" `isPrefixOf` l) || (";" `isPrefixOf` l)

-- =============================================
-- Macro Helpers
-- =============================================

defineMacroCmd :: REPLState -> String -> REPLM (REPLState, String)
defineMacroCmd st defStr = do
  let trimmed = dropWhile (== ' ') defStr
  case parseMacroDef (macros st) trimmed of 
    Left err -> pure (st, err)
    Right macros' -> pure (st { macros = macros' }, "Macro defined.")

parseMacroDef :: MacroMap -> String -> Either String MacroMap
parseMacroDef current s = 
  let tokens = tokenizePrefix s
  in case tokens of 
       [] -> Left "Usage: :macro name [params...] = body"
       (nameTok:rest) -> do 
         let (beforeEq, afterEq) = break (== "=") rest
         
         if null afterEq 
           then Left "Expected '=' in macro definition. Usage: :macro name [params...] = body"
           else return ()
           
         let paramTokens = beforeEq
         let bodyTokens = drop 1 afterEq
         
         params <- case paramTokens of 
                     ("(":_) -> do 
                       (sexpr, leftovers) <- first formatError (parseSExpr paramTokens)
                       if not (null leftovers)
                         then Left "Invalid parameter list (trailing tokens)"
                         else case sexpr of 
                                List atoms -> traverse expectAtom atoms
                                _ -> Left "Parameters must be a list of atoms"
                     _ -> return paramTokens
         
         -- Basic validation
         if any (\p -> p == "(" || p == ")") params
           then Left "Invalid parameter names"
           else return ()

         (body, rest2) <- first formatError (parseSExpr bodyTokens)
         if not (null rest2)
           then Left ("Unexpected tokens after macro body: " ++ unwords rest2)
           else Right (M.insert nameTok (params, body) current)
  where
    expectAtom (Atom a) = Right a
    expectAtom _ = Left "Parameters must be atoms."

formatMacro :: (String, ([String], SExpr)) -> String
formatMacro (name, (params, body)) = 
  "  " ++ name ++ " " ++ show params ++ " => " ++ show body

-- =============================================
-- Area Method Parsers
-- =============================================

parseConstructionStep :: String -> Either String ConstructStep
parseConstructionStep s = 
  let tokens = tokenizePrefix s
  in case parseSExpr tokens of 
       Right (List [Atom p, Atom "free"], _) -> Right (PointFree p)
       Right (List [Atom p, Atom "intersection", Atom a, Atom b, Atom c, Atom d], _) -> Right (PointInter p a b c d)
       Right (List [Atom m, Atom "midpoint", Atom a, Atom b], _) -> Right (PointMid m a b)
       Right (List [Atom y, Atom "inter_ang", Atom u, Atom v, t1s, Atom p, Atom q, t2s], _) -> 
         case (sexprToGeo t1s, sexprToGeo t2s) of 
           (Right t1, Right t2) -> Right (PointInterAng y u v t1 p q t2)
           _ -> Left "Invalid tangent expressions"
       Right (List [Atom p, Atom "on-line", Atom a, Atom b, rat], _) -> 
         case sexprToGeo rat of 
           Right r -> Right (PointOnLine p a b r)
           Left err -> Left ("Invalid ratio: " ++ err)
       _ -> Left "Invalid construction syntax. Expected S-Expression e.g. (P intersection A B C D)"

parseGeoExpr :: String -> Either String GeoExpr
parseGeoExpr s = 
  let tokens = tokenizePrefix s
  in case parseSExpr tokens of 
       Right (sexpr, _) -> sexprToGeo sexpr
       Left err -> Left (formatError err)

sexprToGeo :: SExpr -> Either String GeoExpr
sexprToGeo (Atom s) = 
  case parseRatSExpr (Atom s) of 
    Just r -> Right (G_Const r)
    Nothing -> Right (G_Param s)
sexprToGeo (List [Atom "S", Atom a, Atom b, Atom c]) = Right (S_Area a b c)
sexprToGeo (List [Atom "P", Atom a, Atom b, Atom c]) = Right (P_Pyth a b c)
sexprToGeo (List [Atom "dist2", Atom a, Atom b]) = Right (G_Dist2 a b)
sexprToGeo (List [Atom "+", a, b]) = G_Add <$> sexprToGeo a <*> sexprToGeo b
sexprToGeo (List [Atom "-", a, b]) = G_Sub <$> sexprToGeo a <*> sexprToGeo b
sexprToGeo (List [Atom "*", a, b]) = G_Mul <$> sexprToGeo a <*> sexprToGeo b
sexprToGeo (List [Atom "/", a, b]) = G_Div <$> sexprToGeo a <*> sexprToGeo b
sexprToGeo (List [Atom "const", val]) = G_Const <$> (case parseRatSExpr val of Just r -> Right r; Nothing -> Left "Bad number")
sexprToGeo _ = Left "Unknown GeoExpr format"

parseRatSExpr :: SExpr -> Maybe Rational
parseRatSExpr (Atom s) 
  | all (\c -> isDigit c || c == '-' || c == '/') s = 
      if '/' `elem` s
      then let (n, d) = span (/= '/') s in Just (read n % read (drop 1 d))
      else Just (fromInteger (read s) % 1)
  | otherwise = Nothing
parseRatSExpr _ = Nothing
