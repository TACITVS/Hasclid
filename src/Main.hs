{-# LANGUAGE DeriveGeneric #-}

module Main where

import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyPoly, prettyPolyNice, simplifyExpr, Theory, polyZero, toUnivariate, polyFromConst)
import Parser (parseFormulaPrefix, parseFormulaWithRest)
import Prover (proveTheory, proveTheoryWithCache, proveTheoryWithOptions, buildSubMap, toPolySub, evaluatePoly, ProofTrace, formatProofTrace, buchberger)
import BuchbergerOpt (SelectionStrategy(..), buchbergerOptimized, buchbergerWithStrategy)
import CounterExample (findCounterExample, formatCounterExample)
import CAD (discriminant, toRecursive)
import CADLift (cadDecompose, CADCell(..), formatCADCells, evaluateInequalityCAD)
import Sturm (isolateRoots, samplePoints, evalPoly)
import Wu (wuProve, wuProveWithTrace, formatWuTrace)
import SolverRouter (autoSolve, formatAutoSolveResult, isProved, proofReason, selectedSolver, AutoSolveResult(..))
import Error (ProverError(..), formatError)
import Validation (validateTheory, formatWarnings)
import Cache (GroebnerCache, emptyCache, clearCache, getCacheStats, formatCacheStats)
import TermOrder (TermOrder, defaultTermOrder, parseTermOrder, showTermOrder)

import System.IO (hFlush, stdout)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import qualified Control.Exception as CE

-- =============================================
-- 1. ENTRY POINT
-- =============================================

main :: IO ()
main = do
  putStr "\ESC[2J\ESC[H"
  putStrLn "=================================================="
  putStrLn "   Euclid Geometric Prover v8.0"
  putStrLn "   Now with Complete CAD Lifting!"
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
  }

initialState :: REPLState
initialState = REPLState [] [] [] False True emptyCache defaultTermOrder False NormalStrategy
  -- verbose off, autoSimplify on, empty cache, GrevLex, optimization off, NormalStrategy

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r

-- =============================================
-- 3. Helpers
-- =============================================

-- Lemma File I/O
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
  
    (":clean":_) -> do
        putStr "\ESC[2J\ESC[H"
        hFlush stdout
        return (stateWithHist, "")
        
    (":cls":_) -> do 
        putStr "\ESC[2J\ESC[H"
        hFlush stdout
        return (stateWithHist, "")

    (":history":_) -> do
        let h = reverse (history state)
        let formatted = unlines $ zipWith (\i s -> show i ++ ". " ++ s) [1..] h
        return (state, "Session History:\n" ++ formatted)

    (":reset":_) -> return (stateWithHist { theory = [], groebnerCache = clearCache (groebnerCache state) }, "Active Theory reset (Lemmas preserved, Cache cleared).")
    (":clear":_) -> return (initialState { history = newHist }, "Full System Reset.")

    (":verbose":_) -> do
        let newVerbose = not (verbose state)
        let msg = if newVerbose
                  then "Verbose mode ON: Will show detailed proof explanations"
                  else "Verbose mode OFF: Will show only proof results"
        return (stateWithHist { verbose = newVerbose }, msg)

    (":auto-simplify":_) -> do
        let newAutoSimplify = not (autoSimplify state)
        let msg = if newAutoSimplify
                  then "Auto-simplification ON: Expressions will be simplified automatically"
                  else "Auto-simplification OFF: Expressions will be shown in raw form"
        return (stateWithHist { autoSimplify = newAutoSimplify }, msg)

    (":list":_)  -> do
        let tStr = if null (theory state) then "  (None)" else prettyTheory (theory state)
        let lStr = if null (lemmas state) then "  (None)" else prettyTheory (lemmas state)
        return (stateWithHist, "Active Assumptions:\n" ++ tStr ++ "\nProven Lemmas:\n" ++ lStr)

    (":validate":_) -> do
        let warnings = validateTheory (theory state)
        return (stateWithHist, formatWarnings warnings)

    (":cache-stats":_) -> do
        let stats = getCacheStats (groebnerCache state)
        return (stateWithHist, formatCacheStats stats)

    (":cache-clear":_) -> do
        let newCache = clearCache (groebnerCache state)
        return (stateWithHist { groebnerCache = newCache }, "Cache cleared.")

    (":set-order":orderStr:_) -> do
        case parseTermOrder orderStr of
          Nothing -> return (stateWithHist, "Invalid term order. Use: lex, grlex, or grevlex")
          Just newOrder -> do
            let newCache = clearCache (groebnerCache state)  -- Clear cache when order changes
            let msg = "Term order set to: " ++ showTermOrder newOrder ++ "\n(Cache cleared)"
            return (stateWithHist { termOrder = newOrder, groebnerCache = newCache }, msg)

    (":show-order":_) -> do
        let current = showTermOrder (termOrder state)
        return (stateWithHist, "Current term ordering: " ++ current)

    (":optimize":onOff:_) -> do
        case onOff of
          "on"  -> return (stateWithHist { useOptimizedBuchberger = True },
                          "Optimized Buchberger enabled (2-5x faster with criteria)")
          "off" -> return (stateWithHist { useOptimizedBuchberger = False },
                          "Optimized Buchberger disabled (using standard algorithm)")
          _     -> return (stateWithHist, "Usage: :optimize on|off")

    (":strategy":stratStr:_) -> do
        let parseStrategy s = case map toLowerChar s of
              "normal"  -> Just NormalStrategy
              "sugar"   -> Just SugarStrategy
              "minimal" -> Just MinimalStrategy
              _         -> Nothing
            toLowerChar c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
                          | otherwise = c
        case parseStrategy stratStr of
          Nothing -> return (stateWithHist, "Invalid strategy. Use: normal, sugar, or minimal")
          Just newStrat -> return (stateWithHist { selectionStrategy = newStrat },
                                   "Selection strategy set to: " ++ show newStrat)

    (":show-buchberger":_) -> do
        let optStatus = if useOptimizedBuchberger state then "ON (optimized)" else "OFF (standard)"
        let strategyStr = show (selectionStrategy state)
        return (stateWithHist, unlines [
            "Buchberger Configuration:",
            "  Optimization: " ++ optStatus,
            "  Strategy: " ++ strategyStr
          ])

    (":help":_)  -> return (stateWithHist, unlines [
        "--- Euclid Geometric Prover Commands ---",
        "",
        "Geometry Basics:",
        "  :point A x y z          Define 3D point (or x y for 2D)",
        "  :assume (= xA 0)        Add assumption",
        "  :validate               Check for degenerate configurations",
        "",
        "Geometric Constraints:",
        "  (dist2 A B)             Squared distance between points",
        "  (midpoint A B M)        M is midpoint of AB",
        "  (perpendicular A B C D) AB ⊥ CD (perpendicular)",
        "  (parallel A B C D)      AB ∥ CD (parallel)",
        "  (collinear A B C)       A, B, C are collinear",
        "  (dot A B C D)           Dot product of vectors AB and CD",
        "  (circle P C r)          Point P on circle with center C and radius r",
        "",
        "Logic & Proving:",
        "  (= expr1 expr2)         Prove equality (uses Gröbner basis)",
        "  :auto (= expr1 expr2)   Auto-select best solver (RECOMMENDED)",
        "  :wu (= expr1 expr2)     Prove using Wu's Method (faster for geometry)",
        "  :lemma (= a b)          Prove and store theorem",
        "  :find-counterexample (= a b)  Find counter-example for failed proof",
        "  :verbose                Toggle detailed proof explanations",
        "  :auto-simplify          Toggle automatic expression simplification",
        "  :simplify (expr)        Manually simplify an expression",
        "",
        "Lemma Libraries:",
        "  :save-lemmas file.lemmas  Save proven lemmas to file",
        "  :load-lemmas file.lemmas  Load lemmas from file",
        "  :clear-lemmas             Clear all stored lemmas",
        "",
        "Advanced (CAD for Inequalities):",
        "  :project expr var       Compute CAD Shadow (Discriminant)",
        "  :cad x [y [z]]          Cylindrical Algebraic Decomposition",
        "  :solve (> p 0) x        Solve inequality (1D)",
        "  :solve (> p 0) x y      Solve inequality (2D Lifting)",
        "",
        "Caching & Performance:",
        "  :cache-stats            Show cache statistics",
        "  :cache-clear            Clear Gröbner basis cache",
        "",
        "Term Ordering (affects Gröbner basis performance):",
        "  :set-order lex          Use lexicographic ordering",
        "  :set-order grlex        Use graded lexicographic ordering",
        "  :set-order grevlex      Use graded reverse lex (default, fastest)",
        "  :show-order             Show current term ordering",
        "",
        "Buchberger Optimization (2-5x speedup with criteria):",
        "  :optimize on            Enable optimized Buchberger algorithm",
        "  :optimize off           Use standard Buchberger (default)",
        "  :strategy normal        Use normal selection strategy",
        "  :strategy sugar         Use sugar selection strategy",
        "  :strategy minimal       Use minimal degree strategy",
        "  :show-buchberger        Show current optimization settings",
        "",
        "Utilities:",
        "  :load file.euclid       Run script",
        "  :list, :history, :clean, :reset, :verbose, :auto-simplify, :validate, :q"
        ])

    -- COMMAND: :solve <Formula> <Var1> [Var2]
    (":solve":rest) -> do
       let inputStr = unwords rest

       case parseFormulaWithRest inputStr of
         Left err -> return (stateWithHist, formatError err)
         Right (formula, vars) -> do
           
           if null vars then return (stateWithHist, "Usage: :solve (formula) x [y]")
           else do
             -- Extract polynomial
             let (poly, isGt) = case formula of
                                  Gt l r -> (toPolySub (buildSubMap (theory state)) (Sub l r), True)
                                  Ge l r -> (toPolySub (buildSubMap (theory state)) (Sub l r), True)
                                  Eq l r -> (toPolySub (buildSubMap (theory state)) (Sub l r), False)
                                  -- FIXED: Removed redundant pattern match

             case vars of
                 -- 1D CASE
                 [v] -> do
                     let recPoly = toRecursive poly v
                     let coeffs = map (\p -> case toUnivariate p of Just (_, [c]) -> c; Just (_, []) -> 0; _ -> 0) recPoly
                     let samples = samplePoints coeffs
                     let solutions = filter (\x -> let val = evalPoly coeffs x in if isGt then val > 0 else val == 0) samples
                     return (stateWithHist, "1D Solutions (" ++ v ++ "):\n" ++ show solutions)

                 -- 2D CASE (Lifting)
                 [vx, vy] -> do
                     -- 1. Project x onto y (Discriminant)
                     let shadow = discriminant poly vx
                     
                     -- 2. Solve for y samples (Base Phase)
                     let Just (_, shadowCoeffs) = toUnivariate shadow
                     let ySamples = samplePoints shadowCoeffs
                     
                     -- 3. Lift: Check x for each y sample
                     let results = flip map ySamples $ \yVal -> do
                             let yPoly = polyFromConst yVal
                             let subMap = M.singleton vy yPoly
                             let xPoly = evaluatePoly subMap poly
                             
                             let Just (_, xCoeffs) = toUnivariate xPoly
                             let xSamples = samplePoints xCoeffs
                             
                             let validX = filter (\x -> let val = evalPoly xCoeffs x in if isGt then val > 0 else val == 0) xSamples
                             
                             if null validX then Nothing else Just (yVal, validX)
                             
                     let validRegions = filter (\x -> case x of Nothing -> False; _ -> True) results
                     
                     return (stateWithHist, "2D CAD Solution Regions (y, [valid x samples]):\n" ++ show validRegions)

                 _ -> return (stateWithHist, "Only 1D and 2D solving supported currently.")

    (":lemma":_) ->
        let str = drop 7 input
        in case parseFormulaPrefix str of
             Left err -> return (stateWithHist, formatError err)
             Right formula -> do
               let fullContext = theory state ++ lemmas state
               let buchbergerFunc = if useOptimizedBuchberger state
                                    then buchbergerWithStrategy (selectionStrategy state)
                                    else buchberger
               let (isProved, reason, trace, newCache) = proveTheoryWithOptions buchbergerFunc (Just (groebnerCache state)) fullContext formula
               if isProved
                 then do
                   let msg = "LEMMA ESTABLISHED: " ++ reason ++ "\n(Saved)" ++
                             (if verbose state then "\n\n" ++ formatProofTrace trace else "")
                   let updatedState = stateWithHist { lemmas = formula : lemmas state,
                                                       groebnerCache = maybe (groebnerCache state) id newCache }
                   return (updatedState, msg)
                 else return (stateWithHist, "LEMMA FAILED: " ++ reason)

    (":find-counterexample":_) ->
        let str = drop 20 input
        in case parseFormulaPrefix str of
             Left err -> return (stateWithHist, formatError err)
             Right formula -> do
               let fullContext = theory state ++ lemmas state
               case findCounterExample fullContext formula of
                 Just ce -> return (stateWithHist, formatCounterExample ce)
                 Nothing -> return (stateWithHist, "No counter-example found with simple value sampling.\nThis does NOT mean the formula is true - try a proof instead.")

    (":save-lemmas":filename:_) -> do
        if null (lemmas state)
          then return (stateWithHist, "No lemmas to save.")
          else do
            saveLemmasToFile filename (lemmas state)
            return (stateWithHist, "Saved " ++ show (length (lemmas state)) ++ " lemmas to " ++ filename)

    (":save-lemmas":_) -> return (stateWithHist, "Usage: :save-lemmas filename.lemmas")

    (":load-lemmas":filename:_) -> do
        result <- loadLemmasFromFile filename
        case result of
          Left err -> return (stateWithHist, "Error loading lemmas: " ++ err)
          Right newLemmas -> do
            let combined = newLemmas ++ lemmas state
            return (stateWithHist { lemmas = combined },
                    "Loaded " ++ show (length newLemmas) ++ " lemmas from " ++ filename ++
                    "\nTotal lemmas: " ++ show (length combined))

    (":load-lemmas":_) -> return (stateWithHist, "Usage: :load-lemmas filename.lemmas")

    (":clear-lemmas":_) -> do
        let count = length (lemmas state)
        if count == 0
          then return (stateWithHist, "No lemmas to clear.")
          else return (stateWithHist { lemmas = [] }, "Cleared " ++ show count ++ " lemmas.")

    (":simplify":_) ->
        let str = drop 10 input
        in if null (filter (/= ' ') str)
           then return (stateWithHist, "Usage: :simplify (expression)")
           else do
             let parseInput = "(= " ++ str ++ " 0)"
             case parseFormulaPrefix parseInput of
               Left err -> return (stateWithHist, formatError err)
               Right (Eq e _) -> do
                 let simplified = simplifyExpr e
                 let poly = toPolySub (buildSubMap (theory state)) simplified
                 return (stateWithHist,
                         "Original:    " ++ prettyExpr e ++ "\n" ++
                         "Simplified:  " ++ prettyExpr simplified ++ "\n" ++
                         "As Polynomial: " ++ prettyPolyNice poly)
               _ -> return (stateWithHist, "Parse Error: Could not parse expression")

    (":project":args) -> do
         if null args
           then return (stateWithHist, "Usage: :project (expression) var")
           else do
             let varStr = last args
             let exprParts = init args
             if null exprParts
               then return (stateWithHist, "Usage: :project (expression) var")
               else do
                 let exprStr = unwords exprParts
                 let parseInput = "(= " ++ exprStr ++ " 0)"
                 case parseFormulaPrefix parseInput of
                   Right (Eq e _) -> do
                       let poly = toPolySub (buildSubMap (theory state ++ lemmas state)) e
                       let shadow = discriminant poly varStr
                       return (stateWithHist, "Projection (Discriminant w.r.t " ++ varStr ++ "):\n" ++ prettyPoly shadow)
                   Left err -> return (stateWithHist, formatError err)
                   _ -> return (stateWithHist, "Projection Failed: Unknown logic error.")

    (":cad":vars) -> do
         if null vars
           then return (stateWithHist, "Usage: :cad x [y [z]] - Decompose space using CAD")
           else do
             let fullContext = theory state ++ lemmas state
             let subM = buildSubMap fullContext
             let constraintPolys = [ toPolySub subM (Sub l r) | Eq l r <- fullContext ]
             let cells = cadDecompose constraintPolys vars
             let output = "CAD Decomposition (" ++ show (length cells) ++ " cells):\n\n" ++
                          formatCADCells cells
             return (stateWithHist, output)

    (":wu":_) ->
        let str = drop 4 input
        in case parseFormulaPrefix str of
             Left err -> return (stateWithHist, formatError err)
             Right formula -> do
               case formula of
                 Eq _ _ -> do
                   let fullContext = theory state ++ lemmas state
                   let trace = wuProveWithTrace fullContext formula
                   let (isProved, reason) = wuProve fullContext formula
                   if isProved
                     then do
                       let msg = "WU'S METHOD: PROVED\n" ++ reason ++
                                 (if verbose state then "\n\n" ++ formatWuTrace trace else "")
                       return (stateWithHist, msg)
                     else do
                       let msg = "WU'S METHOD: NOT PROVED\n" ++ reason ++
                                 (if verbose state then "\n\n" ++ formatWuTrace trace else "")
                       return (stateWithHist, msg)
                 _ -> return (stateWithHist, "ERROR: Wu's method only supports equality goals (not inequalities)\nUsage: :wu (= expr1 expr2)")

    (":auto":_) ->
        let str = drop 6 input
        in case parseFormulaPrefix str of
             Left err -> return (stateWithHist, formatError err)
             Right formula -> do
               let fullContext = theory state ++ lemmas state
               let result = autoSolve fullContext formula
               let msg = formatAutoSolveResult result (verbose state)
               return (stateWithHist, msg)

    (":point":name:xStr:yStr:zStr:_) -> do
         let exprX = parseCoord xStr
         let exprY = parseCoord yStr
         let exprZ = parseCoord zStr
         let asmX = Eq (Var ("x" ++ name)) exprX
         let asmY = Eq (Var ("y" ++ name)) exprY
         let asmZ = Eq (Var ("z" ++ name)) exprZ
         return (stateWithHist { theory = asmX : asmY : asmZ : theory state }, 
                 "Defined 3D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ", " ++ zStr ++ ")")
    
    (":point":name:xStr:yStr:_) -> do
         let exprX = parseCoord xStr
         let exprY = parseCoord yStr
         let exprZ = Const 0
         let asmX = Eq (Var ("x" ++ name)) exprX
         let asmY = Eq (Var ("y" ++ name)) exprY
         let asmZ = Eq (Var ("z" ++ name)) exprZ
         return (stateWithHist { theory = asmX : asmY : asmZ : theory state }, 
                 "Defined 2D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ")")
    
    (":point":_) -> return (stateWithHist, "Usage: :point A x y z  OR  :point A x y")

    (":assume":_) ->
        let str = drop 8 input
        in case parseFormulaPrefix str of
             Right f -> return (stateWithHist { theory = f : theory state }, "Assumed: " ++ show f)
             Left err -> return (stateWithHist, formatError err)

    (":load":filename:_) -> do
        content <- readFile filename
        let linesOfFile = lines content
        (finalState, _) <- foldM (\(st, _) line -> do
                                     (newSt, msg) <- processLine st line
                                     if null msg then return () else putStrLn ("[" ++ filename ++ "] " ++ msg)
                                     return (newSt, "")
                                 ) (stateWithHist, "") linesOfFile
        return (finalState, "File loaded: " ++ filename)

    _ ->
        case parseFormulaPrefix input of
          Left err -> return (stateWithHist, formatError err)
          Right formula -> do
            let fullContext = theory state ++ lemmas state
            -- USE ROUTER BY DEFAULT for intelligent solver selection
            let result = autoSolve fullContext formula
            let proved = isProved result
            let reason = proofReason result
            -- Display router's result
            if proved
               then do
                 let msg = "RESULT: PROVED\n" ++
                          "Solver: " ++ show (selectedSolver result) ++ "\n" ++
                          "Reason: " ++ reason ++
                          (if verbose state
                           then "\n\n" ++ formatAutoSolveResult result True
                           else "")
                 return (stateWithHist, msg)
               else do
                 -- Try to find counter-example when proof fails
                 let counterExampleMsg = case findCounterExample fullContext formula of
                       Just ce -> "\n\n" ++ formatCounterExample ce
                       Nothing -> "\n\nNo counter-example found with simple sampling.\nUse :find-counterexample to try more values."
                 return (stateWithHist, "RESULT: NOT PROVED\n" ++
                                       "Solver: " ++ show (selectedSolver result) ++ "\n" ++
                                       "Reason: " ++ reason ++ counterExampleMsg)

-- =============================================
-- 5. REPL Loop
-- =============================================

repl :: REPLState -> IO ()
repl state = do
  putStr "Euclid> "
  hFlush stdout
  input <- getLine
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