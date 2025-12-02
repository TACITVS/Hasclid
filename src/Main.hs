{-# LANGUAGE DeriveGeneric #-}

module Main where

import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyPoly, prettyPolyNice, simplifyExpr, Theory, polyZero, toUnivariate, polyFromConst)
import Parser (parseFormulaPrefix, parseFormulaWithRest)
import Prover (proveTheory, buildSubMap, toPolySub, evaluatePoly, ProofTrace, formatProofTrace)
import CAD (discriminant, toRecursive)
import Sturm (isolateRoots, samplePoints, evalPoly)
import Error (ProverError(..), formatError)

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
  putStrLn "   Euclid Geometric Prover v7.3"
  putStrLn "   Now with Proof Explanations!"
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
  }

initialState :: REPLState
initialState = REPLState [] [] [] False True  -- verbose off, autoSimplify on by default

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

    (":reset":_) -> return (stateWithHist { theory = [] }, "Active Theory reset (Lemmas preserved).")
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
    
    (":help":_)  -> return (stateWithHist, unlines [
        "--- Euclid Geometric Prover Commands ---",
        "",
        "Geometry Basics:",
        "  :point A x y z          Define 3D point (or x y for 2D)",
        "  :assume (= xA 0)        Add assumption",
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
        "  (= expr1 expr2)         Prove equality",
        "  :lemma (= a b)          Prove and store theorem",
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
        "  :solve (> p 0) x        Solve inequality (1D)",
        "  :solve (> p 0) x y      Solve inequality (2D Lifting)",
        "",
        "Utilities:",
        "  :load file.euclid       Run script",
        "  :list, :history, :clean, :reset, :verbose, :auto-simplify, :q"
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
               let (isProved, reason, trace) = proveTheory fullContext formula
               if isProved
                 then do
                   let msg = "LEMMA ESTABLISHED: " ++ reason ++ "\n(Saved)" ++
                             (if verbose state then "\n\n" ++ formatProofTrace trace else "")
                   return (stateWithHist { lemmas = formula : lemmas state }, msg)
                 else return (stateWithHist, "LEMMA FAILED: " ++ reason)

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
            let (isProved, reason, trace) = proveTheory fullContext formula
            let subM = buildSubMap fullContext
            let (l, r) = case formula of
                           Eq a b -> (a,b)
                           Ge a b -> (a,b)
                           Gt a b -> (a,b)

            -- Apply simplification if enabled
            let (l', r', diff) = if autoSimplify state
                                 then (simplifyExpr l, simplifyExpr r, simplifyExpr (Sub l r))
                                 else (l, r, Sub l r)

            -- Use appropriate pretty printer
            let prettyFunc = if autoSimplify state then prettyPolyNice else prettyPoly
            let pL = prettyFunc (toPolySub subM l')
            let pR = prettyFunc (toPolySub subM r')
            let pDiff = prettyFunc (toPolySub subM diff)

            if isProved
               then do
                 let basicMsg = "RESULT: PROVED (" ++ reason ++ ")\nDiff Normal Form: " ++ pDiff
                 let fullMsg = if verbose state
                               then basicMsg ++ "\n\n" ++ formatProofTrace trace
                               else basicMsg
                 return (stateWithHist, fullMsg)
               else return (stateWithHist, "RESULT: FALSE (" ++ reason ++ ")\nLHS: " ++ pL ++ "\nRHS: " ++ pR)

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