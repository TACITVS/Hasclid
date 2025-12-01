{-# LANGUAGE DeriveGeneric #-}

module Main where

import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyPoly, Theory)
import Parser (parseFormulaPrefix)
import Prover (proveTheory, buildSubMap, toPolySub)
import CAD (discriminant)
import System.IO (hFlush, stdout)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit)
import qualified Control.Exception as CE

-- =============================================
-- 1. ENTRY POINT
-- =============================================

main :: IO ()
main = do
  -- Clear screen on startup
  putStr "\ESC[2J\ESC[H"
  putStrLn "=========================================================="
  putStrLn "   Euclid Geometric Prover v5.5 (Release)"
  putStrLn "   A Symbolic Algebra System for Euclidean Geometry"
  putStrLn ""
  putStrLn "   Features:"
  putStrLn "     * 2D/3D Geometry & Theorem Proving"
  putStrLn "     * Gröbner Basis Automated Deduction"
  putStrLn "     * Cylindrical Algebraic Decomposition (CAD)"
  putStrLn "     * Inequality Verification (Sturm's Theorem)"
  putStrLn ""
  putStrLn "   Quick Start:"
  putStrLn "     :help        -> Show all commands"
  putStrLn "     :load file   -> Run a script"
  putStrLn "     :q           -> Quit"
  putStrLn "=========================================================="
  repl initialState

-- =============================================
-- 2. REPL State
-- =============================================

data REPLState = REPLState 
  { theory :: Theory 
  , history :: [String]
  }

initialState :: REPLState
initialState = REPLState [] []

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r

-- =============================================
-- 3. Helpers
-- =============================================

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
  
  -- Record history (unless it's a history command itself)
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

    (":clear":_) -> return (initialState { history = newHist }, "Theory cleared (State reset).")
    (":reset":_) -> return (initialState { history = newHist }, "Theory cleared (State reset).")
    
    (":list":_)  -> return (stateWithHist, prettyTheory (theory state))
    (":help":_)  -> return (stateWithHist, unlines [
        "Commands:",
        "  :point A x y z     -> Define 3D point",
        "  :point A x y       -> Define 2D point",
        "  :assume (= xA 0)   -> Manually assume",
        "  :project expr var  -> CAD Projection (Find shadow)",
        "  :load file.euclid  -> Load script",
        "  :list              -> Show assumptions",
        "  :clean / :cls      -> Clear screen",
        "  :reset             -> Reset theory",
        "  :history           -> Show past commands",
        "  :q / :quit         -> Exit"
        ])

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
                       let poly = toPolySub (buildSubMap (theory state)) e
                       let shadow = discriminant poly varStr
                       return (stateWithHist, "Projection (Discriminant w.r.t " ++ varStr ++ "):\n" ++ prettyPoly shadow)
                   Left err -> return (stateWithHist, "Projection Failed: " ++ err)
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
             Left err -> return (stateWithHist, "Parse Error: " ++ err)

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
          Left err -> return (stateWithHist, "Parse Error: " ++ err)
          Right formula -> do
            let (isProved, reason) = proveTheory (theory state) formula
            let subM = buildSubMap (theory state)
            let (l, r) = case formula of
                           Eq a b -> (a,b)
                           Ge a b -> (a,b)
                           Gt a b -> (a,b)
            let pL = prettyPoly (toPolySub subM l)
            let pR = prettyPoly (toPolySub subM r)
            
            if isProved 
               then return (stateWithHist, "RESULT: PROVED (" ++ reason ++ ")\nDiff Normal Form: " ++ prettyPoly (toPolySub subM (Sub l r)))
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