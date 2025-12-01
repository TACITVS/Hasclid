{-# LANGUAGE DeriveGeneric #-}

module Main where

-- FIXED IMPORT: Changed Expr(...) to Expr(..) to import Sub, Add, etc.
import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyPoly, Theory)
import Parser (parseFormulaPrefix)
import Prover (proveTheory, buildSubMap, toPolySub)
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
  putStrLn "=========================================="
  putStrLn "   Euclid Geometric Prover v4.1 (Final)"
  putStrLn "   Type :help for commands."
  putStrLn "=========================================="
  repl initialState

-- =============================================
-- 2. REPL State
-- =============================================

data REPLState = REPLState { theory :: Theory }

initialState :: REPLState
initialState = REPLState []

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
  
  if null (filter (/= ' ') input) then return (state, "")
  else case words input of
  
    (":clear":_) -> return (initialState, "Theory cleared.")
    (":list":_)  -> return (state, prettyTheory (theory state))
    (":help":_)  -> return (state, unlines [
        "Commands:",
        "  :point A x y z     -> Define 3D point",
        "  :point A x y       -> Define 2D point",
        "  :assume (= xA 0)   -> Manually assume",
        "  :load file.euclid  -> Load script",
        "  :list, :clear      -> Utilities"
        ])

    (":point":name:xStr:yStr:zStr:_) -> do
         let exprX = parseCoord xStr
         let exprY = parseCoord yStr
         let exprZ = parseCoord zStr
         let asmX = Eq (Var ("x" ++ name)) exprX
         let asmY = Eq (Var ("y" ++ name)) exprY
         let asmZ = Eq (Var ("z" ++ name)) exprZ
         return (state { theory = asmX : asmY : asmZ : theory state }, 
                 "Defined 3D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ", " ++ zStr ++ ")")
    
    (":point":name:xStr:yStr:_) -> do
         let exprX = parseCoord xStr
         let exprY = parseCoord yStr
         let exprZ = Const 0
         let asmX = Eq (Var ("x" ++ name)) exprX
         let asmY = Eq (Var ("y" ++ name)) exprY
         let asmZ = Eq (Var ("z" ++ name)) exprZ
         return (state { theory = asmX : asmY : asmZ : theory state }, 
                 "Defined 2D Point " ++ name ++ " at (" ++ xStr ++ ", " ++ yStr ++ ")")
    
    (":point":_) -> return (state, "Usage: :point A x y z  OR  :point A x y")

    (":assume":_) -> 
        let str = drop 8 input
        in case parseFormulaPrefix str of
             Right f -> return (state { theory = f : theory state }, "Assumed: " ++ show f)
             Left err -> return (state, "Parse Error: " ++ err)

    (":load":filename:_) -> do
        content <- readFile filename
        let linesOfFile = lines content
        (finalState, _) <- foldM (\(st, _) line -> do
                                     (newSt, msg) <- processLine st line
                                     if null msg then return () else putStrLn ("[" ++ filename ++ "] " ++ msg)
                                     return (newSt, "")
                                 ) (state, "") linesOfFile
        return (finalState, "File loaded: " ++ filename)

    _ -> 
        case parseFormulaPrefix input of
          Left err -> return (state, "Parse Error: " ++ err)
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
               then return (state, "RESULT: PROVED (" ++ reason ++ ")\nDiff Normal Form: " ++ prettyPoly (toPolySub subM (Sub l r)))
               else return (state, "RESULT: FALSE (" ++ reason ++ ")\nLHS: " ++ pL ++ "\nRHS: " ++ pR)

-- =============================================
-- 5. REPL Loop
-- =============================================

repl :: REPLState -> IO ()
repl state = do
  putStr "Euclid> "
  hFlush stdout
  input <- getLine
  if input `elem` ["exit", "quit"] then putStrLn "Goodbye."
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