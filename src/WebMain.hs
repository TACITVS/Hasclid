{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Web-friendly entry point for browser deployment
-- This provides a reactor-mode API while keeping the local REPL in Main.hs intact

module WebMain where

import Expr (Formula(Eq, Ge, Gt), Expr(..), prettyExpr, prettyPoly, Theory)
import Parser (parseFormulaPrefix)
import Prover (proveTheory, buildSubMap, toPolySub)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Char (isDigit)
import qualified Data.Map.Strict as M

-- =============================================
-- Global State (for web session)
-- =============================================

{-# NOINLINE globalTheory #-}
globalTheory :: IORef Theory
globalTheory = unsafePerformIO (newIORef [])

{-# NOINLINE globalLemmas #-}
globalLemmas :: IORef Theory
globalLemmas = unsafePerformIO (newIORef [])

{-# NOINLINE globalHistory #-}
globalHistory :: IORef [String]
globalHistory = unsafePerformIO (newIORef [])

-- =============================================
-- FFI Exports for JavaScript
-- =============================================

-- | Initialize the prover (call once on startup)
foreign export ccall "prover_init" proverInit :: IO CString

proverInit :: IO CString
proverInit = do
  writeIORef globalTheory []
  writeIORef globalLemmas []
  writeIORef globalHistory []
  newCString "Euclid Geometric Prover v7.2 (Web)\nType :help for commands.\n"

-- | Execute a single command and return the result
foreign export ccall "prover_execute" proverExecute :: CString -> IO CString

proverExecute :: CString -> IO CString
proverExecute cstr = do
  input <- peekCString cstr

  theory <- readIORef globalTheory
  lemmas <- readIORef globalLemmas
  history <- readIORef globalHistory

  -- Add to history
  unless (null input || ":history" `isPrefixOf` input) $
    writeIORef globalHistory (input : history)

  result <- processCommand theory lemmas input

  case result of
    (newTheory, newLemmas, output) -> do
      writeIORef globalTheory newTheory
      writeIORef globalLemmas newLemmas
      newCString output
  where
    unless True _ = return ()
    unless False action = action

-- =============================================
-- Command Processing (adapted from Main.hs)
-- =============================================

processCommand :: Theory -> Theory -> String -> IO (Theory, Theory, String)
processCommand theory lemmas rawInput = do
  let input = stripComment rawInput

  if null (filter (/= ' ') input)
    then return (theory, lemmas, "")
    else case words input of

      (":clean":_) -> return (theory, lemmas, "\ESC[2J\ESC[H")
      (":cls":_)   -> return (theory, lemmas, "\ESC[2J\ESC[H")

      (":history":_) -> do
        hist <- readIORef globalHistory
        let h = reverse hist
        let formatted = unlines $ zipWith (\i s -> show i ++ ". " ++ s) [1..] h
        return (theory, lemmas, "Session History:\n" ++ formatted)

      (":reset":_) -> return ([], lemmas, "Active Theory reset (Lemmas preserved).")
      (":clear":_) -> return ([], [], "Full System Reset.")

      (":list":_) -> do
        let tStr = if null theory then "  (None)" else prettyTheory theory
        let lStr = if null lemmas then "  (None)" else prettyTheory lemmas
        return (theory, lemmas, "Active Assumptions:\n" ++ tStr ++ "\nProven Lemmas:\n" ++ lStr)

      (":help":_) -> return (theory, lemmas, unlines [
          "--- Euclid Commands ---",
          "Geometry:",
          "  :point A x y z     Define 3D point",
          "  :assume (= xA 0)   Add assumption",
          "",
          "Logic & CAD:",
          "  :lemma (= a b)     Prove and store theorem",
          "",
          "Utilities:",
          "  :list, :history, :clean, :reset, :q"
          ])

      (":lemma":_) ->
        let str = drop 7 input
        in case parseFormulaPrefix str of
             Left err -> return (theory, lemmas, "Parse Error: " ++ err)
             Right formula -> do
               let fullContext = theory ++ lemmas
               let (isProved, reason) = proveTheory fullContext formula
               if isProved
                 then return (theory, formula : lemmas,
                              "LEMMA ESTABLISHED: " ++ reason ++ "\n(Saved)")
                 else return (theory, lemmas, "LEMMA FAILED: " ++ reason)

      (":point":name:xStr:yStr:rest) -> do
        let (exprX, exprY, exprZ, dims) =
              case rest of
                (zStr:_) -> (parseCoord xStr, parseCoord yStr, parseCoord zStr, "3D")
                []       -> (parseCoord xStr, parseCoord yStr, Const 0, "2D")
        let asmX = Eq (Var ("x" ++ name)) exprX
        let asmY = Eq (Var ("y" ++ name)) exprY
        let asmZ = Eq (Var ("z" ++ name)) exprZ
        return (asmX : asmY : asmZ : theory, lemmas,
                "Defined " ++ dims ++ " Point " ++ name)

      (":point":_) -> return (theory, lemmas, "Usage: :point A x y [z]")

      (":assume":_) ->
        let str = drop 8 input
        in case parseFormulaPrefix str of
             Right f -> return (f : theory, lemmas, "Assumed: " ++ show f)
             Left err -> return (theory, lemmas, "Parse Error: " ++ err)

      ("exit":_) -> return (theory, lemmas, "Goodbye.")
      ("quit":_) -> return (theory, lemmas, "Goodbye.")
      (":q":_)   -> return (theory, lemmas, "Goodbye.")

      _ ->
        case parseFormulaPrefix input of
          Left err -> return (theory, lemmas, "Parse Error: " ++ err)
          Right formula -> do
            let fullContext = theory ++ lemmas
            let (isProved, reason) = proveTheory fullContext formula
            let subM = buildSubMap fullContext
            let (l, r) = case formula of
                           Eq a b -> (a,b)
                           Ge a b -> (a,b)
                           Gt a b -> (a,b)

            if isProved
               then return (theory, lemmas,
                     "RESULT: PROVED (" ++ reason ++ ")\n" ++
                     "Diff Normal Form: " ++ prettyPoly (toPolySub subM (Sub l r)))
               else return (theory, lemmas,
                     "RESULT: FALSE (" ++ reason ++ ")\n" ++
                     "LHS: " ++ prettyPoly (toPolySub subM l) ++ "\n" ++
                     "RHS: " ++ prettyPoly (toPolySub subM r))

-- =============================================
-- Helper Functions
-- =============================================

prettyTheory :: Theory -> String
prettyTheory th = unlines [ show i ++ ": " ++ showFormula f | (i, f) <- zip [1..] th ]
  where
    showFormula (Eq l r) = prettyExpr l ++ " = " ++ prettyExpr r
    showFormula (Ge l r) = prettyExpr l ++ " >= " ++ prettyExpr r
    showFormula (Gt l r) = prettyExpr l ++ " > " ++ prettyExpr r

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

-- =============================================
-- Entry Point (for WASM)
-- =============================================

-- This is a no-op main that satisfies the WASM linker
-- The actual interaction happens through FFI exports
main :: IO ()
main = return ()
