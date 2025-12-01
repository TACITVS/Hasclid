{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Expr
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Ratio ((%))
import Data.List (foldl')

data SExpr = Atom String | List [SExpr] deriving (Show)

tokenizePrefix :: String -> [String]
tokenizePrefix s = words $ concatMap pad s
  where
    pad '(' = " ( "
    pad ')' = " ) "
    pad c   = [c]

parseSExpr :: [String] -> (SExpr, [String])
parseSExpr [] = error "Unexpected end of input"
parseSExpr (t:ts)
  | t == "(" = parseList ts
  | t == ")" = error "Unexpected ) at start of expression"
  | otherwise = (Atom t, ts)

parseList :: [String] -> (SExpr, [String])
parseList [] = error "Missing closing )"
parseList (t:ts)
  | t == ")" = (List [], ts)
  | otherwise = 
      let (expr, rest) = parseSExpr (t:ts)
          (List exprs, finalRest) = parseList rest
      in (List (expr:exprs), finalRest)

exprFromSExpr :: SExpr -> Expr
exprFromSExpr (Atom t)
  | all (\c -> isDigit c || c == '-' || c == '/') t = 
      if '/' `elem` t 
      then let (n,d) = span (/= '/') t 
               denomStr = drop 1 d
           in Const (read n % read denomStr)
      else Const (fromInteger (read t) % 1)
  | otherwise = Var t 

exprFromSExpr (List (Atom op : args)) = case op of
  "+" -> foldl1 Add (map exprFromSExpr args)
  "-" -> case map exprFromSExpr args of
           [a] -> Sub (Const 0) a
           [a,b] -> Sub a b
           _ -> error "(-) requires 1 or 2 args"
  "*" -> foldl1 Mul (map exprFromSExpr args)
  "/" -> case map exprFromSExpr args of [a,b] -> Div a b; _ -> error "(/) requires 2 args"
  "^" -> case args of [a, Atom n] -> Pow (exprFromSExpr a) (read n); _ -> error "(^) requires base and integer exp"
  
  "dist2"     -> case args of [Atom p1, Atom p2] -> Dist2 p1 p2; _ -> error "Usage: (dist2 A B)"
  "collinear" -> case args of [Atom p1, Atom p2, Atom p3] -> Collinear p1 p2 p3; _ -> error "Usage: (collinear A B C)"
  "dot"       -> case args of [Atom a, Atom b, Atom c, Atom d] -> Dot a b c d; _ -> error "Usage: (dot A B C D)"
  "circle"    -> case args of [Atom p, Atom c, r] -> Circle p c (exprFromSExpr r); _ -> error "Usage: (circle P Center Radius)"
  
  -- Prevent formulas in expressions
  "=" -> errFormula
  ">=" -> errFormula
  ">" -> errFormula
  _ -> error $ "Unknown operator: " ++ op
  where errFormula = error "Formula logic (=, >=, >) found inside expression. Use at top level only."

parseFormulaPrefix :: String -> Either String Formula
parseFormulaPrefix input = 
  let tokens = tokenizePrefix input
  in case parseSExpr tokens of
       (List [Atom "=", lhs, rhs], []) -> Right $ Eq (exprFromSExpr lhs) (exprFromSExpr rhs)
       -- NEW: Inequality Parsing
       (List [Atom ">=", lhs, rhs], []) -> Right $ Ge (exprFromSExpr lhs) (exprFromSExpr rhs)
       (List [Atom ">", lhs, rhs], [])  -> Right $ Gt (exprFromSExpr lhs) (exprFromSExpr rhs)
       
       (List [Atom op, _, _], rest) | op `elem` ["=", ">=", ">"] -> Left $ "Extra tokens after formula: " ++ show rest
       _ -> Left "Expected format: (= lhs rhs) OR (>= lhs rhs)"