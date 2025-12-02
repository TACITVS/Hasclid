{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Expr
import Error
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Ratio ((%))
import Data.List (foldl')

-- =============================================
-- S-Expressions
-- =============================================

data SExpr = Atom String | List [SExpr] deriving (Show)

tokenizePrefix :: String -> [String]
tokenizePrefix s = words $ concatMap pad s
  where
    pad '(' = " ( "
    pad ')' = " ) "
    pad c   = [c]

parseSExpr :: [String] -> Either ProverError (SExpr, [String])
parseSExpr [] = Left $ ParseError EmptyExpression "Expected expression but got end of input"
parseSExpr (t:ts)
  | t == "(" = parseList ts
  | t == ")" = Left $ ParseError (UnexpectedToken ")") "Unexpected ) at start of expression"
  | otherwise = Right (Atom t, ts)

parseList :: [String] -> Either ProverError (SExpr, [String])
parseList [] = Left $ ParseError MissingClosingParen "Missing closing ) at end of input"
parseList (t:ts)
  | t == ")" = Right (List [], ts)
  | otherwise = do
      (expr, rest) <- parseSExpr (t:ts)
      result <- parseList rest
      case result of
        (List exprs, finalRest) -> return (List (expr:exprs), finalRest)
        _ -> Left $ ParseError (InvalidSyntax "unexpected result") "Internal parser error"

exprFromSExpr :: SExpr -> Either ProverError Expr
exprFromSExpr (Atom t)
  | all (\c -> isDigit c || c == '-' || c == '/') t =
      if '/' `elem` t
      then case span (/= '/') t of
             (n, d) | null d || length d < 2 ->
               Left $ ParseError (InvalidNumber t) "Malformed rational number"
             (n, d) -> Right $ Const (read n % read (drop 1 d))
      else Right $ Const (fromInteger (read t) % 1)
  | otherwise = Right $ Var t

exprFromSExpr (List (Atom op : args)) = case op of
  "+" -> do
    exprs <- mapM exprFromSExpr args
    case exprs of
      [] -> Left $ ParseError (WrongArity "+" 1 0) "Addition requires at least 1 argument"
      _ -> Right $ foldl1 Add exprs

  "-" -> do
    exprs <- mapM exprFromSExpr args
    case exprs of
      [a] -> Right $ Sub (Const 0) a
      [a,b] -> Right $ Sub a b
      _ -> Left $ ParseError (WrongArity "-" 2 (length args))
                    "Subtraction requires 1 or 2 arguments"

  "*" -> do
    exprs <- mapM exprFromSExpr args
    case exprs of
      [] -> Left $ ParseError (WrongArity "*" 1 0) "Multiplication requires at least 1 argument"
      _ -> Right $ foldl1 Mul exprs

  "/" -> do
    exprs <- mapM exprFromSExpr args
    case exprs of
      [a,b] -> Right $ Div a b
      _ -> Left $ ParseError (WrongArity "/" 2 (length args))
                    "Division requires exactly 2 arguments"

  "^" -> case args of
    [a, Atom n] | all isDigit n -> do
      base <- exprFromSExpr a
      Right $ Pow base (read n)
    [a, Atom n] -> Left $ ParseError (InvalidSyntax "exponent must be natural number")
                          ("Expected natural number exponent, got: " ++ n)
    _ -> Left $ ParseError (WrongArity "^" 2 (length args))
                  "Power requires base and natural number exponent"

  "dist2" -> case args of
    [Atom p1, Atom p2] -> Right $ Dist2 p1 p2
    _ -> Left $ ParseError (WrongArity "dist2" 2 (length args))
                  "Usage: (dist2 A B)"

  "collinear" -> case args of
    [Atom p1, Atom p2, Atom p3] -> Right $ Collinear p1 p2 p3
    _ -> Left $ ParseError (WrongArity "collinear" 3 (length args))
                  "Usage: (collinear A B C)"

  "dot" -> case args of
    [Atom a, Atom b, Atom c, Atom d] -> Right $ Dot a b c d
    _ -> Left $ ParseError (WrongArity "dot" 4 (length args))
                  "Usage: (dot A B C D)"

  "circle" -> case args of
    [Atom p, Atom c, r] -> do
      radius <- exprFromSExpr r
      Right $ Circle p c radius
    _ -> Left $ ParseError (WrongArity "circle" 3 (length args))
                  "Usage: (circle P Center Radius)"

  "midpoint" -> case args of
    [Atom a, Atom b, Atom m] -> Right $ Midpoint a b m
    _ -> Left $ ParseError (WrongArity "midpoint" 3 (length args))
                  "Usage: (midpoint A B M) - M is midpoint of AB"

  "perpendicular" -> case args of
    [Atom a, Atom b, Atom c, Atom d] -> Right $ Perpendicular a b c d
    _ -> Left $ ParseError (WrongArity "perpendicular" 4 (length args))
                  "Usage: (perpendicular A B C D) - AB ⊥ CD"

  "parallel" -> case args of
    [Atom a, Atom b, Atom c, Atom d] -> Right $ Parallel a b c d
    _ -> Left $ ParseError (WrongArity "parallel" 4 (length args))
                  "Usage: (parallel A B C D) - AB ∥ CD"

  "=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >) found inside expression. Use at top level only."
  ">=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                   "Formula logic (=, >=, >) found inside expression. Use at top level only."
  ">" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >) found inside expression. Use at top level only."

  _ -> Left $ ParseError (UnknownOperator op) ("Unknown operator: " ++ op)

exprFromSExpr _ = Left $ ParseError (InvalidSyntax "invalid S-Expression")
                         "Invalid S-Expression format"

-- Parse strictly (End of input must be empty)
parseFormulaPrefix :: String -> Either ProverError Formula
parseFormulaPrefix input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens

  case sexpr of
    List [Atom "=", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      if null rest
        then Right $ Eq l r
        else Left $ ParseError (ExtraTokens rest)
                       ("Extra tokens after formula: " ++ show rest)

    List [Atom ">=", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      if null rest
        then Right $ Ge l r
        else Left $ ParseError (ExtraTokens rest)
                       ("Extra tokens after formula: " ++ show rest)

    List [Atom ">", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      if null rest
        then Right $ Gt l r
        else Left $ ParseError (ExtraTokens rest)
                       ("Extra tokens after formula: " ++ show rest)

    List [Atom op, _, _] | op `elem` ["=", ">=", ">"] ->
      Left $ ParseError (ExtraTokens rest)
                ("Extra tokens after formula: " ++ show rest)

    _ -> Left $ ParseError (InvalidSyntax "not a formula")
                  "Expected format: (= lhs rhs) OR (>= lhs rhs) OR (> lhs rhs)"

-- NEW: Parse formula and return remaining tokens (for :solve)
parseFormulaWithRest :: String -> Either ProverError (Formula, [String])
parseFormulaWithRest input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens

  case sexpr of
    List [Atom "=", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      Right (Eq l r, rest)

    List [Atom ">=", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      Right (Ge l r, rest)

    List [Atom ">", lhs, rhs] -> do
      l <- exprFromSExpr lhs
      r <- exprFromSExpr rhs
      Right (Gt l r, rest)

    _ -> Left $ ParseError (InvalidSyntax "not a formula")
                  "Expected format: (= lhs rhs) or (>= lhs rhs) or (> lhs rhs)"
