{-# LANGUAGE DeriveGeneric #-}

module Parser 
  ( parseFormulaPrefix
  , parseFormulaWithRest
  , parseFormulaWithMacros
  , parseFormulaWithRestAndMacros
  , SExpr(..)
  , parseSExpr
  , tokenizePrefix
  , expandMacros
  , MacroMap
  ) where

import Expr
import Error
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Ratio ((%))
import Data.List (foldl')
import qualified Data.Map.Strict as M

-- =============================================
-- S-Expressions
-- =============================================

data SExpr = Atom String | List [SExpr] deriving (Show, Eq)

-- | Map of macro names to (parameter list, body)
type MacroMap = M.Map String ([String], SExpr)

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

-- =============================================
-- Macro Expansion
-- =============================================

-- | Recursively expand macros in an S-Expression with depth limit
expandMacros :: MacroMap -> SExpr -> SExpr
expandMacros macros sexpr = expandMacros' 1000 macros sexpr

expandMacros' :: Int -> MacroMap -> SExpr -> SExpr
expandMacros' 0 _ _ = error "Macro expansion depth limit exceeded (possible infinite recursion)"
expandMacros' depth macros expr@(List (Atom name : args)) =
  case name of
    -- Special Form: IF
    -- Usage: (if (= a b) trueBranch falseBranch)
    "if" ->
        case args of
            [cond, trueBranch, falseBranch] ->
                -- Evaluate condition
                case evalCondition (expandMacros' (depth-1) macros cond) of
                    Just True -> expandMacros' (depth-1) macros trueBranch
                    Just False -> expandMacros' (depth-1) macros falseBranch
                    Nothing -> List (Atom "if" : map (expandMacros' (depth-1) macros) args) -- Keep as is if undecidable
            _ -> expr -- Wrong arity

    -- Special Form: Arithmetic (compile-time)
    -- Only reduces if arguments are integer literals
    op | op `elem` ["+", "-", "*"] ->
        let expandedArgs = map (expandMacros' (depth-1) macros) args
        in case mapM toInt expandedArgs of
             Just nums -> Atom (show (foldl1 (opFunc op) nums))
             Nothing -> List (Atom op : expandedArgs)

    _ ->
      case M.lookup name macros of
        Just (params, body) ->
          if length params /= length args
          then List (Atom name : map (expandMacros' (depth-1) macros) args)
          else
            let 
                -- Expand arguments first (eager evaluation of args)
                expandedArgs = map (expandMacros' (depth-1) macros) args
                -- Create substitution map: param -> expanded arg
                -- Only substitute if the atom matches a parameter name
                subst = M.fromList $ zip params expandedArgs
                
                -- Substitute into body, but be careful not to substitute global vars if they clash with params (though unique naming helps)
                -- Actually, the issue might be that `e1`, `e2` etc in the body are being treated as parameters? No, they are atoms in the body.
                -- They should be expanded in the next recursive step if they are macros.
                substitutedBody = substituteSExpr subst body
            in 
                -- Recursively expand the result
                expandMacros' (depth-1) macros substitutedBody
        Nothing -> List (Atom name : map (expandMacros' (depth-1) macros) args)

expandMacros' depth macros (List list) = List (map (expandMacros' depth macros) list)
expandMacros' _ _ atom = atom

-- | Evaluate condition for macros
evalCondition :: SExpr -> Maybe Bool
evalCondition (List [Atom "=", a, b]) = Just (a == b) -- Structural equality
evalCondition (List [Atom "eq", a, b]) = Just (a == b)
evalCondition (List [Atom "ne", a, b]) = Just (a /= b)
evalCondition _ = Nothing

-- | Helper to parse integer from SExpr
toInt :: SExpr -> Maybe Integer
toInt (Atom s) | all (\c -> isDigit c || c == '-') s = Just (read s)
toInt _ = Nothing

-- | Arithmetic operators
opFunc :: String -> (Integer -> Integer -> Integer)
opFunc "+" = (+)
opFunc "-" = (-)
opFunc "*" = (*)
opFunc _ = const

-- | Substitute variables in an SExpr with other SExprs
substituteSExpr :: M.Map String SExpr -> SExpr -> SExpr
substituteSExpr subst (Atom s) = M.findWithDefault (Atom s) s subst
substituteSExpr subst (List xs) = List (map (substituteSExpr subst) xs)

-- =============================================
-- Expression Parsing
-- =============================================

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

  "x" -> case args of
    [Atom p] -> Right $ Var ("x" ++ p)
    _ -> Left $ ParseError (WrongArity "x" 1 (length args)) "Usage: (x Point)"

  "y" -> case args of
    [Atom p] -> Right $ Var ("y" ++ p)
    _ -> Left $ ParseError (WrongArity "y" 1 (length args)) "Usage: (y Point)"

  "z" -> case args of
    [Atom p] -> Right $ Var ("z" ++ p)
    _ -> Left $ ParseError (WrongArity "z" 1 (length args)) "Usage: (z Point)"

  "=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >) found inside expression. Use at top level only."
  ">=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                   "Formula logic (=, >=, >) found inside expression. Use at top level only."
  ">" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >) found inside expression. Use at top level only."

  -- If unknown operator, it might be a macro that wasn't expanded (e.g. wrong arity)
  _ -> Left $ ParseError (UnknownOperator op) ("Unknown operator or macro: " ++ op)

exprFromSExpr _ = Left $ ParseError (InvalidSyntax "invalid S-Expression")
                         "Invalid S-Expression format"

-- Parse strictly with macros
parseFormulaWithMacros :: MacroMap -> String -> Either ProverError Formula
parseFormulaWithMacros macros input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens
  
  let expanded = expandMacros macros sexpr

  case expanded of
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

-- Legacy wrapper for compatibility
parseFormulaPrefix :: String -> Either ProverError Formula
parseFormulaPrefix = parseFormulaWithMacros M.empty

-- NEW: Parse formula and return remaining tokens (for :solve) with macros
parseFormulaWithRestAndMacros :: MacroMap -> String -> Either ProverError (Formula, [String])
parseFormulaWithRestAndMacros macros input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens
  
  let expanded = expandMacros macros sexpr

  case expanded of
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

-- Legacy wrapper
parseFormulaWithRest :: String -> Either ProverError (Formula, [String])
parseFormulaWithRest = parseFormulaWithRestAndMacros M.empty
