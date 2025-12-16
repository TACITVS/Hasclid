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
import Data.Char (isDigit)
import Data.Ratio ((%))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
expandMacros :: MacroMap -> SExpr -> Either ProverError SExpr
expandMacros macros sexpr = expandMacros' 1000 macros sexpr

expandMacros' :: Int -> MacroMap -> SExpr -> Either ProverError SExpr
expandMacros' 0 _ _ = Left $ ParseError (MacroExpansionDepthExceeded 1000)
                        "Macro expansion exceeded 1000 levels (check for recursive macros)"
expandMacros' depth macros expr@(List (Atom name : args)) =
  case name of
    -- Special Form: IF
    -- Usage: (if (= a b) trueBranch falseBranch)
    "if" ->
        case args of
            [cond, trueBranch, falseBranch] -> do
                -- Evaluate condition
                expandedCond <- expandMacros' (depth-1) macros cond
                case evalCondition expandedCond of
                    Just True -> expandMacros' (depth-1) macros trueBranch
                    Just False -> expandMacros' (depth-1) macros falseBranch
                    Nothing -> do
                        expandedArgs <- traverse (expandMacros' (depth-1) macros) args
                        return $ List (Atom "if" : expandedArgs)
            _ -> return expr -- Wrong arity

    -- Special Form: Arithmetic (compile-time)
    -- Only reduces if arguments are integer literals
    op | op `elem` ["+", "-", "*"] -> do
        expandedArgs <- traverse (expandMacros' (depth-1) macros) args
        case traverse toInt expandedArgs of
             Just nums -> return $ Atom (show (foldl1 (opFunc op) nums))
             Nothing -> return $ List (Atom op : expandedArgs)

    -- Special Form: Indexer (compile-time string concatenation)
    -- Usage: (idx P 1) -> P1
    "idx" ->
        case args of
            [Atom prefix, indexExpr] -> do
                expandedIndex <- expandMacros' (depth-1) macros indexExpr
                case toInt expandedIndex of
                     Just n -> return $ Atom (prefix ++ show n)
                     Nothing -> return $ List (Atom "idx" : [Atom prefix, expandedIndex])
            _ -> return expr

    _ ->
      case M.lookup name macros of
        Just (params, body) ->
          if length params /= length args
          then do
              expandedArgs <- traverse (expandMacros' (depth-1) macros) args
              return $ List (Atom name : expandedArgs)
          else do
                -- Expand arguments first (eager evaluation of args)
                expandedArgs <- traverse (expandMacros' (depth-1) macros) args
                -- Create substitution map: param -> expanded arg
                let subst = M.fromList $ zip params expandedArgs
                let substitutedBody = substituteSExpr subst body
                -- Recursively expand the result
                expandMacros' (depth-1) macros substitutedBody
        Nothing -> do
            expandedArgs <- traverse (expandMacros' (depth-1) macros) args
            return $ List (Atom name : expandedArgs)

expandMacros' depth macros (List list) = do
    expandedList <- traverse (expandMacros' depth macros) list
    return $ List expandedList
expandMacros' depth macros atom@(Atom name) =
  case M.lookup name macros of
    Just ([], body) -> expandMacros' (depth-1) macros body
    _ -> return atom

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

exprFromSExpr :: S.Set String -> SExpr -> Either ProverError Expr
exprFromSExpr intVars (Atom t)
  | all (\c -> isDigit c || c == '-' || c == '/') t =
      if '/' `elem` t
      then case span (/= '/') t of
             (_, d) | null d || length d < 2 ->
               Left $ ParseError (InvalidNumber t) "Malformed rational number"
             (n, d) -> Right $ Const (read n % read (drop 1 d))
      else Right $ Const (fromInteger (read t) % 1)
  | S.member t intVars = Right $ IntVar t
  | otherwise = Right $ Var t

exprFromSExpr intVars (List (Atom op : args)) = case op of
  "+" -> do
    exprs <- mapM (exprFromSExpr intVars) args
    case exprs of
      [] -> Left $ ParseError (WrongArity "+" 1 0) "Addition requires at least 1 argument"
      _ -> Right $ foldl1 Add exprs

  "-" -> do
    exprs <- mapM (exprFromSExpr intVars) args
    case exprs of
      [a] -> Right $ Sub (Const 0) a
      [a,b] -> Right $ Sub a b
      _ -> Left $ ParseError (WrongArity "-" 2 (length args))
                    "Subtraction requires 1 or 2 arguments"

  "*" -> do
    exprs <- mapM (exprFromSExpr intVars) args
    case exprs of
      [] -> Left $ ParseError (WrongArity "*" 1 0) "Multiplication requires at least 1 argument"
      _ -> Right $ foldl1 Mul exprs

  "/" -> do
    exprs <- mapM (exprFromSExpr intVars) args
    case exprs of
      [a,b] -> Right $ Div a b
      _ -> Left $ ParseError (WrongArity "/" 2 (length args))
                    "Division requires exactly 2 arguments"

  "mod" -> do
    exprs <- mapM (exprFromSExpr intVars) args
    case exprs of
      [a,b] -> Right $ Mod a b
      _ -> Left $ ParseError (WrongArity "mod" 2 (length args))
                    "Mod requires exactly 2 arguments"

  "^" -> case args of
    [a, Atom n] | all isDigit n -> do
      base <- exprFromSExpr intVars a
      Right $ Pow base (read n)
    [_, Atom n] -> Left $ ParseError (InvalidSyntax "exponent must be natural number")
                          ("Expected natural number exponent, got: " ++ n)
    _ -> Left $ ParseError (WrongArity "^" 2 (length args))
                  "Power requires base and natural number exponent"

  "int" -> case args of
    [Atom name] -> Right $ IntVar name
    _ -> Left $ ParseError (WrongArity "int" 1 (length args)) "Usage: (int x)"

  "int-var" -> case args of
    [Atom name] -> Right $ IntVar name
    _ -> Left $ ParseError (WrongArity "int-var" 1 (length args)) "Usage: (int-var x)"

  "int-const" -> case args of
    [Atom n] | all (\c -> isDigit c || c == '-') n -> Right $ IntConst (read n)
    [Atom n] -> Left $ ParseError (InvalidNumber n) "Malformed integer constant"
    _ -> Left $ ParseError (WrongArity "int-const" 1 (length args)) "Usage: (int-const 5)"

  "sqrt" -> case args of
    [a] -> do
      e <- exprFromSExpr intVars a
      Right $ Sqrt e
    _ -> Left $ ParseError (WrongArity "sqrt" 1 (length args))
                  "sqrt requires exactly 1 argument"

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
      radius <- exprFromSExpr intVars r
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

  "angle-eq" -> case args of
    [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ AngleEq2D a b c d e f
    _ -> Left $ ParseError (WrongArity "angle-eq" 6 (length args))
                  "Usage: (angle-eq A B C D E F) - asserts ∠ABC = ∠DEF in 2D"

  -- Angle equality sugar as an expression (useful when nested under =)
  "angle=" -> case args of
    [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ AngleEq2D a b c d e f
    _ -> Left $ ParseError (WrongArity "angle=" 6 (length args))
                  "Usage: (angle= A B C D E F) - asserts ∠ABC = ∠DEF in 2D"

  "angle-eq-abs" -> case args of
    [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ AngleEq2DAbs a b c d e f
    _ -> Left $ ParseError (WrongArity "angle-eq-abs" 6 (length args))
                  "Usage: (angle-eq-abs A B C D E F) - asserts ∠ABC ≡ ∠DEF up to reflection in 2D"

  "angle=abs" -> case args of
    [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ AngleEq2DAbs a b c d e f
    _ -> Left $ ParseError (WrongArity "angle=abs" 6 (length args))
                  "Usage: (angle=abs A B C D E F) - asserts ∠ABC ≡ ∠DEF up to reflection in 2D"

  "det" -> do
      -- Usage: (det e1 e2 e3 e4 ...) - flattens a square matrix
      -- Calculate dimension
      let len = length args
      let n = floor (sqrt (fromIntegral len) :: Double)
      if n * n /= len 
      then Left $ ParseError (InvalidSyntax "matrix must be square") 
                     ("det expects square number of arguments (1, 4, 9, 16...), got " ++ show len)
      else do
          elements <- mapM (exprFromSExpr intVars) args
          -- Chunk into rows
          let rows = chunk n elements
          Right $ Determinant rows

  "sum" -> case args of
    [Atom var, lo, hi, body] -> do
      l <- exprFromSExpr intVars lo
      h <- exprFromSExpr intVars hi
      b <- exprFromSExpr (S.insert var intVars) body
      Right $ Sum var l h b
    _ -> Left $ ParseError (WrongArity "sum" 4 (length args)) "Usage: (sum var lo hi body)"

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
                  "Formula logic (=, >=, >, <=, <) found inside expression. Use at top level only."
  ">=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                   "Formula logic (=, >=, >, <=, <) found inside expression. Use at top level only."
  ">" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >, <=, <) found inside expression. Use at top level only."
  "<=" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                   "Formula logic (=, >=, >, <=, <) found inside expression. Use at top level only."
  "<" -> Left $ ParseError (InvalidSyntax "formula in expression context")
                  "Formula logic (=, >=, >, <=, <) found inside expression. Use at top level only."

  -- If unknown operator, it might be a macro that wasn't expanded (e.g. wrong arity)
  _ -> Left $ ParseError (UnknownOperator op) ("Unknown operator or macro: " ++ op)

exprFromSExpr _ _ = Left $ ParseError (InvalidSyntax "invalid S-Expression")
                         "Invalid S-Expression format"

-- Parse strictly with macros
parseFormulaWithMacros :: MacroMap -> S.Set String -> String -> Either ProverError Formula
parseFormulaWithMacros macros intVars input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens
  expanded <- expandMacros macros sexpr
  formula <- formulaFromSExpr macros intVars expanded
  if null rest
     then Right formula
     else Left $ ParseError (ExtraTokens rest) ("Extra tokens after formula: " ++ show rest)

-- Legacy wrapper for compatibility
parseFormulaPrefix :: String -> Either ProverError Formula
parseFormulaPrefix = parseFormulaWithMacros M.empty S.empty

-- NEW: Parse formula and return remaining tokens (for :solve) with macros
parseFormulaWithRestAndMacros :: MacroMap -> S.Set String -> String -> Either ProverError (Formula, [String])
parseFormulaWithRestAndMacros macros intVars input = do
  let tokens = tokenizePrefix input
  (sexpr, rest) <- parseSExpr tokens
  expanded <- expandMacros macros sexpr
  formula <- formulaFromSExpr macros intVars expanded
  Right (formula, rest)

-- Legacy wrapper
parseFormulaWithRest :: String -> Either ProverError (Formula, [String])
parseFormulaWithRest = parseFormulaWithRestAndMacros M.empty S.empty

-- =============================================
-- Formula parsing helpers (support quantifiers)
-- =============================================

formulaFromSExpr :: MacroMap -> S.Set String -> SExpr -> Either ProverError Formula
formulaFromSExpr macros intVars sexpr =
  case sexpr of
    List (Atom "angle=abs" : args) ->
      case args of
        [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ Eq (AngleEq2DAbs a b c d e f) (Const 0)
        _ -> Left $ ParseError (WrongArity "angle=abs" 6 (length args)) "Usage: (angle=abs A B C D E F)"

    List (Atom "angle=" : args) ->
      case args of
        [Atom a, Atom b, Atom c, Atom d, Atom e, Atom f] -> Right $ Eq (AngleEq2D a b c d e f) (Const 0)
        _ -> Left $ ParseError (WrongArity "angle=" 6 (length args)) "Usage: (angle= A B C D E F)"

    List [Atom "=", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Eq l r

    List [Atom ">=", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Ge l r

    List [Atom ">", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Gt l r

    List [Atom "<=", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Le l r

    List [Atom "<", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Lt l r

    List [Atom "divides", lhs, rhs] -> do
      l <- exprFromSExpr intVars lhs
      r <- exprFromSExpr intVars rhs
      Right $ Divides l r

    List (Atom "and" : args) -> do
      fs <- mapM (formulaFromSExpr macros intVars) args
      case fs of
        [] -> Left $ ParseError (WrongArity "and" 2 0) "And requires at least 2 arguments (or 1)"
        [x] -> Right x
        _ -> Right $ foldr1 And fs

    -- root-between: (root-between var poly lo hi) expands to poly=0 ∧ lo<var ∧ var<hi
    List [Atom "root-between", Atom v, poly, lo, hi] -> do
      p <- exprFromSExpr intVars poly
      loE <- exprFromSExpr intVars lo
      hiE <- exprFromSExpr intVars hi
      let vExpr = if S.member v intVars then IntVar v else Var v
          eqPart = Eq p (Const 0)
          loPart = Gt vExpr loE
          hiPart = Lt vExpr hiE
      Right (And eqPart (And loPart hiPart))

    List (Atom "or" : args) -> do
      fs <- mapM (formulaFromSExpr macros intVars) args
      case fs of
        [] -> Left $ ParseError (WrongArity "or" 2 0) "Or requires at least 2 arguments (or 1)"
        [x] -> Right x
        _ -> Right $ foldr1 Or fs

    List [Atom "not", arg] -> do
      f <- formulaFromSExpr macros intVars arg
      Right $ Not f

    List [Atom "implies", p, q] -> do
      p' <- formulaFromSExpr macros intVars p
      q' <- formulaFromSExpr macros intVars q
      Right $ Or (Not p') q'

    List [Atom "forall", binderBlock, body] -> do
      qs <- parseBinderBlock intVars binderBlock
      -- Add bound integer variables to the set for the body?
      -- If declared 'int' in quantifier, it's tracked in QuantVar, but we might want `exprFromSExpr` to know.
      -- However, `exprFromSExpr` currently handles `IntVar` explicitly if it parses `(int x)`.
      -- If `(forall ((int n)) ...)` is used, `qs` has type QuantInt.
      -- Inside body, `n` should be `IntVar "n"`.
      -- So we should extend `intVars` for the body.
      let newInts = foldr S.insert intVars [ qvName q | q <- qs, qvType q == QuantInt ]
      inner <- formulaFromSExpr macros newInts body
      Right $ Forall qs inner

    List [Atom "exists", binderBlock, body] -> do
      qs <- parseBinderBlock intVars binderBlock
      let newInts = foldr S.insert intVars [ qvName q | q <- qs, qvType q == QuantInt ]
      inner <- formulaFromSExpr macros newInts body
      Right $ Exists qs inner

    List [Atom op, _, _] | op `elem` ["=", ">=", ">", "<=", "<"] ->
      Left $ ParseError (InvalidSyntax "not a formula") "Expected format: (= lhs rhs) OR (>= lhs rhs) OR (> lhs rhs) OR (<= lhs rhs) OR (< lhs rhs)"

    _ -> Left $ ParseError (InvalidSyntax "not a formula")
                  "Expected format: (= lhs rhs) OR (>= lhs rhs) OR (> lhs rhs) OR (<= lhs rhs) OR (< lhs rhs) OR a quantifier (forall/exists)"

parseQuantVar :: S.Set String -> SExpr -> Either ProverError QuantVar
parseQuantVar _ (Atom v) = Right $ QuantVar v QuantReal Nothing Nothing
parseQuantVar _ (List [Atom v]) = Right $ QuantVar v QuantReal Nothing Nothing
parseQuantVar intVars (List [Atom v, lo, hi]) = do
  loE <- exprFromSExpr intVars lo
  hiE <- exprFromSExpr intVars hi
  Right $ QuantVar v QuantReal (Just loE) (Just hiE)
parseQuantVar _ (List [Atom "int", Atom v]) = Right $ QuantVar v QuantInt Nothing Nothing
parseQuantVar intVars (List [Atom "int", Atom v, lo, hi]) = do
  loE <- exprFromSExpr intVars lo
  hiE <- exprFromSExpr intVars hi
  Right $ QuantVar v QuantInt (Just loE) (Just hiE)
parseQuantVar _ (List [Atom "real", Atom v]) = Right $ QuantVar v QuantReal Nothing Nothing
parseQuantVar intVars (List [Atom "real", Atom v, lo, hi]) = do
  loE <- exprFromSExpr intVars lo
  hiE <- exprFromSExpr intVars hi
  Right $ QuantVar v QuantReal (Just loE) (Just hiE)
parseQuantVar _ bad =
  Left $ ParseError (InvalidSyntax "quantifier binder") ("Invalid binder: " ++ show bad ++ ". Expected name or (int name) or (name lo hi).")

parseBinderBlock :: S.Set String -> SExpr -> Either ProverError [QuantVar]
parseBinderBlock intVars (List binders) = mapM (parseQuantVar intVars) binders
parseBinderBlock intVars single = mapM (parseQuantVar intVars) [single]

-- Helper: Chunk list into sublists of size n
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
