{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Error
-- Description : Hierarchical error types for the geometric theorem prover
-- Copyright   : (c) 2024-2025
-- License     : MIT
-- Stability   : stable
--
-- This module defines a comprehensive error type hierarchy for the prover,
-- providing structured error handling with rich context information.
--
-- = Error Categories
--
-- The prover distinguishes between several error categories:
--
--   * 'ParseError' - Syntax errors during formula parsing
--   * 'ProofError' - Failures during proof attempts
--   * 'ValidationError' - Invalid geometric configurations
--   * 'TimeoutError' - Computation exceeded time limits
--   * 'PolynomialError' - Polynomial arithmetic errors
--
-- = Usage
--
-- Errors are typically created by the various solver modules and can be
-- formatted for user display using 'formatError':
--
-- @
-- case proveTheorem theory goal of
--   Left err -> putStrLn (formatError err)
--   Right proof -> displayProof proof
-- @
--
-- = Error Recovery
--
-- Some error types support recovery strategies:
--
--   * 'TimeoutError' - Can retry with increased timeout
--   * 'MultivariateFallback' - Can switch to CAD solver
--   * 'UnsupportedFormula' - May try different proof strategy

module Error
  ( -- * Main Error Type
    ProverError(..)
    -- * Error Subtypes
  , ParseErrorType(..)
  , ProofErrorType(..)
  , ValidationErrorType(..)
  , TimeoutErrorType(..)
  , PolynomialErrorType(..)
    -- * Error Formatting
  , formatError
    -- * Utilities
  , toEither
  ) where

import GHC.Generics (Generic)

-- =============================================
-- Error Type Hierarchy
-- =============================================

-- | The main error type for all prover operations.
--
-- 'ProverError' is a sum type that categorizes errors by their source
-- and provides context information for debugging and user feedback.
--
-- Most constructors carry a 'String' for additional context about
-- where and why the error occurred.
data ProverError
  = ParseError ParseErrorType String        -- Parse failures with context
  | ProofError ProofErrorType String        -- Proof failures with reason
  | ValidationError ValidationErrorType String  -- Validation failures
  | DivisionByZero String                   -- Division by zero
  | FileError String                        -- File I/O errors
  | CADError String                         -- CAD computation errors
  | UnsupportedOperation String             -- Operation not supported in context
  | MathematicalError String                -- Mathematical impossibility
  | InternalError String                    -- Should never happen
  | TimeoutError TimeoutErrorType           -- Computation timeout
  | PolynomialError PolynomialErrorType     -- Polynomial-specific errors
  | ModularError String                     -- Modular arithmetic failures
  deriving (Eq, Show, Generic)

-- | Timeout error types for different computational phases.
--
-- Each computation-heavy algorithm has its own timeout variant,
-- allowing for targeted timeout configuration and error handling.
data TimeoutErrorType
  = BuchbergerTimeout
    -- ^ Groebner basis computation (Buchberger algorithm) exceeded time limit.
    -- This often occurs with polynomials having many variables or high degrees.
  | CADTimeout
    -- ^ Cylindrical Algebraic Decomposition exceeded time limit.
    -- CAD has doubly-exponential worst-case complexity.
  | CADLiftTimeout
    -- ^ CAD lifting phase exceeded time limit.
    -- Lifting constructs sample points in higher dimensions.
  | IntSolverTimeout
    -- ^ Integer solver exceeded time limit.
    -- May indicate need for different solution strategy.
  | GeneralTimeout String
    -- ^ General computation timeout with context description.
  deriving (Eq, Show, Generic)

-- | Polynomial-specific error types.
--
-- These errors arise during polynomial arithmetic operations,
-- typically in Groebner basis computation or CAD.
data PolynomialErrorType
  = ZeroPolynomialLeadingTerm
    -- ^ Attempted to get leading term of the zero polynomial.
    -- This is mathematically undefined.
  | PolynomialTooLarge Int
    -- ^ Polynomial exceeds the configured term limit.
    -- The 'Int' parameter indicates the number of terms.
  | InvalidPolynomialOperation String
    -- ^ General invalid polynomial operation with description.
  | EmptyBasis
    -- ^ Empty polynomial basis provided where at least one generator required.
  deriving (Eq, Show, Generic)

-- | Parse error types for formula and expression parsing.
--
-- These errors are produced by the parser when processing
-- Euclid language files (.euclid) or REPL input.
data ParseErrorType
  = UnexpectedToken String
    -- ^ Encountered an unexpected token during parsing.
  | MissingClosingParen
    -- ^ Missing closing parenthesis in expression.
  | InvalidSyntax String
    -- ^ General syntax error with description.
  | UnknownOperator String
    -- ^ Unknown operator or function name.
  | WrongArity String Int Int
    -- ^ Operator with wrong number of arguments.
    -- Parameters: operator name, expected count, actual count.
  | InvalidNumber String
    -- ^ Malformed numeric literal.
  | EmptyExpression
    -- ^ Empty input where expression was expected.
  | ExtraTokens [String]
    -- ^ Leftover tokens after parsing complete expression.
  | MacroExpansionDepthExceeded Int
    -- ^ Macro expansion exceeded maximum recursion depth.
    -- This typically indicates circular macro definitions.
  deriving (Eq, Show, Generic)

-- | Proof error types for theorem proving failures.
--
-- These errors indicate that the prover could not establish
-- the desired result, either because it's false or because
-- the current solver strategy is insufficient.
data ProofErrorType
  = EqualityFailed String
    -- ^ Polynomial equality does not hold.
    -- The string contains the non-zero normal form.
  | InequalityFailed String
    -- ^ Polynomial inequality does not hold with explanation.
  | SturmFailed Int
    -- ^ Sturm sequence analysis found real roots.
    -- The 'Int' is the number of roots found (expected 0 for positivity).
  | MultivariateFallback
    -- ^ Univariate method cannot handle multivariate polynomial.
    -- Consider switching to CAD or SOS solver.
  | ConstantNegative Rational
    -- ^ A constant expression evaluated to a negative value.
  | UnsupportedFormula String
    -- ^ Formula type not supported by the current solver.
  deriving (Eq, Show, Generic)

-- | Validation error types for geometric configuration checks.
--
-- These errors detect degenerate or invalid geometric configurations
-- before attempting proof, preventing meaningless results.
data ValidationErrorType
  = UndefinedPoint String
    -- ^ Referenced point has not been defined.
  | CoincidentPoints String String
    -- ^ Two distinct points are required but the same point was given.
  | ZeroLengthSegment String String
    -- ^ Line segment has zero length (endpoints coincide).
  | CollinearPoints String String String
    -- ^ Three non-collinear points required but points are collinear.
  | DegenerateConfiguration String
    -- ^ General degenerate configuration with description.
  deriving (Eq, Show, Generic)

-- =============================================
-- Error Formatting
-- =============================================

-- | Format a 'ProverError' as a human-readable string.
--
-- This function produces multi-line error messages suitable for
-- display in the REPL or command-line output. Each error type
-- is formatted with appropriate context and suggestions.
--
-- ==== Example
--
-- >>> formatError (ParseError (UnexpectedToken "foo") "line 5")
-- "Parse Error: Unexpected token: 'foo'\nContext: line 5"
formatError :: ProverError -> String
formatError (ParseError errType ctx) =
  "Parse Error: " ++ formatParseError errType ++ "\n" ++
  "Context: " ++ ctx

formatError (ProofError errType reason) =
  "Proof Failed: " ++ formatProofError errType ++ "\n" ++
  "Reason: " ++ reason

formatError (ValidationError errType reason) =
  "Validation Error: " ++ formatValidationError errType ++ "\n" ++
  "Details: " ++ reason

formatError (DivisionByZero ctx) =
  "Division by Zero Error\n" ++
  "Context: " ++ ctx

formatError (FileError msg) =
  "File I/O Error: " ++ msg

formatError (CADError msg) =
  "CAD Computation Error: " ++ msg

formatError (UnsupportedOperation msg) =
  "Unsupported Operation: " ++ msg

formatError (MathematicalError msg) =
  "Mathematical Error: " ++ msg

formatError (InternalError msg) =
  "Internal Error (this should not happen!): " ++ msg ++ "\n" ++
  "Please report this as a bug at https://github.com/TACITVS/Hasclid/issues"

formatError (TimeoutError errType) =
  "Timeout Error: " ++ formatTimeoutError errType

formatError (PolynomialError errType) =
  "Polynomial Error: " ++ formatPolynomialError errType

formatError (ModularError msg) =
  "Modular Arithmetic Error: " ++ msg

-- Format Timeout Errors
formatTimeoutError :: TimeoutErrorType -> String
formatTimeoutError BuchbergerTimeout =
  "Groebner basis computation timed out (Buchberger algorithm)"
formatTimeoutError CADTimeout =
  "CAD decomposition timed out"
formatTimeoutError CADLiftTimeout =
  "CAD lift operation timed out"
formatTimeoutError IntSolverTimeout =
  "Integer solver timed out"
formatTimeoutError (GeneralTimeout ctx) =
  "Computation timed out: " ++ ctx

-- Format Polynomial Errors
formatPolynomialError :: PolynomialErrorType -> String
formatPolynomialError ZeroPolynomialLeadingTerm =
  "Attempted to get leading term of zero polynomial"
formatPolynomialError (PolynomialTooLarge size) =
  "Polynomial exceeds size limit (" ++ show size ++ " terms)"
formatPolynomialError (InvalidPolynomialOperation op) =
  "Invalid polynomial operation: " ++ op
formatPolynomialError EmptyBasis =
  "Empty polynomial basis (no generators)"

-- Format Parse Errors
formatParseError :: ParseErrorType -> String
formatParseError (UnexpectedToken tok) =
  "Unexpected token: '" ++ tok ++ "'"
formatParseError MissingClosingParen =
  "Missing closing parenthesis ')'"
formatParseError (InvalidSyntax msg) =
  "Invalid syntax: " ++ msg
formatParseError (UnknownOperator op) =
  "Unknown operator: '" ++ op ++ "'"
formatParseError (WrongArity op expected got) =
  "Operator '" ++ op ++ "' expects " ++ show expected ++
  " arguments, but got " ++ show got
formatParseError (InvalidNumber num) =
  "Invalid number format: '" ++ num ++ "'"
formatParseError EmptyExpression =
  "Empty expression (nothing to parse)"
formatParseError (ExtraTokens tokens) =
  "Extra tokens after formula: " ++ show tokens
formatParseError (MacroExpansionDepthExceeded depth) =
  "Macro expansion exceeded depth limit (" ++ show depth ++ " levels). " ++
  "Possible infinite recursion in macro definitions."

-- Format Proof Errors
formatProofError :: ProofErrorType -> String
formatProofError (EqualityFailed poly) =
  "Equality does not hold. Normal form: " ++ poly
formatProofError (InequalityFailed reason) =
  "Inequality does not hold: " ++ reason
formatProofError (SturmFailed nRoots) =
  "Sturm analysis failed. Found " ++ show nRoots ++ " real roots (expected 0)"
formatProofError MultivariateFallback =
  "Cannot prove multivariate inequality (Sturm requires univariate polynomial)"
formatProofError (ConstantNegative val) =
  "Constant value is negative: " ++ show val
formatProofError (UnsupportedFormula msg) =
  "Formula not supported by this solver: " ++ msg

-- Format Validation Errors
formatValidationError :: ValidationErrorType -> String
formatValidationError (UndefinedPoint p) =
  "Point '" ++ p ++ "' is not defined"
formatValidationError (CoincidentPoints p1 p2) =
  "Points '" ++ p1 ++ "' and '" ++ p2 ++ "' are coincident (same location)"
formatValidationError (ZeroLengthSegment p1 p2) =
  "Segment '" ++ p1 ++ p2 ++ "' has zero length"
formatValidationError (CollinearPoints p1 p2 p3) =
  "Points '" ++ p1 ++ "', '" ++ p2 ++ "', '" ++ p3 ++ "' are collinear"
formatValidationError (DegenerateConfiguration msg) =
  "Degenerate geometric configuration: " ++ msg

-- =============================================
-- Utility Functions
-- =============================================

-- | Convert an error value to a 'Left' result.
--
-- This is a convenience function for error construction.
-- Note: The second argument is ignored; this always returns 'Left'.
--
-- ==== Example
--
-- >>> toEither (DivisionByZero "x/y") undefined
-- Left (DivisionByZero "x/y")
toEither :: ProverError -> a -> Either ProverError a
toEither err _ = Left err
