{-# LANGUAGE DeriveGeneric #-}

module Error
  ( ProverError(..)
  , ParseErrorType(..)
  , ProofErrorType(..)
  , ValidationErrorType(..)
  , formatError
  , toEither
  ) where

import GHC.Generics (Generic)

-- =============================================
-- Error Type Hierarchy
-- =============================================

data ProverError
  = ParseError ParseErrorType String        -- Parse failures with context
  | ProofError ProofErrorType String        -- Proof failures with reason
  | ValidationError ValidationErrorType String  -- Validation failures
  | DivisionByZero String                   -- Division by zero
  | FileError String                        -- File I/O errors
  | CADError String                         -- CAD computation errors
  | InternalError String                    -- Should never happen
  deriving (Eq, Show, Generic)

data ParseErrorType
  = UnexpectedToken String                  -- Unexpected token in input
  | MissingClosingParen                     -- Missing )
  | InvalidSyntax String                    -- General syntax error
  | UnknownOperator String                  -- Unknown operator
  | WrongArity String Int Int               -- Operator with wrong # of args (expected, got)
  | InvalidNumber String                    -- Malformed number
  | EmptyExpression                         -- Empty input
  | ExtraTokens [String]                    -- Leftover tokens after parsing
  | MacroExpansionDepthExceeded Int         -- Macro expansion exceeded depth limit
  deriving (Eq, Show, Generic)

data ProofErrorType
  = EqualityFailed String                   -- Equality doesn't hold
  | InequalityFailed String                 -- Inequality doesn't hold
  | SturmFailed Int                         -- Sturm check failed (# roots)
  | MultivariateFallback                    -- Can't handle multivariate
  | ConstantNegative Rational               -- Constant is negative
  deriving (Eq, Show, Generic)

data ValidationErrorType
  = UndefinedPoint String                   -- Point not defined
  | CoincidentPoints String String          -- Two points are the same
  | ZeroLengthSegment String String         -- Segment has zero length
  | CollinearPoints String String String    -- Three points are collinear
  | DegenerateConfiguration String          -- General degeneracy
  deriving (Eq, Show, Generic)

-- =============================================
-- Error Formatting
-- =============================================

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

formatError (InternalError msg) =
  "Internal Error (this should not happen!): " ++ msg ++ "\n" ++
  "Please report this as a bug at https://github.com/TACITVS/Hasclid/issues"

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
-- Helper Function
-- =============================================

-- Convert error-throwing code to Either
toEither :: ProverError -> a -> Either ProverError a
toEither err _ = Left err
