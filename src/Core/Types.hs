{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Core.Types
-- Description : Core types and operations for the geometric theorem prover
-- Copyright   : (c) 2024-2025
-- License     : MIT
-- Stability   : stable
--
-- This module provides the fundamental data types for representing
-- mathematical expressions, formulas, polynomials, and their operations.
--
-- = Expression Language
--
-- The 'Expr' type represents symbolic mathematical expressions including:
--
--   * Arithmetic: addition, subtraction, multiplication, division, powers
--   * Geometry: distances, collinearity, dot products, circles
--   * Special constructs: determinants, summations
--
-- = Polynomial Engine
--
-- The 'Poly' type represents multivariate polynomials with rational coefficients.
-- Polynomials support standard arithmetic operations and are used as the
-- normal form for algebraic reasoning.
--
-- = Formula Language
--
-- The 'Formula' type represents logical formulas including:
--
--   * Comparisons: equality, inequalities
--   * Connectives: and, or, not
--   * Quantifiers: forall, exists (with bounds)
--
-- = Usage Example
--
-- @
-- -- Create a polynomial from an expression
-- let expr = Add (Var "x") (Mul (Const 2) (Var "y"))
-- let poly = toPoly expr  -- x + 2y
--
-- -- Create a formula
-- let formula = Ge (Pow (Var "x") 2) (Const 0)  -- x^2 >= 0
-- @
module Core.Types
  ( -- * Expression Types
    Expr(..)
  , Formula(..)
  , QuantVar(..)
  , QuantType(..)
  , Theory

    -- * Polynomial Types
  , Poly(..)
  , Monomial(..)

    -- * Pretty Printing (ASCII)
  , prettyExpr
  , prettyFormula
  , prettyPoly
  , prettyPolyNice
  , prettyMonomial
  , prettyRational
  , prettyQuantVar
  , prettyBounds

    -- * Pretty Printing (LaTeX)
  , prettyExprLaTeX
  , prettyFormulaLaTeX
  , prettyPolyLaTeX
  , prettyMonomialLaTeX
  , prettyRationalLaTeX

    -- * Monomial Operations
  , monomialOne
  , monomialMul
  , monomialDiv
  , monomialLCM
  , monomialGCD

    -- * Polynomial Construction
  , polyZero
  , polyFromConst
  , polyFromVar

    -- * Polynomial Arithmetic
  , polyAdd
  , polyNeg
  , polySub
  , polyMul
  , polyPow
  , polyScale

    -- * Polynomial Queries
  , getLeadingTerm
  , getLeadingTermByOrder
  , getVars
  , getMainVar
  , isConstPoly
  , polyContent
  , polyPrimitive
  , polyDerivative

    -- * Expression Manipulation
  , simplifyExpr
  , substituteExpr
  , substituteAll
  , normalizeCommAssoc
  , mapExpr

    -- * Conversion Functions
  , toPoly
  , polyToExpr
  , toUnivariate
  , fromUnivariate
  , buildSubMap
  , evaluatePoly
  , toPolySub

    -- * Predicates
  , containsSqrtExpr
  , containsSqrtFormula
  , containsDivExpr
  , containsDivFormula
  , containsIntExpr
  , containsIntFormula
  , containsQuantifier
  , hasNonPolynomial
  , exprEqualsSymbolic

    -- * Integer/Formula Operations
  , substituteInts
  , substituteIntsExpr

    -- * Helper Functions
  , collectAdd
  , collectMul
  , partitionConsts
  , transpose
  , hasDuplicates
  , integerSqrt
  , rationalGCD
  , varsInExpr
  , varsInFormula
  ) where

import Data.List (intercalate, sortBy, nub, maximumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric.Natural (Natural)
import Data.Ratio ((%), numerator, denominator)
import Error (ProverError(..), formatError)

-- =============================================
-- Symbolic Expressions (AST)
-- =============================================

-- | Symbolic mathematical expression.
--
-- This is the primary AST for representing mathematical formulas before
-- conversion to polynomial form. Supports arithmetic, geometry primitives,
-- and high-level constructs like determinants and summations.
--
-- Most expressions can be converted to 'Poly' using 'toPoly', except for
-- those containing division by variables, square roots of non-perfect-squares,
-- or modulo operations.
data Expr
  = Var String
    -- ^ Real-valued variable (e.g., @Var \"x\"@ represents x)
  | Const Rational
    -- ^ Rational constant (e.g., @Const (1 % 2)@ represents 1/2)
  | IntVar String
    -- ^ Integer-domain variable for Presburger arithmetic
  | IntConst Integer
    -- ^ Integer constant
  | Add Expr Expr
    -- ^ Addition: @Add a b@ represents @a + b@
  | Sub Expr Expr
    -- ^ Subtraction: @Sub a b@ represents @a - b@
  | Mul Expr Expr
    -- ^ Multiplication: @Mul a b@ represents @a * b@
  | Div Expr Expr
    -- ^ Division: @Div a b@ represents @a / b@ (non-polynomial if b has variables)
  | Mod Expr Expr
    -- ^ Modulo: @Mod a b@ represents @a mod b@
  | Pow Expr Natural
    -- ^ Power with natural exponent: @Pow e n@ represents @e^n@
  | Sqrt Expr
    -- ^ Square root (may not convert to polynomial)
  | NthRoot Natural Expr
    -- ^ Nth root: @NthRoot n e@ represents @e^(1/n)@ for n >= 2
    -- NthRoot 2 e is semantically equivalent to Sqrt e
    -- NthRoot 3 e is the cube root, etc.
  -- Trigonometric functions
  | Sin Expr
    -- ^ Sine: @Sin x@
  | Cos Expr
    -- ^ Cosine: @Cos x@
  | Tan Expr
    -- ^ Tangent: @Tan x@
  | Asin Expr
    -- ^ Arcsine: @Asin x@
  | Acos Expr
    -- ^ Arccosine: @Acos x@
  | Atan Expr
    -- ^ Arctangent: @Atan x@
  -- Geometric primitives
  | Dist2 String String
    -- ^ Squared Euclidean distance: @Dist2 \"A\" \"B\"@ = |AB|^2
  | Collinear String String String
    -- ^ Collinearity constraint: points A, B, C lie on a line
  | Dot String String String String
    -- ^ Dot product: @Dot \"A\" \"B\" \"C\" \"D\"@ = AB . CD
  | Circle String String Expr
    -- ^ Point on circle: @Circle \"P\" \"C\" r@ means P is on circle with center C, radius r
  -- Geometry helpers
  | Midpoint String String String
    -- ^ Midpoint: @Midpoint \"A\" \"B\" \"M\"@ means M is midpoint of AB
  | Perpendicular String String String String
    -- ^ Perpendicularity: @Perpendicular \"A\" \"B\" \"C\" \"D\"@ means AB perp CD
  | Parallel String String String String
    -- ^ Parallelism: @Parallel \"A\" \"B\" \"C\" \"D\"@ means AB parallel CD
  | AngleEq2D String String String String String String
    -- ^ Oriented angle equality in 2D: angle ABC = angle DEF
  | AngleEq2DAbs String String String String String String
    -- ^ Unsigned angle equality in 2D: |angle ABC| = |angle DEF|
  -- High-level algebraic primitives
  | Determinant [[Expr]]
    -- ^ Matrix determinant (lazy expansion)
  | Sum String Expr Expr Expr
    -- ^ Summation: @Sum \"i\" lo hi body@ = sum from i=lo to hi of body
  deriving (Eq, Ord, Show)

-- | Quantifier domain: real or integer.
data QuantType
  = QuantReal  -- ^ Real-valued quantifier (default for geometric proofs)
  | QuantInt   -- ^ Integer-valued quantifier (for number theory)
  deriving (Eq, Show)

-- | Bound variable for quantified formulas.
--
-- Quantified variables can have optional lower and upper bounds,
-- which restrict the domain of quantification.
data QuantVar = QuantVar
  { qvName  :: String      -- ^ Variable name
  , qvType  :: QuantType   -- ^ Domain (real or integer)
  , qvLower :: Maybe Expr  -- ^ Optional lower bound
  , qvUpper :: Maybe Expr  -- ^ Optional upper bound
  } deriving (Eq, Show)

-- | Logical formula for theorem statements.
--
-- Formulas combine expressions with relational operators and
-- logical connectives. The prover attempts to verify these formulas
-- using various algebraic and geometric techniques.
data Formula
  = Eq Expr Expr
    -- ^ Equality: @Eq a b@ means @a = b@
  | Ge Expr Expr
    -- ^ Greater or equal: @Ge a b@ means @a >= b@
  | Gt Expr Expr
    -- ^ Strictly greater: @Gt a b@ means @a > b@
  | Le Expr Expr
    -- ^ Less or equal: @Le a b@ means @a <= b@
  | Lt Expr Expr
    -- ^ Strictly less: @Lt a b@ means @a < b@
  | Divides Expr Expr
    -- ^ Divisibility: @Divides a b@ means @a | b@ (a divides b)
  | And Formula Formula
    -- ^ Conjunction: both formulas must hold
  | Or Formula Formula
    -- ^ Disjunction: at least one formula must hold
  | Not Formula
    -- ^ Negation
  | Forall [QuantVar] Formula
    -- ^ Universal quantifier: for all values of variables
  | Exists [QuantVar] Formula
    -- ^ Existential quantifier: there exist values of variables
  deriving (Eq, Show)

prettyExpr :: Expr -> String
prettyExpr (Var x)      = x
prettyExpr (Const r)    = prettyRational r
prettyExpr (IntVar x)   = "(int " ++ x ++ ")"
prettyExpr (IntConst i) = "(int-const " ++ show i ++ ")"
prettyExpr (Add e1 e2)  = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2)  = "(- " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Mul e1 e2)  = "(* " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Div e1 e2)  = "(/ " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Mod e1 e2)  = "(mod " ++ prettyExpr e1 ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Pow e n)    = "(^ " ++ prettyExpr e ++ " " ++ show n ++ ")"
prettyExpr (Sqrt e)     = "(sqrt " ++ prettyExpr e ++ ")"
prettyExpr (NthRoot 2 e) = "(sqrt " ++ prettyExpr e ++ ")"  -- Canonical form
prettyExpr (NthRoot 3 e) = "(cbrt " ++ prettyExpr e ++ ")"
prettyExpr (NthRoot n e) = "(root " ++ show n ++ " " ++ prettyExpr e ++ ")"
prettyExpr (Sin e)      = "(sin " ++ prettyExpr e ++ ")"
prettyExpr (Cos e)      = "(cos " ++ prettyExpr e ++ ")"
prettyExpr (Tan e)      = "(tan " ++ prettyExpr e ++ ")"
prettyExpr (Asin e)     = "(asin " ++ prettyExpr e ++ ")"
prettyExpr (Acos e)     = "(acos " ++ prettyExpr e ++ ")"
prettyExpr (Atan e)     = "(atan " ++ prettyExpr e ++ ")"
prettyExpr (Dist2 p1 p2) = "(dist2 " ++ p1 ++ " " ++ p2 ++ ")"
prettyExpr (Collinear p1 p2 p3) = "(collinear " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ ")"
prettyExpr (Dot a b c d) = "(dot " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (Circle p c r) = "(circle " ++ p ++ " " ++ c ++ " " ++ prettyExpr r ++ ")"
prettyExpr (Midpoint a b m) = "(midpoint " ++ a ++ " " ++ b ++ " " ++ m ++ ")"
prettyExpr (Perpendicular a b c d) = "(perpendicular " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (Parallel a b c d) = "(parallel " ++ a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ ")"
prettyExpr (AngleEq2D a b c d e f) = "(angle-eq " ++ unwords [a,b,c,d,e,f] ++ ")"
prettyExpr (AngleEq2DAbs a b c d e f) = "(angle-eq-abs " ++ unwords [a,b,c,d,e,f] ++ ")"
prettyExpr (Determinant rows) =
  case rows of
    (r0:_) -> "(det " ++ show (length rows) ++ "x" ++ show (length r0) ++ ")"
    []     -> "(det 0x0)"
prettyExpr (Sum v lo hi body) = "(sum " ++ v ++ " " ++ prettyExpr lo ++ " " ++ prettyExpr hi ++ " " ++ prettyExpr body ++ ")"

prettyRational :: Rational -> String
prettyRational r
  | d == 1    = show n
  | otherwise = show n ++ "/" ++ show d
  where n = numerator r
        d = denominator r

prettyQuantVar :: QuantVar -> String
prettyQuantVar (QuantVar v QuantReal lo hi) = v ++ prettyBounds lo hi
prettyQuantVar (QuantVar v QuantInt lo hi)  = "(int " ++ v ++ ")" ++ prettyBounds lo hi

prettyBounds :: Maybe Expr -> Maybe Expr -> String
prettyBounds Nothing Nothing = ""
prettyBounds (Just l) (Just u) = " in [" ++ prettyExpr l ++ ", " ++ prettyExpr u ++ "]"
prettyBounds (Just l) Nothing  = " >= " ++ prettyExpr l
prettyBounds Nothing (Just u)  = " <= " ++ prettyExpr u

prettyFormula :: Formula -> String
prettyFormula (Eq l r) = "(= " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (Ge l r) = "(>= " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (Gt l r) = "(> " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (Le l r) = "(<= " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (Lt l r) = "(< " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (Divides l r) = "(divides " ++ prettyExpr l ++ " " ++ prettyExpr r ++ ")"
prettyFormula (And f1 f2) = "(and " ++ prettyFormula f1 ++ " " ++ prettyFormula f2 ++ ")"
prettyFormula (Or f1 f2) = "(or " ++ prettyFormula f1 ++ " " ++ prettyFormula f2 ++ ")"
prettyFormula (Not f) = "(not " ++ prettyFormula f ++ ")"
prettyFormula (Forall qs f) =
  "(forall (" ++ unwords (map prettyQuantVar qs) ++ ") " ++ prettyFormula f ++ ")"
prettyFormula (Exists qs f) =
  "(exists (" ++ unwords (map prettyQuantVar qs) ++ ") " ++ prettyFormula f ++ ")"

-- =============================================
-- LaTeX Pretty Printing
-- =============================================

-- | Format a rational number in LaTeX.
-- Uses \\frac{}{} for non-integer rationals, plain integer otherwise.
prettyRationalLaTeX :: Rational -> String
prettyRationalLaTeX r
  | d == 1    = show n
  | n < 0     = "-\\frac{" ++ show (abs n) ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  where n = numerator r
        d = denominator r

-- | Format a monomial in LaTeX with proper superscripts.
-- e.g., x^2*y^3 becomes x^{2}y^{3}
prettyMonomialLaTeX :: Monomial -> String
prettyMonomialLaTeX (Monomial m)
  | M.null m = ""
  | otherwise = concatMap formatVar (M.toAscList m)
  where
    formatVar (v, 1) = formatVarName v
    formatVar (v, e) = formatVarName v ++ "^{" ++ show e ++ "}"
    -- Format variable names: subscripts for trailing digits
    formatVarName v =
      let (base, suffix) = span (not . (`elem` ['0'..'9'])) v
      in if null suffix
         then v
         else base ++ "_{" ++ suffix ++ "}"

-- | Format a polynomial in LaTeX with inline math notation.
prettyPolyLaTeX :: Poly -> String
prettyPolyLaTeX (Poly m)
  | M.null m = "0"
  | otherwise =
      let terms = sortTerms [ (c, mono) | (mono, c) <- M.toList m, c /= 0 ]
      in formatTerms terms
  where
    sortTerms = sortBy (\(_, m1) (_, m2) ->
      let deg1 = totalDegree m1
          deg2 = totalDegree m2
      in compare deg2 deg1 <> compare m2 m1)

    totalDegree (Monomial vars) = sum (M.elems vars)

    formatTerms [] = "0"
    formatTerms [(c, mono)] = formatFirstTerm c mono
    formatTerms ((c, mono):rest) = formatFirstTerm c mono ++ concatMap (uncurry formatOtherTerm) rest

    formatFirstTerm c mono
      | M.null (let Monomial mm = mono in mm) = prettyRationalLaTeX c
      | c == 1 = prettyMonomialLaTeX mono
      | c == -1 = "-" ++ prettyMonomialLaTeX mono
      | otherwise = prettyRationalLaTeX c ++ " " ++ prettyMonomialLaTeX mono

    formatOtherTerm c mono
      | c < 0 = " - " ++ formatCoeff (abs c) mono
      | otherwise = " + " ++ formatCoeff c mono

    formatCoeff c mono
      | M.null (let Monomial mm = mono in mm) = prettyRationalLaTeX c
      | c == 1 = prettyMonomialLaTeX mono
      | otherwise = prettyRationalLaTeX c ++ " " ++ prettyMonomialLaTeX mono

-- | Format an expression in LaTeX with inline math notation.
-- Produces proper mathematical notation with superscripts, fractions, etc.
prettyExprLaTeX :: Expr -> String
prettyExprLaTeX (Var x)      = formatVarNameLaTeX x
prettyExprLaTeX (Const r)    = prettyRationalLaTeX r
prettyExprLaTeX (IntVar x)   = formatVarNameLaTeX x
prettyExprLaTeX (IntConst i) = show i
prettyExprLaTeX (Add e1 e2)  = prettyExprLaTeX e1 ++ " + " ++ prettyExprLaTeX e2
prettyExprLaTeX (Sub e1 e2)  = prettyExprLaTeX e1 ++ " - " ++ wrapIfSum e2
  where
    wrapIfSum e@(Add _ _) = "(" ++ prettyExprLaTeX e ++ ")"
    wrapIfSum e@(Sub _ _) = "(" ++ prettyExprLaTeX e ++ ")"
    wrapIfSum e = prettyExprLaTeX e
prettyExprLaTeX (Mul e1 e2)  = wrapIfAddSub e1 ++ " \\cdot " ++ wrapIfAddSub e2
  where
    wrapIfAddSub e@(Add _ _) = "(" ++ prettyExprLaTeX e ++ ")"
    wrapIfAddSub e@(Sub _ _) = "(" ++ prettyExprLaTeX e ++ ")"
    wrapIfAddSub e = prettyExprLaTeX e
prettyExprLaTeX (Div e1 e2)  = "\\frac{" ++ prettyExprLaTeX e1 ++ "}{" ++ prettyExprLaTeX e2 ++ "}"
prettyExprLaTeX (Mod e1 e2)  = prettyExprLaTeX e1 ++ " \\mod " ++ prettyExprLaTeX e2
prettyExprLaTeX (Pow e n)    = wrapIfComplex e ++ "^{" ++ show n ++ "}"
  where
    wrapIfComplex (Var x) = formatVarNameLaTeX x
    wrapIfComplex (Const r) = prettyRationalLaTeX r
    wrapIfComplex e' = "(" ++ prettyExprLaTeX e' ++ ")"
prettyExprLaTeX (Sqrt e)     = "\\sqrt{" ++ prettyExprLaTeX e ++ "}"
prettyExprLaTeX (NthRoot 2 e) = "\\sqrt{" ++ prettyExprLaTeX e ++ "}"
prettyExprLaTeX (NthRoot 3 e) = "\\sqrt[3]{" ++ prettyExprLaTeX e ++ "}"
prettyExprLaTeX (NthRoot n e) = "\\sqrt[" ++ show n ++ "]{" ++ prettyExprLaTeX e ++ "}"
prettyExprLaTeX (Sin e)      = "\\sin(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Cos e)      = "\\cos(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Tan e)      = "\\tan(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Asin e)     = "\\arcsin(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Acos e)     = "\\arccos(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Atan e)     = "\\arctan(" ++ prettyExprLaTeX e ++ ")"
prettyExprLaTeX (Dist2 p1 p2) = "|" ++ p1 ++ p2 ++ "|^{2}"
prettyExprLaTeX (Collinear p1 p2 p3) = "\\text{collinear}(" ++ p1 ++ ", " ++ p2 ++ ", " ++ p3 ++ ")"
prettyExprLaTeX (Dot a b c d) = "\\vec{" ++ a ++ b ++ "} \\cdot \\vec{" ++ c ++ d ++ "}"
prettyExprLaTeX (Circle p c r) = "\\text{circle}(" ++ p ++ ", " ++ c ++ ", " ++ prettyExprLaTeX r ++ ")"
prettyExprLaTeX (Midpoint a b m) = m ++ " = \\text{midpoint}(" ++ a ++ ", " ++ b ++ ")"
prettyExprLaTeX (Perpendicular a b c d) = a ++ b ++ " \\perp " ++ c ++ d
prettyExprLaTeX (Parallel a b c d) = a ++ b ++ " \\parallel " ++ c ++ d
prettyExprLaTeX (AngleEq2D a b c d e f) = "\\angle " ++ a ++ b ++ c ++ " = \\angle " ++ d ++ e ++ f
prettyExprLaTeX (AngleEq2DAbs a b c d e f) = "|\\angle " ++ a ++ b ++ c ++ "| = |\\angle " ++ d ++ e ++ f ++ "|"
prettyExprLaTeX (Determinant rows) =
  "\\begin{vmatrix} " ++ intercalate " \\\\ " (map formatRow rows) ++ " \\end{vmatrix}"
  where formatRow row = intercalate " & " (map prettyExprLaTeX row)
prettyExprLaTeX (Sum v lo hi body) =
  "\\sum_{" ++ v ++ "=" ++ prettyExprLaTeX lo ++ "}^{" ++ prettyExprLaTeX hi ++ "} " ++ prettyExprLaTeX body

-- | Helper to format variable names with subscripts for numeric suffixes
formatVarNameLaTeX :: String -> String
formatVarNameLaTeX v =
  let (base, suffix) = span (not . (`elem` ['0'..'9'])) v
  in if null suffix
     then v
     else base ++ "_{" ++ suffix ++ "}"

-- | Format a formula in LaTeX with proper logical connectives.
prettyFormulaLaTeX :: Formula -> String
prettyFormulaLaTeX (Eq l r) = prettyExprLaTeX l ++ " = " ++ prettyExprLaTeX r
prettyFormulaLaTeX (Ge l r) = prettyExprLaTeX l ++ " \\geq " ++ prettyExprLaTeX r
prettyFormulaLaTeX (Gt l r) = prettyExprLaTeX l ++ " > " ++ prettyExprLaTeX r
prettyFormulaLaTeX (Le l r) = prettyExprLaTeX l ++ " \\leq " ++ prettyExprLaTeX r
prettyFormulaLaTeX (Lt l r) = prettyExprLaTeX l ++ " < " ++ prettyExprLaTeX r
prettyFormulaLaTeX (Divides l r) = prettyExprLaTeX l ++ " \\mid " ++ prettyExprLaTeX r
prettyFormulaLaTeX (And f1 f2) = prettyFormulaLaTeX f1 ++ " \\land " ++ prettyFormulaLaTeX f2
prettyFormulaLaTeX (Or f1 f2) = prettyFormulaLaTeX f1 ++ " \\lor " ++ prettyFormulaLaTeX f2
prettyFormulaLaTeX (Not f) = "\\neg (" ++ prettyFormulaLaTeX f ++ ")"
prettyFormulaLaTeX (Forall qs f) =
  "\\forall " ++ intercalate ", " (map prettyQuantVarLaTeX qs) ++ ": " ++ prettyFormulaLaTeX f
prettyFormulaLaTeX (Exists qs f) =
  "\\exists " ++ intercalate ", " (map prettyQuantVarLaTeX qs) ++ ": " ++ prettyFormulaLaTeX f

-- | Format a quantified variable in LaTeX
prettyQuantVarLaTeX :: QuantVar -> String
prettyQuantVarLaTeX (QuantVar v QuantReal lo hi) = formatVarNameLaTeX v ++ prettyBoundsLaTeX lo hi
prettyQuantVarLaTeX (QuantVar v QuantInt lo hi)  = formatVarNameLaTeX v ++ " \\in \\mathbb{Z}" ++ prettyBoundsLaTeX lo hi

-- | Format bounds in LaTeX
prettyBoundsLaTeX :: Maybe Expr -> Maybe Expr -> String
prettyBoundsLaTeX Nothing Nothing = ""
prettyBoundsLaTeX (Just l) (Just u) = " \\in [" ++ prettyExprLaTeX l ++ ", " ++ prettyExprLaTeX u ++ "]"
prettyBoundsLaTeX (Just l) Nothing  = " \\geq " ++ prettyExprLaTeX l
prettyBoundsLaTeX Nothing (Just u)  = " \\leq " ++ prettyExprLaTeX u

-- =============================================
-- Polynomial Engine
-- =============================================

-- | A monomial is a product of variables raised to natural number powers.
--
-- Represented as a map from variable names to their exponents.
-- The monomial @x^2 * y^3@ is @Monomial (Map.fromList [(\"x\", 2), (\"y\", 3)])@.
--
-- The constant monomial 1 is represented as @Monomial Map.empty@.
newtype Monomial = Monomial (M.Map String Natural) deriving (Eq, Show)

-- | Lexicographic term order for monomials.
--
-- Variables are compared in descending alphabetical order (z > y > x > ... > a).
-- This ordering is used for Groebner basis computations and polynomial normalization.
--
-- ==== Examples
--
-- >>> compare (Monomial (Map.singleton "x" 2)) (Monomial (Map.singleton "y" 1))
-- LT  -- because y > x in the variable ordering
instance Ord Monomial where
  compare (Monomial m1) (Monomial m2) =
    let vars = sortBy (flip compare) (nub (M.keys m1 ++ M.keys m2))
    in go vars
    where
      go [] = EQ
      go (v:vs) =
        let e1 = M.findWithDefault 0 v m1
            e2 = M.findWithDefault 0 v m2
        in case compare e1 e2 of
             EQ -> go vs
             other -> other

-- | A multivariate polynomial with rational coefficients.
--
-- Represented as a map from monomials to their coefficients.
-- Zero coefficients are automatically removed to maintain canonical form.
--
-- ==== Examples
--
-- The polynomial @3x^2 + 2xy - 5@ would be:
--
-- @
-- Poly (Map.fromList
--   [ (Monomial (Map.singleton \"x\" 2), 3)
--   , (Monomial (Map.fromList [(\"x\", 1), (\"y\", 1)]), 2)
--   , (Monomial Map.empty, -5)
--   ])
-- @
newtype Poly = Poly (M.Map Monomial Rational) deriving (Eq, Ord, Show)

monomialOne :: Monomial
monomialOne = Monomial M.empty

monomialMul :: Monomial -> Monomial -> Monomial
monomialMul (Monomial m1) (Monomial m2) = Monomial (M.unionWith (+) m1 m2)

-- Monomial Division
monomialDiv :: Monomial -> Monomial -> Maybe Monomial
monomialDiv (Monomial m1) (Monomial m2)
  | M.isSubmapOfBy (<=) m2 m1 = Just $ Monomial (M.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just (v1 - v2)) m1 m2)
  | otherwise = Nothing

monomialLCM :: Monomial -> Monomial -> Monomial
monomialLCM (Monomial m1) (Monomial m2) = Monomial (M.unionWith max m1 m2)

polyZero :: Poly
polyZero = Poly M.empty

polyFromConst :: Rational -> Poly
polyFromConst r | r == 0 = polyZero | otherwise = Poly (M.singleton monomialOne r)

polyFromVar :: String -> Poly
polyFromVar x = Poly (M.singleton (Monomial (M.singleton x 1)) 1)

polyAdd :: Poly -> Poly -> Poly
polyAdd (Poly p1) (Poly p2) = Poly (M.filter (/= 0) (M.unionWith (+) p1 p2))

polyNeg :: Poly -> Poly
polyNeg (Poly p) = Poly (M.map negate p)

polySub :: Poly -> Poly -> Poly
polySub p1 p2 = polyAdd p1 (polyNeg p2)

polyMul :: Poly -> Poly -> Poly
polyMul (Poly p1) (Poly p2) =
  Poly . M.filter (/= 0) $ M.fromListWith (+)
    [ (monomialMul m1 m2, c1 * c2) | (m1, c1) <- M.toList p1, (m2, c2) <- M.toList p2 ]

polyPow :: Poly -> Natural -> Poly
polyPow _ 0 = polyFromConst 1
polyPow p 1 = p
polyPow p n | even n = let h = polyPow p (n `div` 2) in polyMul h h
            | otherwise = polyMul p (polyPow p (n - 1))

getLeadingTerm :: Poly -> Maybe (Monomial, Rational)
getLeadingTerm (Poly m)
  | M.null m = Nothing
  | otherwise = M.lookupMax m

-- | Get leading term using a custom monomial ordering
-- This allows GrevLex and other orderings to be used instead of the default Lex
getLeadingTermByOrder :: (Monomial -> Monomial -> Ordering) -> Poly -> Maybe (Monomial, Rational)
getLeadingTermByOrder cmp (Poly m)
  | M.null m = Nothing
  | otherwise = Just $ maximumBy (\(m1, _) (m2, _) -> cmp m1 m2) (M.toList m)

prettyMonomial :: Monomial -> String
prettyMonomial (Monomial m)
  | M.null m = ""
  | otherwise = intercalate "*" [ if e==1 then v else v++"^"++show e | (v,e)<-M.toList m ]

prettyPoly :: Poly -> String
prettyPoly (Poly m)
  | M.null m = "0"
  | otherwise =
      let terms = [ (c, mono) | (mono, c) <- M.toDescList m, c /= 0 ]
          str (c, mo) = (if c < 0 then " - " else " + ") ++
                        (if abs c == 1 && not (M.null (let Monomial x = mo in x)) then "" else prettyRational (abs c)) ++
                        prettyMonomial mo
      in case concatMap str terms of
           (' ':'+':' ':rest) -> rest
           (' ':'-':' ':rest) -> "-" ++ rest
           s -> s

-- Enhanced pretty printing with mathematical notation
prettyPolyNice :: Poly -> String
prettyPolyNice (Poly m)
  | M.null m = "0"
  | otherwise =
      let terms = sortTerms [ (c, mono) | (mono, c) <- M.toList m, c /= 0 ]
      in formatTerms terms
  where
    -- Sort terms by total degree (descending), then lexicographically
    sortTerms = sortBy (\(_, m1) (_, m2) ->
      let deg1 = totalDegree m1
          deg2 = totalDegree m2
      in compare deg2 deg1 <> compare m2 m1)

    totalDegree (Monomial vars) = sum (M.elems vars)

    formatTerms [] = "0"
    formatTerms [(c, mono)] = formatFirstTerm c mono
    formatTerms ((c, mono):rest) = formatFirstTerm c mono ++ concatMap (uncurry formatOtherTerm) rest

    formatFirstTerm c mono
      | M.null (let Monomial m = mono in m) = prettyRational c
      | c == 1 = formatMonomial mono
      | c == -1 = "-" ++ formatMonomial mono
      | otherwise = prettyRational c ++ formatMonomial mono

    formatOtherTerm c mono
      | c < 0 = " - " ++ formatCoeff (abs c) mono
      | otherwise = " + " ++ formatCoeff c mono

    formatCoeff c mono
      | M.null (let Monomial m = mono in m) = prettyRational c
      | c == 1 = formatMonomial mono
      | otherwise = prettyRational c ++ formatMonomial mono

    formatMonomial (Monomial m)
      | M.null m = ""
      | otherwise = concatMap formatVar (M.toAscList m)

    formatVar (v, 1) = v
    formatVar (v, e) = v ++ "^" ++ show e

-- =============================================
-- Expression Simplification
-- =============================================

-- | Substitute a variable with an expression
substituteExpr :: String -> Expr -> Expr -> Expr
substituteExpr var val (Var v) | v == var = val
substituteExpr _ _ (Var v) = Var v
substituteExpr _ _ (Const c) = Const c
substituteExpr var val (IntVar v) | v == var = val
substituteExpr _ _ e@(IntVar _) = e
substituteExpr _ _ e@(IntConst _) = e
substituteExpr var val (Add e1 e2) = Add (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Sub e1 e2) = Sub (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Mul e1 e2) = Mul (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Div e1 e2) = Div (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Mod e1 e2) = Mod (substituteExpr var val e1) (substituteExpr var val e2)
substituteExpr var val (Pow e n) = Pow (substituteExpr var val e) n
substituteExpr var val (Sqrt e) = Sqrt (substituteExpr var val e)
substituteExpr var val (NthRoot n e) = NthRoot n (substituteExpr var val e)
substituteExpr var val (Sin e) = Sin (substituteExpr var val e)
substituteExpr var val (Cos e) = Cos (substituteExpr var val e)
substituteExpr var val (Tan e) = Tan (substituteExpr var val e)
substituteExpr var val (Asin e) = Asin (substituteExpr var val e)
substituteExpr var val (Acos e) = Acos (substituteExpr var val e)
substituteExpr var val (Atan e) = Atan (substituteExpr var val e)
substituteExpr var val (Sum i lo hi body) =
  let lo' = substituteExpr var val lo
      hi' = substituteExpr var val hi
      body' = if i == var then body else substituteExpr var val body
  in Sum i lo' hi' body'
substituteExpr _ _ e = e

-- | Substitute multiple variables
substituteAll :: M.Map String Expr -> Expr -> Expr
substituteAll subMap (Var v) = M.findWithDefault (Var v) v subMap
substituteAll _ (Const c) = Const c
substituteAll subMap (IntVar v) = M.findWithDefault (IntVar v) v subMap
substituteAll _ e@(IntConst _) = e
substituteAll subMap (Add e1 e2) = Add (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Sub e1 e2) = Sub (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Mul e1 e2) = Mul (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Div e1 e2) = Div (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Mod e1 e2) = Mod (substituteAll subMap e1) (substituteAll subMap e2)
substituteAll subMap (Pow e n) = Pow (substituteAll subMap e) n
substituteAll subMap (Sqrt e) = Sqrt (substituteAll subMap e)
substituteAll subMap (NthRoot n e) = NthRoot n (substituteAll subMap e)
substituteAll subMap (Sin e) = Sin (substituteAll subMap e)
substituteAll subMap (Cos e) = Cos (substituteAll subMap e)
substituteAll subMap (Tan e) = Tan (substituteAll subMap e)
substituteAll subMap (Asin e) = Asin (substituteAll subMap e)
substituteAll subMap (Acos e) = Acos (substituteAll subMap e)
substituteAll subMap (Atan e) = Atan (substituteAll subMap e)
substituteAll subMap (Sum i lo hi body) =
  let lo' = substituteAll subMap lo
      hi' = substituteAll subMap hi
      subMap' = M.delete i subMap
      body' = substituteAll subMap' body
  in Sum i lo' hi' body'
substituteAll _ e = e

-- | Check if two expressions are symbolically equal
-- Tries to convert to polynomials for robust equality checking
-- Falls back to structural equality (after simplification) if conversion fails (e.g. division)
exprEqualsSymbolic :: Expr -> Expr -> Bool
exprEqualsSymbolic e1 e2 = 
  let s1 = normalizeCommAssoc (simplifyExpr e1)
      s2 = normalizeCommAssoc (simplifyExpr e2)
  in if s1 == s2 then True -- Fast path: identical (after normalization)
     else 
       -- Try polynomial equivalence
       -- Catch errors from toPoly (like Division)
       if hasNonPolynomial s1 || hasNonPolynomial s2 
       then False -- Cannot prove equality with Division easily yet
       else toPoly s1 == toPoly s2

hasNonPolynomial :: Expr -> Bool
hasNonPolynomial (Div _ _) = True
hasNonPolynomial (Mod _ _) = True
hasNonPolynomial (Sqrt _) = True
hasNonPolynomial (NthRoot _ _) = True
hasNonPolynomial (Sin _) = True
hasNonPolynomial (Cos _) = True
hasNonPolynomial (Tan _) = True
hasNonPolynomial (Asin _) = True
hasNonPolynomial (Acos _) = True
hasNonPolynomial (Atan _) = True
hasNonPolynomial (IntVar _) = True
hasNonPolynomial (IntConst _) = True
hasNonPolynomial (Add e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Sub e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Mul e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Pow e _) = hasNonPolynomial e
hasNonPolynomial (Sum _ _ _ _) = True
hasNonPolynomial (Dist2 _ _) = False
hasNonPolynomial (Collinear _ _ _) = False
hasNonPolynomial (Dot _ _ _ _) = False
hasNonPolynomial (Circle _ _ r) = hasNonPolynomial r
hasNonPolynomial (Midpoint _ _ _) = False
hasNonPolynomial (Perpendicular _ _ _ _) = False
hasNonPolynomial (Parallel _ _ _ _) = False
hasNonPolynomial (AngleEq2D _ _ _ _ _ _) = False
hasNonPolynomial (AngleEq2DAbs _ _ _ _ _ _) = False
hasNonPolynomial (Determinant rows) = any hasNonPolynomial (concat rows)
hasNonPolynomial _ = False

-- Simplify an Expr by applying algebraic rules
simplifyExpr :: Expr -> Expr
simplifyExpr (Add e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, e) -> e                    -- 0 + e = e
       (e, Const 0) -> e                    -- e + 0 = e
       (Const r1, Const r2) -> Const (r1 + r2)  -- Fold constants
       _ -> Add s1 s2

simplifyExpr (Sub e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (e, Const 0) -> e                    -- e - 0 = e
       (Const 0, e) -> Mul (Const (-1)) e   -- 0 - e = -e
       (Const r1, Const r2) -> Const (r1 - r2)
       _ | s1 == s2 -> Const 0              -- e - e = 0
       _ -> Sub s1 s2

simplifyExpr (Mul e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       (Const 0, _) -> Const 0              -- 0 * e = 0
       (_, Const 0) -> Const 0              -- e * 0 = 0
       (Const 1, e) -> e                    -- 1 * e = e
       (e, Const 1) -> e                    -- e * 1 = e
       (Const (-1), Const (-1)) -> Const 1  -- -1 * -1 = 1
       (Const r1, Const r2) -> Const (r1 * r2)
       _ -> Mul s1 s2

simplifyExpr (Div e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       -- Check for division by zero FIRST
       (_, Const 0) -> error $ formatError (DivisionByZero
                        "Attempted to simplify division by zero constant")
       (_, IntConst 0) -> error $ formatError (DivisionByZero
                           "Attempted to simplify division by zero integer")
       (Const 0, _) -> Const 0              -- 0 / e = 0 (only if e /= 0, already checked)
       (e, Const 1) -> e                    -- e / 1 = e
       (e, IntConst 1) -> e                 -- e / 1 = e
       (Const r1, Const r2) -> Const (r1 / r2)  -- r2 /= 0 guaranteed by guards
       (e, Const c) -> Mul e (Const (1 / c))     -- c /= 0 guaranteed by guards
       (e, IntConst c) -> Mul e (Const (1 % c))  -- c /= 0 guaranteed by guards
       (Div a b, c) -> simplifyExpr (Div a (Mul b c))  -- (a/b)/c = a/(bc)
       (a, Div b c) -> simplifyExpr (Div (Mul a c) b)  -- a/(b/c) = (ac)/b
       _ | s1 == s2 && s1 /= Const 0 && s1 /= IntConst 0 -> Const 1  -- e / e = 1 if e /= 0
       _ -> Div s1 s2

simplifyExpr (Mod e1 e2) =
  let s1 = simplifyExpr e1
      s2 = simplifyExpr e2
  in case (s1, s2) of
       -- Check for modulo by zero FIRST
       (_, Const 0) -> error $ formatError (DivisionByZero
                        "Attempted modulo by zero constant")
       (_, IntConst 0) -> error $ formatError (DivisionByZero
                           "Attempted modulo by zero integer")
       (_, Const 1) -> Const 0              -- e mod 1 = 0
       (_, IntConst 1) -> Const 0
       (Const r1, Const r2) | denominator r1 == 1 && denominator r2 == 1 ->
           Const (fromIntegral (numerator r1 `mod` numerator r2))  -- r2 /= 0 guaranteed
       (IntConst i1, IntConst i2) -> IntConst (i1 `mod` i2)  -- i2 /= 0 guaranteed
       _ -> Mod s1 s2

simplifyExpr (Pow _ 0) = Const 1            -- e^0 = 1
simplifyExpr (Pow e 1) = simplifyExpr e     -- e^1 = e
simplifyExpr (Pow e n) = 
  let s = simplifyExpr e 
  in case s of
       Const r -> Const (r ^ n)
       _ -> Pow s n

simplifyExpr (Sqrt e) =
  let s = simplifyExpr e
  in case s of
       Const r | r < 0 -> Sqrt s  -- Keep as-is for non-real roots
       Const r ->
         let n = numerator r
             d = denominator r
             rn = integerSqrt n
             rd = integerSqrt d
         in if rn * rn == n && rd * rd == d
            then Const (fromIntegral rn % fromIntegral rd)
            else Sqrt s
       _ -> Sqrt s

-- NthRoot simplification: NthRoot n e represents e^(1/n)
simplifyExpr (NthRoot 0 _) = error "NthRoot: index must be >= 2"
simplifyExpr (NthRoot 1 e) = simplifyExpr e  -- 1st root is identity
simplifyExpr (NthRoot 2 e) = simplifyExpr (Sqrt e)  -- Canonical: use Sqrt for n=2
simplifyExpr (NthRoot n e) =
  let s = simplifyExpr e
  in case s of
       Const 0 -> Const 0  -- 0^(1/n) = 0
       Const 1 -> Const 1  -- 1^(1/n) = 1
       Const r | r < 0 && even n -> NthRoot n s  -- Even root of negative: keep
       Const r ->
         -- Try to simplify perfect nth powers
         let num = numerator r
             den = denominator r
             tryNthRoot x =
               let candidate = round ((fromIntegral (abs x) :: Double) ** (1 / fromIntegral n))
               in if candidate ^ n == abs x
                  then Just (if x < 0 then -candidate else candidate)
                  else Nothing
         in case (tryNthRoot num, tryNthRoot den) of
              (Just rn, Just rd) | rd /= 0 -> Const (rn % rd)
              _ -> NthRoot n s
       -- Simplify NthRoot n (Pow e n) = e when e >= 0 (for even n)
       Pow base m | fromIntegral m == n -> base  -- Assumes positive base
       _ -> NthRoot n s

simplifyExpr (Sin e) =
  let s = simplifyExpr e
  in case s of
       Const 0 -> Const 0
       IntConst 0 -> Const 0
       _ -> Sin s

simplifyExpr (Cos e) =
  let s = simplifyExpr e
  in case s of
       Const 0 -> Const 1
       IntConst 0 -> Const 1
       _ -> Cos s

simplifyExpr (Tan e) =
  let s = simplifyExpr e
  in case s of
       Const 0 -> Const 0
       IntConst 0 -> Const 0
       _ -> Tan s

simplifyExpr (Asin e) = Asin (simplifyExpr e)
simplifyExpr (Acos e) = Acos (simplifyExpr e)
simplifyExpr (Atan e) = Atan (simplifyExpr e)

simplifyExpr (Sum i lo hi body) =
  let lo' = simplifyExpr lo
      hi' = simplifyExpr hi
      body' = simplifyExpr body
      
      asInt (IntConst n) = Just n
      asInt (Const r) | denominator r == 1 = Just (numerator r)
      asInt _ = Nothing
      
  in case (asInt lo', asInt hi') of
       (Just l, Just h) ->
          if l > h then Const 0
          else
            let terms = [ substituteExpr i (IntConst k) body' | k <- [l..h] ]
            in simplifyExpr (foldl Add (Const 0) terms)
       _ -> Sum i lo' hi' body'
simplifyExpr e@(IntVar _) = e
simplifyExpr e@(IntConst _) = e

simplifyExpr (Determinant rows) =
  let 
      -- 1. Simplify all elements
      simpRows = map (map simplifyExpr) rows
      
      -- 2. Check for zero rows or zero columns
      hasZeroRow = any (all (== Const 0)) simpRows
      
      -- Transpose to check columns
      cols = if null simpRows then [] else transpose simpRows
      hasZeroCol = any (all (== Const 0)) cols
      
      -- 3. Check for dependent rows (identical rows)
      hasIdenticalRows = hasDuplicates simpRows
      
  in if hasZeroRow || hasZeroCol || hasIdenticalRows
     then Const 0
     else 
       -- 4. Symbolic Gaussian Elimination (Bareiss Algorithm - Simplified Step)
       -- If we find a row starting with 0, we can't easily pivot without division (which we avoid).
       -- But if we find triangular form, it's just product of diagonals.
       -- For now, just return the simplified matrix structure to allow lazy expansion later.
       -- Full Bareiss is complex to implement on Expr tree without a proper polynomial type here.
       -- We settle for "Zero/Identity Detection" which handles many geometric cases (coplanarity).
       Determinant simpRows

-- Geometric primitives don't simplify further at Expr level
simplifyExpr e@(Dist2 _ _) = e
simplifyExpr e@(Collinear _ _ _) = e
simplifyExpr e@(Dot _ _ _ _) = e
simplifyExpr e@(Circle _ _ _) = e
simplifyExpr e@(Midpoint _ _ _) = e
simplifyExpr e@(Perpendicular _ _ _ _) = e
simplifyExpr e@(Parallel _ _ _ _) = e
simplifyExpr e@(AngleEq2D _ _ _ _ _ _) = e
simplifyExpr e@(AngleEq2DAbs _ _ _ _ _ _) = e

-- Base cases
simplifyExpr e@(Var _) = e
simplifyExpr e@(Const _) = e

-- Helper: Matrix transposition
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose rows
  | any null rows = []
  | otherwise =
      let heads = [h | (h:_) <- rows]
          tails = [t | (_:t) <- rows, not (null t) || null rows]
      in heads : transpose tails

-- Helper: Check for duplicate rows
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

-- =============================================
-- Commutative / Associative Normalization
-- =============================================

-- | Normalize expression trees by flattening and sorting commutative operations
-- This helps symbolic equality catch cases like (a + b) vs (b + a)
normalizeCommAssoc :: Expr -> Expr
normalizeCommAssoc (Sub a b) = normalizeCommAssoc (Add a (Mul (Const (-1)) b))
normalizeCommAssoc (Add a b) =
  let terms = concatMap collectAdd [normalizeCommAssoc a, normalizeCommAssoc b]
      (consts, others) = partitionConsts terms
      constSum = sum consts
      sortedOthers = sortBy (\x y -> compare (prettyExpr x) (prettyExpr y)) others
      rebuilt = case (constSum, sortedOthers) of
                  (0, [])     -> Const 0
                  (c, [])     -> Const c
                  (0, (o:os)) -> foldl Add o os
                  (c, os)     -> foldl Add (Const c) os
  in rebuilt
normalizeCommAssoc (Mul a b) =
  let factors = concatMap collectMul [normalizeCommAssoc a, normalizeCommAssoc b]
      (consts, others) = partitionConsts factors
      constProd = product consts
      sortedOthers = sortBy (\x y -> compare (prettyExpr x) (prettyExpr y)) others
      rebuilt = case (constProd, sortedOthers) of
                  (0, _)      -> Const 0
                  (c, [])     -> Const c
                  (1, (o:os)) -> foldl Mul o os
                  (-1, (o:os))-> foldl Mul (Mul (Const (-1)) o) os
                  (c, (o:os)) -> foldl Mul (Mul (Const c) o) os
  in rebuilt
normalizeCommAssoc (Div a b) = Div (normalizeCommAssoc a) (normalizeCommAssoc b)
normalizeCommAssoc (Mod a b) = Mod (normalizeCommAssoc a) (normalizeCommAssoc b)
normalizeCommAssoc (Pow e n) = Pow (normalizeCommAssoc e) n
normalizeCommAssoc (Sqrt e) = Sqrt (normalizeCommAssoc e)
normalizeCommAssoc (NthRoot n e) = NthRoot n (normalizeCommAssoc e)
normalizeCommAssoc (Sin e) = Sin (normalizeCommAssoc e)
normalizeCommAssoc (Cos e) = Cos (normalizeCommAssoc e)
normalizeCommAssoc (Tan e) = Tan (normalizeCommAssoc e)
normalizeCommAssoc (Asin e) = Asin (normalizeCommAssoc e)
normalizeCommAssoc (Acos e) = Acos (normalizeCommAssoc e)
normalizeCommAssoc (Atan e) = Atan (normalizeCommAssoc e)
normalizeCommAssoc (Determinant rows) = Determinant (map (map normalizeCommAssoc) rows)
normalizeCommAssoc e = e

collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e = [e]

collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e = [e]

partitionConsts :: [Expr] -> ([Rational], [Expr])
partitionConsts = foldr step ([], [])
  where
    step (Const c) (cs, es) = (c:cs, es)
    step e (cs, es)         = (cs, e:es)

-- =============================================
-- Univariate Support (New for Sturm)
-- =============================================

-- Converts a Poly to a list of coefficients [c0, c1, c2...] 
-- Returns Nothing if the Poly has more than one variable.
toUnivariate :: Poly -> Maybe (String, [Rational])
toUnivariate (Poly m) =
  let
      -- Get all variables used across all monomials
      vars = concatMap (\(Monomial vm) -> M.keys vm) (M.keys m)

      -- Get UNIQUE variables (not just the first one!)
      uniqueVars = nub vars

      -- Check if each monomial has at most one variable
      isValid = all (\(Monomial vm) -> length (M.keys vm) <= 1) (M.keys m)

  in if not isValid then Nothing else
     case uniqueVars of
       [] -> Just ("x", [M.findWithDefault 0 monomialOne m]) -- Constant poly
       [v] ->  -- Exactly one unique variable -> truly univariate
         let maxDeg = maximum (0 : map (\(Monomial vm) -> M.findWithDefault 0 v vm) (M.keys m))
             coeffs = [ M.findWithDefault 0 (Monomial (if i==0 then M.empty else M.singleton v (fromIntegral i))) m
                      | i <- [0..maxDeg] ]
         in Just (v, coeffs)
       _ -> Nothing  -- Multiple variables -> not univariate

-- Convert back
fromUnivariate :: String -> [Rational] -> Poly
fromUnivariate v coeffs = 
  foldl polyAdd polyZero
    [ polyMul (polyFromConst c) (polyPow (polyFromVar v) (fromIntegral i)) 
    | (i, c) <- zip [0 :: Integer ..] coeffs, c /= 0 ]

-- =============================================
-- Conversion: Expr -> Poly
-- =============================================

toPoly :: Expr -> Poly
toPoly (Var x)     = polyFromVar x
toPoly (Const r)   = polyFromConst r
toPoly (IntVar x)  = polyFromVar x
toPoly (IntConst i) = polyFromConst (fromIntegral i)
toPoly (Add e1 e2) = polyAdd (toPoly e1) (toPoly e2)
toPoly (Sub e1 e2) = polySub (toPoly e1) (toPoly e2)
toPoly (Mul e1 e2) = polyMul (toPoly e1) (toPoly e2)
toPoly (Div e1 e2)   =
  let simp = simplifyExpr (Div e1 e2)
  in case simp of
       Div _ _ -> error ("Division Error: Division is not supported in polynomial expressions.\nNote: Rational constants like 1/2 are supported, but division of variables is not.\nContext: Attempting to convert Div expression to polynomial: " ++ prettyExpr (Div e1 e2))
       _ -> toPoly simp
toPoly (Mod e1 e2) =
  let simp = simplifyExpr (Mod e1 e2)
  in case simp of
       Mod _ _ -> error "Modulo Error: Modulo is not supported in polynomial expressions unless simplifiable to constant."
       _ -> toPoly simp
toPoly (Pow e n)   = polyPow (toPoly e) n
toPoly (Sqrt _)    = error "Sqrt Error: Square roots are not supported in polynomial expressions. Rewrite without sqrt or use a geometric/analytic solver."
toPoly (NthRoot _ _) = error "NthRoot Error: Nth roots are not supported in polynomial expressions. Use eliminateSqrt or a geometric solver."
toPoly (Sin _) = error "Sin Error: Sine functions are not supported in polynomial expressions. Use trigonometric substitutions or heuristics."
toPoly (Cos _) = error "Cos Error: Cosine functions are not supported in polynomial expressions. Use trigonometric substitutions or heuristics."
toPoly (Tan _) = error "Tan Error: Tangent functions are not supported in polynomial expressions. Use trigonometric substitutions or heuristics."
toPoly (Asin _) = error "Asin Error: Arcsine functions are not supported in polynomial expressions."
toPoly (Acos _) = error "Acos Error: Arccosine functions are not supported in polynomial expressions."
toPoly (Atan _) = error "Atan Error: Arctangent functions are not supported in polynomial expressions."

toPoly (Sum i lo hi body) =
  let lo' = case simplifyExpr lo of { Const r -> numerator r; _ -> error "Sum low bound must be constant" }
      hi' = case simplifyExpr hi of { Const r -> numerator r; _ -> error "Sum high bound must be constant" }
  in foldl polyAdd polyZero [ toPoly (substituteExpr i (Const (fromInteger j % 1)) body) | j <- [lo'..hi'] ]

toPoly (Dist2 p1 p2) =
  let x1 = polyFromVar ("x" ++ p1); y1 = polyFromVar ("y" ++ p1); z1 = polyFromVar ("z" ++ p1)
      x2 = polyFromVar ("x" ++ p2); y2 = polyFromVar ("y" ++ p2); z2 = polyFromVar ("z" ++ p2)
      dx = polySub x1 x2; dy = polySub y1 y2; dz = polySub z1 z2
  in polyAdd (polyAdd (polyMul dx dx) (polyMul dy dy)) (polyMul dz dz)

toPoly (Dot a b c d) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a); za = polyFromVar ("z" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b); zb = polyFromVar ("z" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c); zc = polyFromVar ("z" ++ c)
      xd = polyFromVar ("x" ++ d); yd = polyFromVar ("y" ++ d); zd = polyFromVar ("z" ++ d)
      vABx = polySub xb xa; vABy = polySub yb ya; vABz = polySub zb za
      vCDx = polySub xd xc; vCDy = polySub yd yc; vCDz = polySub zd zc
  in polyAdd (polyAdd (polyMul vABx vCDx) (polyMul vABy vCDy)) (polyMul vABz vCDz)

toPoly (Collinear a b c) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c)
      vABx = polySub xb xa; vABy = polySub yb ya
      vACx = polySub xc xa; vACy = polySub yc ya
  in polySub (polyMul vABx vACy) (polyMul vABy vACx)

toPoly (Circle p c r) =
  let distSq = toPoly (Dist2 p c)
      rad    = toPoly r
  in polySub distSq (polyPow rad 2)

-- Midpoint: M is midpoint of AB means 2*xM = xA + xB, 2*yM = yA + yB, 2*zM = zA + zB
-- We return the constraint for x-coordinate: 2*xM - xA - xB = 0
-- User should add similar constraints for y and z coordinates
toPoly (Midpoint a b m) =
  let xA = polyFromVar ("x" ++ a); xB = polyFromVar ("x" ++ b); xM = polyFromVar ("x" ++ m)
      yA = polyFromVar ("y" ++ a); yB = polyFromVar ("y" ++ b); yM = polyFromVar ("y" ++ m)
      zA = polyFromVar ("z" ++ a); zB = polyFromVar ("z" ++ b); zM = polyFromVar ("z" ++ m)
      -- Combined constraint: (2xM - xA - xB)² + (2yM - yA - yB)² + (2zM - zA - zB)² = 0
      xConstraint = polySub (polySub (polyMul (polyFromConst 2) xM) xA) xB
      yConstraint = polySub (polySub (polyMul (polyFromConst 2) yM) yA) yB
      zConstraint = polySub (polySub (polyMul (polyFromConst 2) zM) zA) zB
  in polyAdd (polyAdd (polyMul xConstraint xConstraint) (polyMul yConstraint yConstraint)) (polyMul zConstraint zConstraint)

-- Perpendicular: AB ⊥ CD means AB · CD = 0
toPoly (Perpendicular a b c d) = toPoly (Dot a b c d)

-- Parallel: AB ∥ CD means AB × CD = 0 (cross product = 0)
-- In 3D: (AB × CD)_x² + (AB × CD)_y² + (AB × CD)_z² = 0
-- Cross product: u × v = (u_y*v_z - u_z*v_y, u_z*v_x - u_x*v_z, u_x*v_y - u_y*v_x)
toPoly (Parallel a b c d) =
  let xa = polyFromVar ("x" ++ a); ya = polyFromVar ("y" ++ a); za = polyFromVar ("z" ++ a)
      xb = polyFromVar ("x" ++ b); yb = polyFromVar ("y" ++ b); zb = polyFromVar ("z" ++ b)
      xc = polyFromVar ("x" ++ c); yc = polyFromVar ("y" ++ c); zc = polyFromVar ("z" ++ c)
      xd = polyFromVar ("x" ++ d); yd = polyFromVar ("y" ++ d); zd = polyFromVar ("z" ++ d)
      -- Vector AB
      vABx = polySub xb xa; vABy = polySub yb ya; vABz = polySub zb za
      -- Vector CD
      vCDx = polySub xd xc; vCDy = polySub yd yc; vCDz = polySub zd zc
      -- Cross product components
      crossX = polySub (polyMul vABy vCDz) (polyMul vABz vCDy)
      crossY = polySub (polyMul vABz vCDx) (polyMul vABx vCDz)
      crossZ = polySub (polyMul vABx vCDy) (polyMul vABy vCDx)
  in polyAdd (polyAdd (polyMul crossX crossX) (polyMul crossY crossY)) (polyMul crossZ crossZ)

-- Angle equality in 2D (oriented): ∠ABC = ∠DEF
-- Uses scaled sine and cosine comparisons to avoid divisions/square roots.
-- angleEqPoly = (cosDiff)^2 + (sinDiff)^2 where
--   cosDiff = (dot1 * l2) - (dot2 * l1)
--   sinDiff = (cross1 * l2) - (cross2 * l1)
--   l1 = |BA|^2 * |BC|^2 ; l2 = |DE|^2 * |DF|^2
toPoly (AngleEq2D a b c d e f) =
  let vx p q axis = polySub (polyFromVar (axis ++ p)) (polyFromVar (axis ++ q))
      u1x = vx b a "x"; u1y = vx b a "y"
      v1x = vx c b "x"; v1y = vx c b "y"
      u2x = vx e d "x"; u2y = vx e d "y"
      v2x = vx f e "x"; v2y = vx f e "y"
      dot1 = polyAdd (polyMul u1x v1x) (polyMul u1y v1y)
      dot2 = polyAdd (polyMul u2x v2x) (polyMul u2y v2y)
      u1norm = polyAdd (polyMul u1x u1x) (polyMul u1y u1y)
      v1norm = polyAdd (polyMul v1x v1x) (polyMul v1y v1y)
      u2norm = polyAdd (polyMul u2x u2x) (polyMul u2y u2y)
      v2norm = polyAdd (polyMul v2x v2x) (polyMul v2y v2y)
      l1 = polyMul u1norm v1norm
      l2 = polyMul u2norm v2norm
      cross1 = polySub (polyMul u1x v1y) (polyMul u1y v1x)
      cross2 = polySub (polyMul u2x v2y) (polyMul u2y v2x)
      cosDiff = polySub (polyMul dot1 l2) (polyMul dot2 l1)
      sinDiff = polySub (polyMul cross1 l2) (polyMul cross2 l1)
  in polyAdd (polyMul cosDiff cosDiff) (polyMul sinDiff sinDiff)

-- Angle equality in 2D up to reflection (orientation-insensitive)
-- Uses cosDiff as above and absolute sine via squared cross products:
--   sinAbsDiff = (cross1^2 * l2^2) - (cross2^2 * l1^2)
toPoly (AngleEq2DAbs a b c d e f) =
  let vx p q axis = polySub (polyFromVar (axis ++ p)) (polyFromVar (axis ++ q))

      u1x = vx b a "x"; u1y = vx b a "y"
      v1x = vx c b "x"; v1y = vx c b "y"

      u2x = vx e d "x"; u2y = vx e d "y"
      v2x = vx f e "x"; v2y = vx f e "y"

      dot1 = polyAdd (polyMul u1x v1x) (polyMul u1y v1y)
      dot2 = polyAdd (polyMul u2x v2x) (polyMul u2y v2y)

      u1norm = polyAdd (polyMul u1x u1x) (polyMul u1y u1y)
      v1norm = polyAdd (polyMul v1x v1x) (polyMul v1y v1y)
      u2norm = polyAdd (polyMul u2x u2x) (polyMul u2y u2y)
      v2norm = polyAdd (polyMul v2x v2x) (polyMul v2y v2y)

      l1 = polyMul u1norm v1norm
      l2 = polyMul u2norm v2norm

      cross1 = polySub (polyMul u1x v1y) (polyMul u1y v1x)
      cross2 = polySub (polyMul u2x v2y) (polyMul u2y v2x)

      cosDiff = polySub (polyMul dot1 l2) (polyMul dot2 l1)
      sinAbsDiff = polySub (polyMul (polyMul cross1 cross1) (polyMul l2 l2))
                            (polyMul (polyMul cross2 cross2) (polyMul l1 l1))
  in polyAdd (polyMul cosDiff cosDiff) (polyMul sinAbsDiff sinAbsDiff)

-- Determinant: Recursive expansion (Laplace expansion along first row)
-- For a matrix M, det(M) = sum_{j=1..n} (-1)^(1+j) * M_{1,j} * det(M_{1,j})
-- Base case: 1x1 matrix [a] -> a
toPoly (Determinant rows) = detPoly rows
  where
    detPoly [] = polyZero
    detPoly [[x]] = toPoly x
    detPoly (firstRow:restRows) = 
        let n = length firstRow
            terms = [ let element = firstRow !! colIndex
                          subMatrix = [ removeAt colIndex row | row <- restRows ]
                          term = polyMul (toPoly element) (detPoly subMatrix)
                      in if even colIndex then term else polyNeg term
                    | colIndex <- [0..n-1] ]
        in foldl polyAdd polyZero terms

    removeAt i xs = take i xs ++ drop (i+1) xs

-- =============================================
-- Non-polynomial detection helpers
-- =============================================

-- | Check if expression contains any root (Sqrt or NthRoot)
containsSqrtExpr :: Expr -> Bool
containsSqrtExpr (Sqrt _) = True
containsSqrtExpr (NthRoot _ _) = True
containsSqrtExpr (Add a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Sub a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Mul a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Div a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Mod a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Pow e _) = containsSqrtExpr e
containsSqrtExpr (Sin e) = containsSqrtExpr e
containsSqrtExpr (Cos e) = containsSqrtExpr e
containsSqrtExpr (Tan e) = containsSqrtExpr e
containsSqrtExpr (Asin e) = containsSqrtExpr e
containsSqrtExpr (Acos e) = containsSqrtExpr e
containsSqrtExpr (Atan e) = containsSqrtExpr e
containsSqrtExpr (Determinant rows) = any containsSqrtExpr (concat rows)
containsSqrtExpr (Circle _ _ e) = containsSqrtExpr e
containsSqrtExpr _ = False

-- =============================================
-- Integer detection helpers
-- =============================================

containsIntExpr :: Expr -> Bool
containsIntExpr (IntVar _) = True
containsIntExpr (IntConst _) = True
containsIntExpr (Add a b) = containsIntExpr a || containsIntExpr b
containsIntExpr (Sub a b) = containsIntExpr a || containsIntExpr b
containsIntExpr (Mul a b) = containsIntExpr a || containsIntExpr b
containsIntExpr (Div a b) = containsIntExpr a || containsIntExpr b
containsIntExpr (Mod a b) = containsIntExpr a || containsIntExpr b
containsIntExpr (Pow e _) = containsIntExpr e
containsIntExpr (Sqrt e) = containsIntExpr e
containsIntExpr (NthRoot _ e) = containsIntExpr e
containsIntExpr (Sin e) = containsIntExpr e
containsIntExpr (Cos e) = containsIntExpr e
containsIntExpr (Tan e) = containsIntExpr e
containsIntExpr (Asin e) = containsIntExpr e
containsIntExpr (Acos e) = containsIntExpr e
containsIntExpr (Atan e) = containsIntExpr e
containsIntExpr (Determinant rows) = any containsIntExpr (concat rows)
containsIntExpr (Circle _ _ e) = containsIntExpr e
containsIntExpr _ = False

-- Utility: substitute all IntVar occurrences in a Formula using a map
substituteInts :: M.Map String Expr -> Formula -> Formula
substituteInts sub (Eq l r) = Eq (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (Ge l r) = Ge (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (Gt l r) = Gt (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (Le l r) = Le (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (Lt l r) = Lt (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (Divides l r) = Divides (substituteIntsExpr sub l) (substituteIntsExpr sub r)
substituteInts sub (And f1 f2) = And (substituteInts sub f1) (substituteInts sub f2)
substituteInts sub (Or f1 f2) = Or (substituteInts sub f1) (substituteInts sub f2)
substituteInts sub (Not f) = Not (substituteInts sub f)
substituteInts sub (Forall qs f) =
  let sub' = foldr M.delete sub (map qvName qs)
  in Forall qs (substituteInts sub' f)
substituteInts sub (Exists qs f) =
  let sub' = foldr M.delete sub (map qvName qs)
  in Exists qs (substituteInts sub' f)

substituteIntsExpr :: M.Map String Expr -> Expr -> Expr
substituteIntsExpr sub (IntVar v) = M.findWithDefault (IntVar v) v sub
substituteIntsExpr sub (Var v) = M.findWithDefault (Var v) v sub
substituteIntsExpr _ e@(IntConst _) = e
substituteIntsExpr _ e@(Const _) = e
substituteIntsExpr sub (Add a b) = Add (substituteIntsExpr sub a) (substituteIntsExpr sub b)
substituteIntsExpr sub (Sub a b) = Sub (substituteIntsExpr sub a) (substituteIntsExpr sub b)
substituteIntsExpr sub (Mul a b) = Mul (substituteIntsExpr sub a) (substituteIntsExpr sub b)
substituteIntsExpr sub (Div a b) = Div (substituteIntsExpr sub a) (substituteIntsExpr sub b)
substituteIntsExpr sub (Mod a b) = Mod (substituteIntsExpr sub a) (substituteIntsExpr sub b)
substituteIntsExpr sub (Pow e n) = Pow (substituteIntsExpr sub e) n
substituteIntsExpr sub (Sqrt e) = Sqrt (substituteIntsExpr sub e)
substituteIntsExpr sub (NthRoot n e) = NthRoot n (substituteIntsExpr sub e)
substituteIntsExpr sub (Sin e) = Sin (substituteIntsExpr sub e)
substituteIntsExpr sub (Cos e) = Cos (substituteIntsExpr sub e)
substituteIntsExpr sub (Tan e) = Tan (substituteIntsExpr sub e)
substituteIntsExpr sub (Asin e) = Asin (substituteIntsExpr sub e)
substituteIntsExpr sub (Acos e) = Acos (substituteIntsExpr sub e)
substituteIntsExpr sub (Atan e) = Atan (substituteIntsExpr sub e)
substituteIntsExpr _ e = e

containsSqrtFormula :: Formula -> Bool
containsSqrtFormula (Eq l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Ge l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Gt l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Le l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Lt l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Divides l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (And f1 f2) = containsSqrtFormula f1 || containsSqrtFormula f2
containsSqrtFormula (Or f1 f2) = containsSqrtFormula f1 || containsSqrtFormula f2
containsSqrtFormula (Not f) = containsSqrtFormula f
containsSqrtFormula (Forall _ f) = containsSqrtFormula f
containsSqrtFormula (Exists _ f) = containsSqrtFormula f

-- | Check if expression contains division (for rational elimination)
containsDivExpr :: Expr -> Bool
containsDivExpr (Div _ _) = True
containsDivExpr (Mod _ _) = True
containsDivExpr (Add a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Sub a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Mul a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Pow e _) = containsDivExpr e
containsDivExpr (Sqrt e) = containsDivExpr e
containsDivExpr (NthRoot _ e) = containsDivExpr e
containsDivExpr (Sin e) = containsDivExpr e
containsDivExpr (Cos e) = containsDivExpr e
containsDivExpr (Tan e) = containsDivExpr e
containsDivExpr (Asin e) = containsDivExpr e
containsDivExpr (Acos e) = containsDivExpr e
containsDivExpr (Atan e) = containsDivExpr e
containsDivExpr (Determinant rows) = any containsDivExpr (concat rows)
containsDivExpr (Circle _ _ e) = containsDivExpr e
containsDivExpr _ = False

containsDivFormula :: Formula -> Bool
containsDivFormula (Eq l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Ge l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Gt l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Le l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Lt l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Divides l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (And f1 f2) = containsDivFormula f1 || containsDivFormula f2
containsDivFormula (Or f1 f2) = containsDivFormula f1 || containsDivFormula f2
containsDivFormula (Not f) = containsDivFormula f
containsDivFormula (Forall _ f) = containsDivFormula f
containsDivFormula (Exists _ f) = containsDivFormula f

-- =============================================
-- Integer square root helper
-- =============================================

-- | Integer square root (floor) using pure Integer arithmetic (Newton's method).
-- Safe for massive integers where Double conversion would fail.
integerSqrt :: Integer -> Integer
integerSqrt n
  | n < 0 = 0
  | n == 0 = 0
  | otherwise = go (n `div` 2 + 1)
  where
    go x
      | y < x     = go y
      | otherwise = x
      where y = (x + n `div` x) `div` 2

-- =============================================
-- Logic helpers
-- =============================================

type Theory = [Formula]

containsQuantifier :: Formula -> Bool
containsQuantifier (Forall _ _) = True
containsQuantifier (Exists _ _) = True
containsQuantifier (Divides _ _) = False
containsQuantifier _ = False

containsIntFormula :: Formula -> Bool
containsIntFormula (Eq l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Ge l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Gt l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Le l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Lt l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Divides l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (And f1 f2) = containsIntFormula f1 || containsIntFormula f2
containsIntFormula (Or f1 f2) = containsIntFormula f1 || containsIntFormula f2
containsIntFormula (Not f) = containsIntFormula f
containsIntFormula (Forall qs f) =
  any (\q -> qvType q == QuantInt) qs || containsIntFormula f
containsIntFormula (Exists qs f) =
  any (\q -> qvType q == QuantInt) qs || containsIntFormula f

-- =============================================
-- Polynomial Evaluation & Substitution
-- =============================================

buildSubMap :: Theory -> M.Map String Poly
buildSubMap theory = M.fromList $ concat 
  [ case lhs of
      Var v -> if hasNonPolynomial rhs then [] else [(v, toPoly rhs)]
      IntVar v -> if hasNonPolynomial rhs then [] else [(v, toPoly rhs)]
      _ -> []
  | Eq lhs rhs <- theory 
  ]

evaluatePoly :: M.Map String Poly -> Poly -> Poly
evaluatePoly subM (Poly m) =
  let
      evalMono :: Monomial -> Rational -> Poly
      evalMono (Monomial vars) coeff =
        let
          termExpanded = M.foldlWithKey (\accPoly varname power ->
              let basePoly = case M.lookup varname subM of
                               Just p  -> evaluatePoly subM p
                               Nothing -> polyFromVar varname
              in polyMul accPoly (polyPow basePoly power)
            ) (polyFromConst 1) vars
        in polyMul (polyFromConst coeff) termExpanded
      results = map (\(mono, coeff) -> evalMono mono coeff) (M.toList m)
  in foldl polyAdd polyZero results

toPolySub :: M.Map String Poly -> Expr -> Poly
toPolySub subM expr = evaluatePoly subM (toPoly expr)

-- | Convert Poly back to Expr
polyToExpr :: Poly -> Expr
polyToExpr (Poly m)
  | M.null m = Const 0
  | otherwise =
      let termExpr (Monomial vars, c) =
            let base = foldl (\acc (v, e) -> Mul acc (Pow (Var v) (fromIntegral e))) (Const 1) (M.toList vars)
            in Mul (Const c) base
          terms = map termExpr (M.toList m)
      in foldl1 Add terms

-- =============================================
-- Shared Polynomial Utilities
-- =============================================

isConstPoly :: Poly -> Bool
isConstPoly p = p == polyZero || S.null (getVars p)

getVars :: Poly -> S.Set String
getVars (Poly m) =
  S.fromList [ v | (Monomial mono, _) <- M.toList m, (v, _) <- M.toList mono ]

monomialGCD :: Monomial -> Monomial -> Monomial
monomialGCD (Monomial m1) (Monomial m2) =
  Monomial $ M.intersectionWith min m1 m2

getMainVar :: Poly -> String
getMainVar p =
  let vars = S.toList (getVars p)
  in if null vars then "" else maximum vars

-- | Partial derivative with respect to a variable
polyDerivative :: Poly -> String -> Poly
polyDerivative (Poly m) var =
  Poly $ M.fromListWith (+)
    [ (Monomial (M.update (\e -> if e <= 1 then Nothing else Just (e-1)) var vars), c * fromIntegral e)
    | (Monomial vars, c) <- M.toList m
    , let e = M.findWithDefault 0 var vars
    , e > 0
    ]

-- | GCD of two rationals
rationalGCD :: Rational -> Rational -> Rational
rationalGCD a b
  | b == 0 = abs a
  | otherwise =
      let na = numerator a
          da = denominator a
          nb = numerator b
          db = denominator b
          -- GCD of rationals: gcd(a/b, c/d) = gcd(a*d, c*b) / (b*d)
          numGcd = gcd (na * db) (nb * da)
          denLcm = lcm da db
      in abs (fromInteger numGcd / fromInteger denLcm)

-- | Content of a polynomial (GCD of all coefficients)
-- Returns 1 for zero polynomial to avoid division by zero
polyContent :: Poly -> Rational
polyContent (Poly m)
  | M.null m = 1
  | otherwise =
      let coeffs = M.elems m
      in foldr1 rationalGCD (map abs coeffs)

-- | Primitive part of a polynomial (polynomial divided by its content)
-- This keeps coefficients small during algebraic manipulations
polyPrimitive :: Poly -> Poly
polyPrimitive p@(Poly m)
  | M.null m = p
  | otherwise =
      let c = polyContent p
      in if c == 0 || c == 1
         then p
         else Poly (M.map (/ c) m)

-- | Scale a polynomial by a rational constant
polyScale :: Rational -> Poly -> Poly
polyScale 0 _ = polyZero
polyScale 1 p = p
polyScale s (Poly m) = Poly (M.map (* s) m)

-- | Generic expression traversal (bottom-up)
mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f expr = f $ case expr of
  Add a b -> Add (mapExpr f a) (mapExpr f b)
  Sub a b -> Sub (mapExpr f a) (mapExpr f b)
  Mul a b -> Mul (mapExpr f a) (mapExpr f b)
  Div a b -> Div (mapExpr f a) (mapExpr f b)
  Mod a b -> Mod (mapExpr f a) (mapExpr f b)
  Pow a n -> Pow (mapExpr f a) n
  Sqrt a  -> Sqrt (mapExpr f a)
  NthRoot n a -> NthRoot n (mapExpr f a)
  Sin a   -> Sin (mapExpr f a)
  Cos a   -> Cos (mapExpr f a)
  Tan a   -> Tan (mapExpr f a)
  Asin a  -> Asin (mapExpr f a)
  Acos a  -> Acos (mapExpr f a)
  Atan a  -> Atan (mapExpr f a)
  Sum i l h b -> Sum i (mapExpr f l) (mapExpr f h) (mapExpr f b)
  Determinant m -> Determinant (map (map (mapExpr f)) m)
  Circle p c r -> Circle p c (mapExpr f r)
  _ -> expr

-- =============================================
-- Variable Extraction (Centralized)
-- =============================================

varsInExpr :: Expr -> [String]
varsInExpr (Var v) = [v]
varsInExpr (Add e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Sub e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Mul e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Div e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Mod e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Pow e _) = varsInExpr e
varsInExpr (Sqrt e) = varsInExpr e
varsInExpr (NthRoot _ e) = varsInExpr e
varsInExpr (Sin e) = varsInExpr e
varsInExpr (Cos e) = varsInExpr e
varsInExpr (Tan e) = varsInExpr e
varsInExpr (Asin e) = varsInExpr e
varsInExpr (Acos e) = varsInExpr e
varsInExpr (Atan e) = varsInExpr e
varsInExpr (Dist2 p1 p2) = ["x"++p1, "y"++p1, "z"++p1, "x"++p2, "y"++p2, "z"++p2]
varsInExpr (Collinear p1 p2 p3) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3]
varsInExpr (Dot p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (Circle _ _ r) = varsInExpr r
varsInExpr (Midpoint p1 p2 p3) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3]
varsInExpr (Perpendicular p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (Parallel p1 p2 p3 p4) = concatMap (\p -> ["x"++p, "y"++p, "z"++p]) [p1, p2, p3, p4]
varsInExpr (AngleEq2D p1 p2 p3 p4 p5 p6) = concatMap (\p -> ["x"++p, "y"++p]) [p1, p2, p3, p4, p5, p6]
varsInExpr (AngleEq2DAbs p1 p2 p3 p4 p5 p6) = concatMap (\p -> ["x"++p, "y"++p]) [p1, p2, p3, p4, p5, p6]
varsInExpr (Determinant rows) = concatMap (concatMap varsInExpr) rows
varsInExpr (Sum _ lo hi body) = varsInExpr lo ++ varsInExpr hi ++ varsInExpr body
varsInExpr _ = []

varsInFormula :: Formula -> [String]
varsInFormula (Eq e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Le e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Lt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Ge e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Gt e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (Divides e1 e2) = varsInExpr e1 ++ varsInExpr e2
varsInFormula (And f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Or f1 f2) = varsInFormula f1 ++ varsInFormula f2
varsInFormula (Not f) = varsInFormula f
varsInFormula (Forall _ f) = varsInFormula f
varsInFormula (Exists _ f) = varsInFormula f


