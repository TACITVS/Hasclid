{-|
Module: Polynomial
Description: Unified polynomial operations for the theorem prover

This module provides a clean, centralized API for polynomial operations.
It re-exports the core types and functions from Expr.hs while providing
additional utilities and a cleaner namespace.

= Design Philosophy

Polynomials are represented as sparse maps from Monomials to Rational coefficients.
This representation is efficient for the typical case of polynomials with few
non-zero terms relative to their degree.

= Usage

@
import Polynomial

-- Creating polynomials
let x = var "x"
    y = var "y"
    p = x^2 + 2*x*y + y^2  -- Uses Num instance

-- Operations
let q = p * p
    r = p + q
    s = reduce p [q]  -- Polynomial reduction
@

-}
module Polynomial
  ( -- * Core Types
    Poly(..)
  , Monomial(..)

    -- * Construction
  , var
  , constant
  , zero
  , one
  , fromMonomial
  , fromTerms

    -- * Basic Arithmetic
  , add
  , sub
  , mul
  , neg
  , scale

    -- * Powers
  , pow
  , square

    -- * Queries
  , isZero
  , isConstant
  , degree
  , totalDegree
  , leadingTerm
  , leadingCoefficient
  , variables
  , coefficients
  , terms

    -- * Monomial Operations
  , monomialDegree
  , monomialDiv
  , monomialLCM
  , monomialGCD

    -- * Coefficient Management
  , content
  , primitivePart
  , simplify
  , clearDenominators

    -- * Re-exports from Expr for compatibility
  , polyAdd
  , polySub
  , polyMul
  , polyNeg
  , polyPow
  , polyFromVar
  , polyFromConst
  , polyZero
  , getVars
  , getLeadingTerm
  , monomialMul
  ) where

import Expr
  ( Poly(..)
  , Monomial(..)
  , polyAdd
  , polySub
  , polyMul
  , polyNeg
  , polyPow
  , polyFromVar
  , polyFromConst
  , polyZero
  , getVars
  , getLeadingTerm
  , isConstPoly
  , monomialMul
  )
import qualified Expr
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)

-- =============================================
-- Construction
-- =============================================

-- | Create a polynomial from a single variable.
var :: String -> Poly
var = polyFromVar

-- | Create a constant polynomial.
constant :: Rational -> Poly
constant = polyFromConst

-- | The zero polynomial.
zero :: Poly
zero = polyZero

-- | The constant polynomial 1.
one :: Poly
one = polyFromConst 1

-- | Create a polynomial from a monomial and coefficient.
fromMonomial :: Monomial -> Rational -> Poly
fromMonomial m c
  | c == 0 = zero
  | otherwise = Poly (M.singleton m c)

-- | Create a polynomial from a list of (monomial, coefficient) pairs.
fromTerms :: [(Monomial, Rational)] -> Poly
fromTerms ts = Poly $ M.filter (/= 0) $ M.fromListWith (+) ts

-- =============================================
-- Basic Arithmetic
-- =============================================

-- | Add two polynomials.
add :: Poly -> Poly -> Poly
add = polyAdd

-- | Subtract two polynomials.
sub :: Poly -> Poly -> Poly
sub = polySub

-- | Multiply two polynomials.
mul :: Poly -> Poly -> Poly
mul = polyMul

-- | Negate a polynomial.
neg :: Poly -> Poly
neg = polyNeg

-- | Scale a polynomial by a rational constant.
scale :: Rational -> Poly -> Poly
scale c (Poly m)
  | c == 0 = zero
  | otherwise = Poly $ M.map (* c) m

-- =============================================
-- Powers
-- =============================================

-- | Raise a polynomial to a non-negative integer power.
pow :: Poly -> Natural -> Poly
pow = polyPow

-- | Square a polynomial (more efficient than pow p 2).
square :: Poly -> Poly
square p = mul p p

-- =============================================
-- Queries
-- =============================================

-- | Check if a polynomial is zero.
isZero :: Poly -> Bool
isZero (Poly m) = M.null m

-- | Check if a polynomial is a constant.
isConstant :: Poly -> Bool
isConstant = isConstPoly

-- | Get the degree of a polynomial in a specific variable.
degree :: String -> Poly -> Int
degree v (Poly m)
  | M.null m = 0
  | otherwise = maximum $ 0 : [fromIntegral (M.findWithDefault 0 v vars) | (Monomial vars, _) <- M.toList m]

-- | Get the total degree of a polynomial (maximum sum of exponents in any term).
totalDegree :: Poly -> Int
totalDegree (Poly m)
  | M.null m = 0
  | otherwise = maximum $ 0 : [fromIntegral (sum (M.elems vars)) | (Monomial vars, _) <- M.toList m]

-- | Get the leading term (monomial, coefficient) using lexicographic order.
leadingTerm :: Poly -> Maybe (Monomial, Rational)
leadingTerm = getLeadingTerm

-- | Get the leading coefficient.
leadingCoefficient :: Poly -> Maybe Rational
leadingCoefficient p = snd <$> leadingTerm p

-- | Get all variables in a polynomial.
variables :: Poly -> S.Set String
variables = getVars

-- | Get all coefficients (including zero is excluded).
coefficients :: Poly -> [Rational]
coefficients (Poly m) = M.elems m

-- | Get all terms as (monomial, coefficient) pairs.
terms :: Poly -> [(Monomial, Rational)]
terms (Poly m) = M.toList m

-- =============================================
-- Monomial Operations
-- =============================================

-- | Get the total degree of a monomial.
monomialDegree :: Monomial -> Int
monomialDegree (Monomial m) = fromIntegral $ sum (M.elems m)

-- | Divide one monomial by another, if divisible.
-- Returns Nothing if not divisible.
monomialDiv :: Monomial -> Monomial -> Maybe Monomial
monomialDiv (Monomial m1) (Monomial m2) =
  let diff = M.mergeWithKey
               (\_ e1 e2 -> if e1 >= e2 then Just (e1 - e2) else Nothing)
               id
               (const M.empty)
               m1 m2
  in if any (< 0) (M.elems diff)
     then Nothing
     else Just $ Monomial $ M.filter (> 0) diff

-- | Compute the least common multiple of two monomials.
monomialLCM :: Monomial -> Monomial -> Monomial
monomialLCM (Monomial m1) (Monomial m2) =
  Monomial $ M.unionWith max m1 m2

-- | Compute the greatest common divisor of two monomials.
monomialGCD :: Monomial -> Monomial -> Monomial
monomialGCD (Monomial m1) (Monomial m2) =
  Monomial $ M.filter (> 0) $ M.intersectionWith min m1 m2

-- =============================================
-- Coefficient Management
-- =============================================

-- | Compute the content of a polynomial (GCD of all coefficients).
-- Returns the positive GCD of all numerators divided by LCM of all denominators.
-- For the zero polynomial, returns 0.
content :: Poly -> Rational
content (Poly m)
  | M.null m = 0
  | otherwise =
      let coeffs = M.elems m
          nums = map (abs . numerator) coeffs
          dens = map denominator coeffs
          gcdNums = foldl1' gcd nums
          lcmDens = foldl1' lcm dens
      in gcdNums % lcmDens

-- | Compute the primitive part of a polynomial (polynomial / content).
-- The result has integer coefficients with GCD = 1.
-- For the zero polynomial, returns the zero polynomial.
primitivePart :: Poly -> Poly
primitivePart p
  | isZero p = zero
  | otherwise =
      let c = content p
      in if c == 0 then p else scale (1 / c) p

-- | Simplify a polynomial by dividing out the content.
-- Equivalent to primitivePart but named for clarity.
simplify :: Poly -> Poly
simplify = primitivePart

-- | Clear denominators by multiplying by LCM of all coefficient denominators.
-- Returns (multiplier, resulting polynomial with integer coefficients).
clearDenominators :: Poly -> (Integer, Poly)
clearDenominators (Poly m)
  | M.null m = (1, zero)
  | otherwise =
      let dens = map denominator (M.elems m)
          mult = foldl' lcm 1 dens
          scaleFactor = fromInteger mult
          newCoeffs = M.map (* scaleFactor) m
      in (mult, Poly newCoeffs)

-- | Helper: compute GCD of a list of integers
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl' f x xs
foldl1' _ [] = error "Polynomial.foldl1': empty list"
