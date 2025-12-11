{-# LANGUAGE DeriveGeneric #-}

module TermOrder
  ( TermOrder(..)
  , compareMonomialsWithOrder
  , compareMonomials
  , defaultTermOrder
  , parseTermOrder
  , showTermOrder
  ) where

import qualified Data.Map.Strict as M
import Numeric.Natural
import Data.List (sortBy)
import GHC.Generics (Generic)
import Expr (Monomial(..))

-- =============================================
-- Term Ordering Types
-- =============================================

-- | Term ordering strategies for Gröbner basis computation
data TermOrder
  = Lex      -- ^ Pure lexicographic ordering
  | GrLex    -- ^ Graded lexicographic (total degree, then lex)
  | GrevLex  -- ^ Graded reverse lexicographic (most common in practice)
  deriving (Eq, Show, Read, Generic)

-- | Default term ordering (GrevLex is generally most efficient)
defaultTermOrder :: TermOrder
defaultTermOrder = GrevLex

-- =============================================
-- Monomial Comparison
-- =============================================

-- | Compare two monomials using specified term ordering
-- Takes variable maps and returns Ordering
compareMonomialsWithOrder :: TermOrder -> M.Map String Natural -> M.Map String Natural -> Ordering
compareMonomialsWithOrder Lex     = compareLex
compareMonomialsWithOrder GrLex   = compareGrLex
compareMonomialsWithOrder GrevLex = compareGrevLex

-- | Compare two Monomial values using the specified term ordering
-- This is a wrapper around compareMonomialsWithOrder for the Monomial newtype
compareMonomials :: TermOrder -> Monomial -> Monomial -> Ordering
compareMonomials ord (Monomial m1) (Monomial m2) = compareMonomialsWithOrder ord m1 m2

-- =============================================
-- Lexicographic Ordering (Lex)
-- =============================================
-- Compare variables in alphabetical order
-- Example: x^2*y > x*y^2 > y^3

compareLex :: M.Map String Natural -> M.Map String Natural -> Ordering
compareLex m1 m2 =
  let allVars = M.keys m1 ++ M.keys m2
      sortedVars = sortBy compare allVars
  in compareLexByVars sortedVars m1 m2
  where
    compareLexByVars [] _ _ = EQ
    compareLexByVars (v:vs) ma mb =
      let ea = M.findWithDefault 0 v ma
          eb = M.findWithDefault 0 v mb
      in case compare ea eb of
           EQ -> compareLexByVars vs ma mb
           other -> other

-- =============================================
-- Graded Lexicographic Ordering (GrLex)
-- =============================================
-- First compare total degree, then use lex
-- Example: x*y^2 > x^2 (degree 3 > 2), x^2*y > x*y^2 (same degree, lex wins)

compareGrLex :: M.Map String Natural -> M.Map String Natural -> Ordering
compareGrLex m1 m2 =
  let deg1 = totalDegree m1
      deg2 = totalDegree m2
  in case compare deg1 deg2 of
       EQ -> compareLex m1 m2  -- Same degree, use lex
       other -> other           -- Different degree

totalDegree :: M.Map String Natural -> Natural
totalDegree = M.foldl (+) 0

-- =============================================
-- Graded Reverse Lexicographic Ordering (GrevLex)
-- =============================================
-- First compare total degree, then reverse lex from the right
-- Most efficient for most problems
-- Example: x^2*y > x*y^2 (degree tie, rightmost variable y has lower power)

compareGrevLex :: M.Map String Natural -> M.Map String Natural -> Ordering
compareGrevLex m1 m2 =
  let deg1 = totalDegree m1
      deg2 = totalDegree m2
  in case compare deg1 deg2 of
       EQ -> compareReverseLex m1 m2  -- Same degree, use reverse lex
       other -> other                  -- Different degree

compareReverseLex :: M.Map String Natural -> M.Map String Natural -> Ordering
compareReverseLex m1 m2 =
  let allVars = M.keys m1 ++ M.keys m2
      sortedVars = reverse (sortBy compare allVars)  -- Reverse order
  in compareRevLexByVars sortedVars m1 m2
  where
    compareRevLexByVars [] _ _ = EQ
    compareRevLexByVars (v:vs) ma mb =
      let ea = M.findWithDefault 0 v ma
          eb = M.findWithDefault 0 v mb
      in case compare eb ea of  -- Note: reversed comparison (eb, ea)
           EQ -> compareRevLexByVars vs ma mb
           other -> other

-- =============================================
-- Parsing and Display
-- =============================================

parseTermOrder :: String -> Maybe TermOrder
parseTermOrder str = case map toLowerChar str of
  "lex"     -> Just Lex
  "grlex"   -> Just GrLex
  "grevlex" -> Just GrevLex
  _         -> Nothing
  where
    toLowerChar c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
                  | otherwise = c

showTermOrder :: TermOrder -> String
showTermOrder Lex     = "Lex (Lexicographic)"
showTermOrder GrLex   = "GrLex (Graded Lexicographic)"
showTermOrder GrevLex = "GrevLex (Graded Reverse Lexicographic)"

-- =============================================
-- Performance Notes
-- =============================================
{-
Term ordering choice significantly affects Gröbner basis computation:

Lex:     Simplest, but often produces largest bases (slowest)
GrLex:   Good middle ground
GrevLex: Most efficient for typical problems (RECOMMENDED)

Performance varies by problem structure:
- Lex: Good for elimination ideals
- GrLex: Good for symmetric problems
- GrevLex: Best general-purpose choice (2-10x faster than Lex)
-}
