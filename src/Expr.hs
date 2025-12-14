{-# LANGUAGE DeriveGeneric #-}

module Expr where

import Data.List (intercalate, sortBy, nub, maximumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric.Natural (Natural)
import Data.Ratio ((%), numerator, denominator)

-- =============================================
-- Symbolic Expressions (AST)
-- =============================================

data Expr
  = Var String
  | Const Rational
  | IntVar String               -- Integer domain variable
  | IntConst Integer            -- Integer domain constant
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Natural
  | Sqrt Expr
  -- Geometric primitives
  | Dist2 String String                    -- Squared distance between two points
  | Collinear String String String         -- Three points are collinear
  | Dot String String String String        -- Dot product of vectors AB and CD
  | Circle String String Expr              -- Point on circle with center and radius
  -- New geometry helpers
  | Midpoint String String String          -- M is midpoint of AB: (xM = (xA+xB)/2, etc.)
  | Perpendicular String String String String  -- AB ⊥ CD (dot product = 0)
  | Parallel String String String String       -- AB ∥ CD (cross product = 0)
  | AngleEq2D String String String String String String -- ∠ABC = ∠DEF (2D, oriented)
  | AngleEq2DAbs String String String String String String -- ∠ABC ≡ ∠DEF up to reflection (2D)
  -- High-level algebraic primitives
  | Determinant [[Expr]]                   -- Lazy determinant of a matrix
  | Sum String Expr Expr Expr              -- Summation: sum index low high body
  deriving (Eq, Ord, Show)

-- | Quantifier domain
data QuantType = QuantReal | QuantInt deriving (Eq, Show)

-- | Bound variable for quantifiers
data QuantVar = QuantVar
  { qvName :: String
  , qvType :: QuantType
  , qvLower :: Maybe Expr
  , qvUpper :: Maybe Expr
  } deriving (Eq, Show)

-- | Logical formulas (now with quantifiers)
data Formula
  = Eq Expr Expr    -- Equal (=)
  | Ge Expr Expr    -- Greater or Equal (>=)
  | Gt Expr Expr    -- Greater Than (>)
  | Le Expr Expr    -- Less or Equal (<=)
  | Lt Expr Expr    -- Less Than (<)
  | And Formula Formula
  | Or Formula Formula
  | Not Formula
  | Forall [QuantVar] Formula
  | Exists [QuantVar] Formula
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
prettyExpr (Pow e n)    = "(^ " ++ prettyExpr e ++ " " ++ show n ++ ")"
prettyExpr (Sqrt e)     = "(sqrt " ++ prettyExpr e ++ ")"
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
prettyFormula (And f1 f2) = "(and " ++ prettyFormula f1 ++ " " ++ prettyFormula f2 ++ ")"
prettyFormula (Or f1 f2) = "(or " ++ prettyFormula f1 ++ " " ++ prettyFormula f2 ++ ")"
prettyFormula (Not f) = "(not " ++ prettyFormula f ++ ")"
prettyFormula (Forall qs f) =
  "(forall (" ++ unwords (map prettyQuantVar qs) ++ ") " ++ prettyFormula f ++ ")"
prettyFormula (Exists qs f) =
  "(exists (" ++ unwords (map prettyQuantVar qs) ++ ") " ++ prettyFormula f ++ ")"

-- =============================================
-- Polynomial Engine
-- =============================================

newtype Monomial = Monomial (M.Map String Natural) deriving (Eq, Show)

-- | Lexicographic Term Order (Variable name based: z > y > x > ... > a)
-- Compares variables in descending order.
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
substituteExpr var val (Pow e n) = Pow (substituteExpr var val e) n
substituteExpr var val (Sqrt e) = Sqrt (substituteExpr var val e)
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
substituteAll subMap (Pow e n) = Pow (substituteAll subMap e) n
substituteAll subMap (Sqrt e) = Sqrt (substituteAll subMap e)
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
hasNonPolynomial (Sqrt _) = True
hasNonPolynomial (IntVar _) = True
hasNonPolynomial (IntConst _) = True
hasNonPolynomial (Add e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Sub e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Mul e1 e2) = hasNonPolynomial e1 || hasNonPolynomial e2
hasNonPolynomial (Pow e _) = hasNonPolynomial e
hasNonPolynomial (Sum _ _ _ _) = True
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
       (Const 0, _) -> Const 0              -- 0 / e = 0
       (e, Const 1) -> e                    -- e / 1 = e
       (Const r1, Const r2) | r2 /= 0 -> Const (r1 / r2)
       (Div a b, c) -> simplifyExpr (Div a (Mul b c))  -- (a/b)/c = a/(bc)
       (a, Div b c) -> simplifyExpr (Div (Mul a c) b)  -- a/(b/c) = (ac)/b
       _ | s1 == s2 -> Const 1              -- e / e = 1
       _ -> Div s1 s2

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
normalizeCommAssoc (Pow e n) = Pow (normalizeCommAssoc e) n
normalizeCommAssoc (Sqrt e) = Sqrt (normalizeCommAssoc e)
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
toPoly (Div _ _)   = error "Division Error: Division is not supported in polynomial expressions.\nNote: Rational constants like 1/2 are supported, but division of variables is not.\nContext: Attempting to convert Div expression to polynomial."
toPoly (Pow e n)   = polyPow (toPoly e) n
toPoly (Sqrt _)    = error "Sqrt Error: Square roots are not supported in polynomial expressions. Rewrite without sqrt or use a geometric/analytic solver."
toPoly (Sum _ _ _ _) = error "Sum Error: Summation must be expanded or handled by induction before polynomial conversion."

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

containsSqrtExpr :: Expr -> Bool
containsSqrtExpr (Sqrt _) = True
containsSqrtExpr (Add a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Sub a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Mul a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Div a b) = containsSqrtExpr a || containsSqrtExpr b
containsSqrtExpr (Pow e _) = containsSqrtExpr e
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
containsIntExpr (Pow e _) = containsIntExpr e
containsIntExpr (Sqrt e) = containsIntExpr e
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
substituteIntsExpr sub (Pow e n) = Pow (substituteIntsExpr sub e) n
substituteIntsExpr sub (Sqrt e) = Sqrt (substituteIntsExpr sub e)
substituteIntsExpr _ e = e

containsSqrtFormula :: Formula -> Bool
containsSqrtFormula (Eq l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Ge l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Gt l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Le l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (Lt l r) = containsSqrtExpr l || containsSqrtExpr r
containsSqrtFormula (And f1 f2) = containsSqrtFormula f1 || containsSqrtFormula f2
containsSqrtFormula (Or f1 f2) = containsSqrtFormula f1 || containsSqrtFormula f2
containsSqrtFormula (Not f) = containsSqrtFormula f
containsSqrtFormula (Forall _ f) = containsSqrtFormula f
containsSqrtFormula (Exists _ f) = containsSqrtFormula f

-- | Check if expression contains division (for rational elimination)
containsDivExpr :: Expr -> Bool
containsDivExpr (Div _ _) = True
containsDivExpr (Add a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Sub a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Mul a b) = containsDivExpr a || containsDivExpr b
containsDivExpr (Pow e _) = containsDivExpr e
containsDivExpr (Sqrt e) = containsDivExpr e
containsDivExpr (Determinant rows) = any containsDivExpr (concat rows)
containsDivExpr (Circle _ _ e) = containsDivExpr e
containsDivExpr _ = False

containsDivFormula :: Formula -> Bool
containsDivFormula (Eq l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Ge l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Gt l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Le l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (Lt l r) = containsDivExpr l || containsDivExpr r
containsDivFormula (And f1 f2) = containsDivFormula f1 || containsDivFormula f2
containsDivFormula (Or f1 f2) = containsDivFormula f1 || containsDivFormula f2
containsDivFormula (Not f) = containsDivFormula f
containsDivFormula (Forall _ f) = containsDivFormula f
containsDivFormula (Exists _ f) = containsDivFormula f

-- =============================================
-- Integer square root helper
-- =============================================

integerSqrt :: Integer -> Integer
integerSqrt n
  | n < 0 = 0
  | otherwise = floor (sqrt (fromIntegral n :: Double))

-- =============================================
-- Logic helpers
-- =============================================

type Theory = [Formula]

containsQuantifier :: Formula -> Bool
containsQuantifier (Forall _ _) = True
containsQuantifier (Exists _ _) = True
containsQuantifier _ = False

containsIntFormula :: Formula -> Bool
containsIntFormula (Eq l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Ge l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Gt l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Le l r) = containsIntExpr l || containsIntExpr r
containsIntFormula (Lt l r) = containsIntExpr l || containsIntExpr r
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
      Var v -> [(v, toPoly rhs)]
      IntVar v -> [(v, toPoly rhs)]
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
