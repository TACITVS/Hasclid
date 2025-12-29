module Locus
  ( locusEquation
  , formatLocus
  ) where

import Core.Types
import Core.GB (buchbergerOptimized)
import TermOrder (TermOrder(..), compareMonomials)
import Preprocessing (preprocessGeometry)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (nub, partition)

-- | Compute the locus equation for a point P given a theory
-- Returns a list of polynomials describing the locus (ideally just one)
locusEquation :: M.Map String Expr -> Theory -> String -> [Poly]
locusEquation pointSubs theory pointName =
  let 
    -- 1. Preprocess theory to get polynomial constraints
    -- Use a dummy goal to trigger full preprocessing
    dummyGoal = Eq (Var "dummy") (Const 0)
    (theoryG, _, _) = preprocessGeometry pointSubs theory dummyGoal
    
    -- 2. Convert to polynomials (implicit elimination of some vars via pointSubs)
    -- We need to handle sqrt/rational? For now assume basic polynomial geometry.
    -- Or re-use SolverRouter's pipeline?
    -- Let's stick to raw polynomial conversion for transparency.
    subMap = buildSubMap theoryG
    polys = [ toPolySub subMap (Sub l r) | Eq l r <- theoryG ]
    
    -- 3. Identify Variables
    allVars = S.toList $ S.unions $ map getVars polys
    
    -- Locus Variables: xP, yP (, zP)
    locusVars = filter (isPointCoord pointName) allVars
    
    -- Dependent Variables: Everything else
    -- dependentVars = filter (`notElem` locusVars) allVars
    
    -- 4. Construct Elimination Order: Dependent >> Locus
    -- We use renaming trick because Poly type has fixed Ord Monomial
    
    -- 5. Compute Gröbner Basis
    -- Use Buchberger (or F5 via SolverRouter if we link it, but keep it simple here)
    -- We need a custom GB function that accepts TermOrder?
    -- Core.GB.buchbergerOptimized uses internal order?
    -- No, it takes [Poly]. Poly uses Ord Monomial. 
    -- Monomial Ord is fixed!
    -- WE CANNOT CHANGE TERM ORDER EASILY without re-wrapping Poly.
    -- BUT: We can rename variables!
    -- Rename dependent variables to be "greater" than locus variables.
    -- Default Monomial comparison is Lexicographic on variable names.
    -- So if Dependent vars start with "a_" and Locus with "z_", "z_" > "a_" (Lex).
    -- Wait, Lex: "a" < "z".
    -- If we want Dependent > Locus, we need Dependent to be "larger".
    -- "var1" > "var2" in Map keys?
    -- Haskell Map is sorted.
    -- Monomial comparison uses `compare` on variable lists.
    -- `compare (Monomial m1) (Monomial m2)` sorts variables.
    -- Let's check `Core.Types.hs` `instance Ord Monomial`.
    
    {- 
    instance Ord Monomial where
      compare (Monomial m1) (Monomial m2) =
        let vars = sortBy (flip compare) (nub (M.keys m1 ++ M.keys m2)) 
        in go vars ...
    -}
    -- It sorts variables in DESCENDING order: z, y, x...
    -- Then compares exponents.
    -- So "z" is considered "leading" (highest).
    -- Lex order: checking exponents of largest variable first.
    -- If we want to eliminate `D` (Dependent), `D` should be the leading variables.
    -- So `D` should be lexicographically "larger" than `L` (Locus).
    -- So we should rename Dependent vars to "yy_..." and Locus to "aa_..."?
    -- Yes.
    
    -- 6. Renaming Strategy
    -- Prefix Dependent vars with "yy_" (high)
    -- Prefix Locus vars with "aa_" (low)
    -- Keep Constants/Params? Parameters should be low too (kept).
    -- So:
    --   Dependent -> "yy_" ++ var
    --   Locus     -> "aa_" ++ var
    --   Others    -> "aa_" ++ var
    
    rename v
      | v `elem` locusVars = "aa_" ++ v
      | otherwise          = "yy_" ++ v
      
    inverseRename v
      | "aa_" `isPrefixOf` v = drop 3 v
      | "yy_" `isPrefixOf` v = drop 3 v
      | otherwise = v
      
    renamedPolys = map (renamePoly rename) polys
    
    -- 7. Compute Basis
    basis = buchbergerOptimized renamedPolys -- Uses default Monomial Ord (Lex-like on sorted vars)
    
    -- 8. Filter and Rename Back
    -- Keep polynomials that ONLY contain "aa_" variables (Locus + Params)
    isLocusPoly p = all ("aa_" `isPrefixOf`) (S.toList $ getVars p)
    
    locusPolys = filter isLocusPoly basis
    finalPolys = map (renamePoly inverseRename) locusPolys
    
  in finalPolys

isPointCoord :: String -> String -> Bool
isPointCoord p v = v == ("x"++p) || v == ("y"++p) || v == ("z"++p)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

renamePoly :: (String -> String) -> Poly -> Poly
renamePoly f (Poly m) =
  let terms = [ (renameMonomial f mono, c) | (mono, c) <- M.toList m ]
  in Poly (M.fromListWith (+) terms)

renameMonomial :: (String -> String) -> Monomial -> Monomial
renameMonomial f (Monomial m) =
  Monomial (M.mapKeys f m)

formatLocus :: [Poly] -> String
formatLocus [] = "Locus is the whole plane (or computation failed)"
formatLocus [p] = "Locus Equation: " ++ prettyPolyNice p ++ " = 0"
formatLocus ps = "Locus defined by system:\n" ++ unlines (map (\p -> "  " ++ prettyPolyNice p ++ " = 0") ps)
