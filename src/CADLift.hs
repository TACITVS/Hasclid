{-# LANGUAGE DeriveGeneric #-}

module CADLift
  ( cadDecompose
  , CADCell(..)
  , SampleTree(..)
  , SignAssignment
  , evaluateInequalityCAD
  , formatCADCells
  ) where

import Expr
import CAD (discriminant, toRecursive, resultant)
import Sturm (isolateRoots, samplePoints, evalPoly)
import qualified Data.Map.Strict as M
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%), numerator, denominator)

-- =============================================
-- CAD Data Structures
-- =============================================

-- | A CAD cell represents a region in space
data CADCell = CADCell
  { cellDimension :: Int                    -- Dimension (1D, 2D, 3D...)
  , samplePoint :: M.Map String Rational    -- Representative sample point
  , cellDescription :: String                -- Human-readable description
  } deriving (Show, Eq)

-- | Sample tree for CAD lifting
data SampleTree
  = Leaf CADCell                             -- Base case: 1D cell
  | Node
      { baseValue :: Rational                -- Value in lower dimension
      , variable :: String                   -- Variable being lifted
      , children :: [SampleTree]             -- Cells above this value
      }
  deriving (Show, Eq)

-- | Sign assignment: polynomial -> sign at sample point
type SignAssignment = M.Map Poly Sign

data Sign = Negative | Zero | Positive deriving (Show, Eq, Ord)

-- =============================================
-- Main CAD Algorithm
-- =============================================

-- | Complete CAD decomposition for a set of polynomials
-- Returns: List of cells with sign assignments
cadDecompose :: [Poly] -> [String] -> [(CADCell, SignAssignment)]
cadDecompose polys vars =
  case vars of
    [] -> []  -- No variables
    [v] -> cad1D polys v  -- Base case: univariate
    (v:vs) ->  -- Recursive case: multivariate
      let
          -- Phase 1: Project to lower dimension
          projectedPolys = projectPolynomials polys v

          -- Phase 2: Recursively decompose lower dimension
          lowerCells = cadDecompose projectedPolys vs

          -- Phase 3: Lift to current dimension
          liftedCells = concatMap (liftCell polys v) lowerCells
      in
          liftedCells

-- =============================================
-- Phase 1: Projection
-- =============================================

projectPolynomials :: [Poly] -> String -> [Poly]
projectPolynomials polys var =
  let
      -- Collect all projection polynomials
      discriminants = [ discriminant p var | p <- polys, polyDegreeIn p var >= 2 ]
      resultants = [ resultant p q var | p <- polys, q <- polys, p /= q ]

      -- Remove zero polynomials and duplicates
      projected = nub $ filter (/= polyZero) (discriminants ++ resultants)
  in
      projected

polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  maximum (0 : [ fromIntegral (M.findWithDefault 0 var vars) | (Monomial vars, _) <- M.toList m ])

-- =============================================
-- Phase 2: Lifting (The Heart of CAD)
-- =============================================

-- | Lift a cell from (n-1)-D to n-D
liftCell :: [Poly] -> String -> (CADCell, SignAssignment) -> [(CADCell, SignAssignment)]
liftCell polys var (lowerCell, lowerSigns) =
  let
      -- Substitute lower cell's sample point into polynomials
      substituted = [ evaluatePoly (samplePoint lowerCell) p | p <- polys ]

      -- Find critical values (roots) in current variable
      criticalValues = concatMap (findRootsIn var) substituted

      -- Sort and create sample points between roots
      sortedValues = sort $ nub criticalValues

      -- Generate samples: one between each pair, plus -∞ and +∞
      samples = generateSamples sortedValues

      -- For each sample, create a cell and compute signs
      cells = [ createLiftedCell lowerCell var sampleVal polys
              | sampleVal <- samples ]
  in
      cells

-- | Find roots of a polynomial in a specific variable
findRootsIn :: String -> Poly -> [Rational]
findRootsIn var p =
  case toUnivariate p of
    Just (v, coeffs) | v == var ->
      let roots = isolateRoots coeffs
          -- Extract midpoints of intervals as sample roots
          midpoints = [ (lo + hi) / 2 | (lo, hi) <- roots ]
      in midpoints
    _ -> []  -- Not univariate in this variable

-- | Generate sample points between critical values
generateSamples :: [Rational] -> [Rational]
generateSamples [] = [0]  -- Default sample
generateSamples sorted =
  let
      -- Sample before first root
      beforeFirst = head sorted - 1

      -- Samples between consecutive roots
      between = [ (a + b) / 2 | (a, b) <- zip sorted (tail sorted) ]

      -- Sample after last root
      afterLast = last sorted + 1
  in
      beforeFirst : between ++ [afterLast]

-- | Create a lifted cell with sign assignment
createLiftedCell :: CADCell -> String -> Rational -> [Poly] -> (CADCell, SignAssignment)
createLiftedCell lowerCell var value polys =
  let
      -- Extend sample point with new variable
      newSample = M.insert var value (samplePoint lowerCell)

      -- Create new cell
      newCell = CADCell
        { cellDimension = cellDimension lowerCell + 1
        , samplePoint = newSample
        , cellDescription = cellDescription lowerCell ++ ", " ++ var ++ "=" ++ show value
        }

      -- Compute sign assignment
      signs = M.fromList [ (p, determineSign p newSample) | p <- polys ]
  in
      (newCell, signs)

-- | Determine sign of polynomial at a point
determineSign :: Poly -> M.Map String Rational -> Sign
determineSign p assignment =
  let evaluated = evaluatePoly assignment p
  in case polyToRational evaluated of
       Just r | r > 0 -> Positive
              | r < 0 -> Negative
              | otherwise -> Zero
       Nothing -> Zero  -- Default if can't evaluate

-- =============================================
-- Base Case: 1D CAD
-- =============================================

cad1D :: [Poly] -> String -> [(CADCell, SignAssignment)]
cad1D polys var =
  let
      -- Find all roots
      allRoots = concatMap (findRootsIn var) polys
      sortedRoots = sort $ nub allRoots

      -- Generate samples
      samples = generateSamples sortedRoots

      -- Create cells
      cells = [ create1DCell var value polys | value <- samples ]
  in
      cells

create1DCell :: String -> Rational -> [Poly] -> (CADCell, SignAssignment)
create1DCell var value polys =
  let
      sample = M.singleton var value
      cell = CADCell
        { cellDimension = 1
        , samplePoint = sample
        , cellDescription = var ++ "=" ++ show value
        }
      signs = M.fromList [ (p, determineSign p sample) | p <- polys ]
  in
      (cell, signs)

-- =============================================
-- Inequality Evaluation Using CAD
-- =============================================

-- | Evaluate an inequality using CAD
evaluateInequalityCAD :: [Poly] -> Poly -> [String] -> Bool
evaluateInequalityCAD constraints inequality vars =
  let
      -- Decompose with both constraints and inequality
      allPolys = inequality : constraints

      cells = cadDecompose allPolys vars

      -- Check if inequality holds in all cells satisfying constraints
      validCells = filter (cellSatisfiesConstraints constraints) cells

      inequalityHolds = all (cellSatisfiesInequality inequality) validCells
  in
      inequalityHolds

cellSatisfiesConstraints :: [Poly] -> (CADCell, SignAssignment) -> Bool
cellSatisfiesConstraints constraints (cell, signs) =
  all (\p -> M.lookup p signs == Just Zero) constraints

cellSatisfiesInequality :: Poly -> (CADCell, SignAssignment) -> Bool
cellSatisfiesInequality ineq (cell, signs) =
  M.lookup ineq signs == Just Positive || M.lookup ineq signs == Just Zero

-- =============================================
-- Helpers
-- =============================================

polyToRational :: Poly -> Maybe Rational
polyToRational (Poly m)
  | M.null m = Just 0
  | M.size m == 1 =
      case M.toList m of
        [(Monomial vm, c)] | M.null vm -> Just c
        _ -> Nothing
  | otherwise = Nothing

-- | Evaluate polynomial at a point
evaluatePoly :: M.Map String Rational -> Poly -> Poly
evaluatePoly assignment (Poly m) =
  let
      evalMonomial (Monomial vars) coeff =
        let varList = M.toList vars
            canEval = all (`M.member` assignment) (map fst varList)
        in if canEval
           then
             let value = product [ (assignment M.! v) ^ exp | (v, exp) <- varList ]
             in Just (coeff * value)
           else Nothing

      evaluated = [ (Monomial M.empty, val) | (mono, coeff) <- M.toList m
                                             , Just val <- [evalMonomial mono coeff] ]
  in
      if null evaluated
      then Poly m  -- Couldn't evaluate
      else Poly (M.fromListWith (+) evaluated)

-- =============================================
-- Pretty Printing
-- =============================================

formatCADCells :: [(CADCell, SignAssignment)] -> String
formatCADCells cells =
  unlines [ "Cell " ++ show i ++ ": " ++ cellDescription cell ++
            "\n  Sample: " ++ show (samplePoint cell)
          | (i, (cell, _)) <- zip [1..] cells ]
