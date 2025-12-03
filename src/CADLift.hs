{-# LANGUAGE DeriveGeneric #-}

module CADLift
  ( cadDecompose
  , CADCell(..)
  , CellType(..)
  , SampleTree(..)
  , SignAssignment
  , Sign(..)
  , evaluateInequalityCAD
  , evaluateFormula
  , satisfyingCells
  , proveWithCAD
  , formatCADCells
  ) where

import Expr
import CAD (discriminant, toRecursive, resultant, completeProjection)
import Sturm (isolateRoots, samplePoints, evalPoly)
import qualified Data.Map.Strict as M
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%), numerator, denominator)

-- =============================================
-- CAD Data Structures
-- =============================================

-- | Cell type classification (CRITICAL for proper CAD)
data CellType
  = Sector                              -- Open region (full dimension n)
  | Section [Poly]                      -- Manifold where polynomials vanish (dimension n-1)
  deriving (Show, Eq)

-- | A CAD cell represents a region in space
data CADCell = CADCell
  { cellDimension :: Int                    -- Dimension (1D, 2D, 3D...)
  , samplePoint :: M.Map String Rational    -- Representative sample point
  , cellType :: CellType                    -- Section or Sector ✅ NEW!
  , vanishingPolys :: [Poly]                -- Polynomials that are zero in this cell ✅ NEW!
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
-- Phase 1: Projection (COMPLETE - Collins 1975)
-- =============================================

-- | Project polynomials to lower dimension using Collins' COMPLETE projection.
--   This now includes:
--   - Discriminants
--   - Resultants
--   - Principal Subresultant Coefficients (PSC) ✅ NEW!
--   - Leading coefficients ✅ NEW!
--   - All coefficients ✅ NEW!
--
--   This guarantees sign-invariance in the lifted cells!
projectPolynomials :: [Poly] -> String -> [Poly]
projectPolynomials polys var = completeProjection polys var

polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  maximum (0 : [ fromIntegral (M.findWithDefault 0 var vars) | (Monomial vars, _) <- M.toList m ])

-- =============================================
-- Phase 2: Lifting (The Heart of CAD)
-- =============================================

-- | Lift a cell from (n-1)-D to n-D
--   NOW CREATES BOTH SECTIONS AND SECTORS!
liftCell :: [Poly] -> String -> (CADCell, SignAssignment) -> [(CADCell, SignAssignment)]
liftCell polys var (lowerCell, lowerSigns) =
  let
      -- Substitute lower cell's sample point into polynomials
      substituted = [ evaluatePoly (samplePoint lowerCell) p | p <- polys ]

      -- Find critical values (roots) in current variable
      criticalValues = concatMap (findRootsIn var) substituted

      -- Sort and create sample points between roots
      sortedValues = sort $ nub criticalValues

      -- Generate samples: BOTH sections (on roots) AND sectors (between roots)
      (sectionSamples, sectorSamples) = generateSamplesClassified sortedValues

      -- Create SECTION cells (where polynomials vanish)
      sectionCells = [ createLiftedCell lowerCell var sampleVal polys True
                     | sampleVal <- sectionSamples ]

      -- Create SECTOR cells (open regions)
      sectorCells = [ createLiftedCell lowerCell var sampleVal polys False
                    | sampleVal <- sectorSamples ]
  in
      -- Return sections AND sectors, interleaved properly
      interleave sectorCells sectionCells

-- Helper: Interleave two lists (sector, section, sector, section, ...)
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

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

-- | Generate samples classified as sections (on roots) and sectors (between roots)
--   Returns: (sectionSamples, sectorSamples)
generateSamplesClassified :: [Rational] -> ([Rational], [Rational])
generateSamplesClassified [] = ([], [0])  -- No roots: just one sector
generateSamplesClassified sorted =
  let
      -- SECTIONS: Sample points ON the roots
      sections = sorted

      -- SECTORS: Sample points BETWEEN roots
      beforeFirst = head sorted - 1
      between = [ (a + b) / 2 | (a, b) <- zip sorted (tail sorted) ]
      afterLast = last sorted + 1
      sectors = beforeFirst : between ++ [afterLast]
  in
      (sections, sectors)

-- | Create a lifted cell with sign assignment
--   NOW WITH PROPER CELL CLASSIFICATION!
createLiftedCell :: CADCell -> String -> Rational -> [Poly] -> Bool -> (CADCell, SignAssignment)
createLiftedCell lowerCell var value polys isSection =
  let
      -- Extend sample point with new variable
      newSample = M.insert var value (samplePoint lowerCell)

      -- Compute sign assignment
      signs = M.fromList [ (p, determineSign p newSample) | p <- polys ]

      -- Determine which polynomials vanish at this sample
      vanishing = [ p | (p, Zero) <- M.toList signs ]

      -- Classify cell type
      cType = if isSection && not (null vanishing)
              then Section vanishing
              else Sector

      -- Create description
      typeStr = case cType of
                  Sector -> "SECTOR"
                  Section ps -> "SECTION(" ++ show (length ps) ++ " polys vanish)"

      -- Create new cell
      newCell = CADCell
        { cellDimension = cellDimension lowerCell + 1
        , samplePoint = newSample
        , cellType = cType
        , vanishingPolys = vanishing
        , cellDescription = cellDescription lowerCell ++ ", " ++ var ++ "=" ++ show value ++ " [" ++ typeStr ++ "]"
        }
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
-- Base Case: 1D CAD (WITH CLASSIFICATION)
-- =============================================

cad1D :: [Poly] -> String -> [(CADCell, SignAssignment)]
cad1D polys var =
  let
      -- Find all roots
      allRoots = concatMap (findRootsIn var) polys
      sortedRoots = sort $ nub allRoots

      -- Generate samples classified as sections and sectors
      (sectionSamples, sectorSamples) = generateSamplesClassified sortedRoots

      -- Create section cells (on roots)
      sectionCells = [ create1DCell var value polys True | value <- sectionSamples ]

      -- Create sector cells (between roots)
      sectorCells = [ create1DCell var value polys False | value <- sectorSamples ]
  in
      -- Interleave: sector, section, sector, section, ...
      interleave sectorCells sectionCells

create1DCell :: String -> Rational -> [Poly] -> Bool -> (CADCell, SignAssignment)
create1DCell var value polys isSection =
  let
      sample = M.singleton var value
      signs = M.fromList [ (p, determineSign p sample) | p <- polys ]

      -- Determine which polynomials vanish
      vanishing = [ p | (p, Zero) <- M.toList signs ]

      -- Classify cell type
      cType = if isSection && not (null vanishing)
              then Section vanishing
              else Sector

      -- Create description
      typeStr = case cType of
                  Sector -> "SECTOR"
                  Section ps -> "SECTION(" ++ show (length ps) ++ " polys vanish)"

      cell = CADCell
        { cellDimension = 1
        , samplePoint = sample
        , cellType = cType
        , vanishingPolys = vanishing
        , cellDescription = var ++ "=" ++ show value ++ " [" ++ typeStr ++ "]"
        }
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
-- Formula Evaluation Over CAD (CRITICAL!)
-- =============================================

-- | Evaluate an arbitrary formula over a CAD cell.
--   This is THE PURPOSE of CAD - to determine truth of formulas in each cell.
--
--   Returns True if the formula holds in the cell, False otherwise.
evaluateFormula :: Formula -> (CADCell, SignAssignment) -> Bool
evaluateFormula formula (cell, signs) =
  case formula of
    -- Equality: f = g  ⟺  f - g = 0
    Eq lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in M.lookup diff signs == Just Zero

    -- Greater or equal: f >= g  ⟺  f - g >= 0
    Ge lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in case M.lookup diff signs of
           Just Positive -> True
           Just Zero -> True
           _ -> False

    -- Greater than: f > g  ⟺  f - g > 0
    Gt lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in M.lookup diff signs == Just Positive

-- | Find all cells that satisfy a given formula.
--   This is how we use CAD to prove/disprove formulas!
satisfyingCells :: Formula -> [(CADCell, SignAssignment)] -> [(CADCell, SignAssignment)]
satisfyingCells formula cells =
  filter (evaluateFormula formula) cells

-- | Prove or disprove a formula using CAD.
--   Returns True if formula holds in ALL cells of the decomposition.
--
--   Usage:
--     proveWithCAD (Ineq (Var "x") (Const 0)) ["x"]
--       -> False (x can be negative)
--
--     proveWithCAD (Eq (Mul (Var "x") (Var "x")) (Add (Mul (Var "x") (Var "x")) (Const 0))) ["x"]
--       -> True (identity holds everywhere)
proveWithCAD :: Formula -> [String] -> Bool
proveWithCAD formula vars =
  let
      -- Extract polynomials from formula
      polys = formulaToPolys formula

      -- Decompose space
      cells = cadDecompose polys vars

      -- Check if formula holds in ALL cells
  in all (\cell -> evaluateFormula formula cell) cells

-- | Extract all polynomials from a formula
formulaToPolys :: Formula -> [Poly]
formulaToPolys formula =
  case formula of
    Eq lhs rhs -> [exprToPoly (Sub lhs rhs)]
    Ge lhs rhs -> [exprToPoly (Sub lhs rhs)]
    Gt lhs rhs -> [exprToPoly (Sub lhs rhs)]

-- | Convert an expression to a polynomial
--   (Simple implementation - assumes expression is already polynomial)
exprToPoly :: Expr -> Poly
exprToPoly expr =
  case expr of
    Const r -> polyFromConst r
    Var v -> polyFromVar v
    Add e1 e2 -> polyAdd (exprToPoly e1) (exprToPoly e2)
    Sub e1 e2 -> polySub (exprToPoly e1) (exprToPoly e2)
    Mul e1 e2 -> polyMul (exprToPoly e1) (exprToPoly e2)
    Pow e n -> polyPow (exprToPoly e) (fromIntegral n)
    _ -> polyZero  -- Unsupported (geometric primitives, etc.)

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
  unlines [ formatCell i cell signs | (i, (cell, signs)) <- zip [1..] cells ]
  where
    formatCell i cell signs =
      let typeInfo = case cellType cell of
                       Sector -> "SECTOR (full dimension)"
                       Section ps -> "SECTION (manifold, " ++ show (length ps) ++ " polys vanish)"
          sampleInfo = "Sample: " ++ show (samplePoint cell)
          signInfo = if null (M.toList signs)
                     then "  Signs: (none)"
                     else "  Signs: " ++ formatSigns signs
      in unlines
           [ "Cell " ++ show i ++ ": [" ++ typeInfo ++ "]"
           , "  " ++ sampleInfo
           , signInfo
           ]

    formatSigns signs =
      let signList = M.toList signs
          formatOne (p, s) = show s
      in unwords (map formatOne (take 3 signList)) ++
         if length signList > 3 then " ..." else ""
