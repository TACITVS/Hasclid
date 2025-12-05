{-# LANGUAGE DeriveGeneric #-}

module Validation
  ( ValidationWarning(..)
  , validateTheory
  , checkCoincidentPoints
  , checkZeroLengthSegments
  , checkCollinearity
  , formatWarning
  , formatWarnings
  ) where

import Expr
import Prover (buildSubMap, toPolySub)
import qualified Data.Map.Strict as M
import Data.List (nub, tails)

-- =============================================
-- Validation Warning Types
-- =============================================

data ValidationWarning
  = CoincidentPointsWarning String String           -- Two points at same location
  | ZeroLengthSegmentWarning String String          -- Segment with zero length
  | CollinearPointsWarning String String String     -- Three collinear points
  | DegenerateConfigWarning String                  -- General degeneracy
  deriving (Eq, Show)

-- =============================================
-- Main Validation Function
-- =============================================

-- | Validate a theory and return list of warnings
validateTheory :: Theory -> [ValidationWarning]
validateTheory theory =
  let points = extractPoints theory
      subMap = buildSubMap theory
  in concat
       [ checkCoincidentPoints points subMap
       , checkZeroLengthSegments points subMap
       -- Collinearity is commented out for now as it may have too many false positives
       -- , checkCollinearity points subMap
       ]

-- =============================================
-- Point Extraction
-- =============================================

-- | Extract all defined points from a theory
extractPoints :: Theory -> [String]
extractPoints theory =
  let varNames = [ v | Eq (Var v) _ <- theory ]
      -- Extract point names from variables like "xA", "yA", "zA"
      pointVars = [ drop 1 n | n <- varNames, length n >= 2, take 1 n `elem` ["x", "y", "z"] ]
  in nub pointVars

-- | Get coordinates of a point from substitution map
getPointCoords :: String -> M.Map String Poly -> Maybe (Rational, Rational, Rational)
getPointCoords pname subMap =
  let xVar = "x" ++ pname
      yVar = "y" ++ pname
      zVar = "z" ++ pname

      getConst poly = case poly of
                        Poly m | M.size m == 1 ->
                          case M.toList m of
                            [(Monomial vars, c)] | M.null vars -> Just c
                            _ -> Nothing
                        Poly m | M.null m -> Just 0
                        _ -> Nothing

      mx = M.lookup xVar subMap >>= getConst
      my = M.lookup yVar subMap >>= getConst
      mz = M.lookup zVar subMap >>= getConst
  in case (mx, my, mz) of
       (Just x, Just y, Just z) -> Just (x, y, z)
       _ -> Nothing

-- =============================================
-- Validation Checks
-- =============================================

-- | Check for coincident points (same location)
checkCoincidentPoints :: [String] -> M.Map String Poly -> [ValidationWarning]
checkCoincidentPoints points subMap =
  [ CoincidentPointsWarning p1 p2
  | (p1:rest) <- tails points
  , p2 <- rest
  , areCoincident p1 p2 subMap
  ]
  where
    areCoincident :: String -> String -> M.Map String Poly -> Bool
    areCoincident p1 p2 subMap =
      case (getPointCoords p1 subMap, getPointCoords p2 subMap) of
        (Just (x1, y1, z1), Just (x2, y2, z2)) ->
          x1 == x2 && y1 == y2 && z1 == z2
        _ -> False

-- | Check for zero-length segments
checkZeroLengthSegments :: [String] -> M.Map String Poly -> [ValidationWarning]
checkZeroLengthSegments points subMap =
  [ ZeroLengthSegmentWarning p1 p2
  | (p1:rest) <- tails points
  , p2 <- rest
  , hasZeroLength p1 p2 subMap
  ]
  where
    hasZeroLength :: String -> String -> M.Map String Poly -> Bool
    hasZeroLength p1 p2 subMap =
      case (getPointCoords p1 subMap, getPointCoords p2 subMap) of
        (Just (x1, y1, z1), Just (x2, y2, z2)) ->
          let dx = x2 - x1
              dy = y2 - y1
              dz = z2 - z1
          in dx == 0 && dy == 0 && dz == 0
        _ -> False

-- | Check for collinear point triples (commented out - may have false positives)
checkCollinearity :: [String] -> M.Map String Poly -> [ValidationWarning]
checkCollinearity points subMap =
  [ CollinearPointsWarning p1 p2 p3
  | p1 <- points
  , p2 <- points
  , p3 <- points
  , p1 /= p2 && p2 /= p3 && p1 /= p3
  , areCollinear p1 p2 p3 subMap
  ]
  where
    areCollinear :: String -> String -> String -> M.Map String Poly -> Bool
    areCollinear p1 p2 p3 subMap =
      case (getPointCoords p1 subMap, getPointCoords p2 subMap, getPointCoords p3 subMap) of
        (Just (x1, y1, _), Just (x2, y2, _), Just (x3, y3, _)) ->
          let -- Cross product magnitude for 2D
              crossProd = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
          in crossProd == 0
        _ -> False

-- =============================================
-- Warning Formatting
-- =============================================

formatWarning :: ValidationWarning -> String
formatWarning (CoincidentPointsWarning p1 p2) =
  "[WARNING] Points '" ++ p1 ++ "' and '" ++ p2 ++ "' are coincident (same location)"

formatWarning (ZeroLengthSegmentWarning p1 p2) =
  "[WARNING] Segment '" ++ p1 ++ p2 ++ "' has zero length (points coincide)"

formatWarning (CollinearPointsWarning p1 p2 p3) =
  "[WARNING] Points '" ++ p1 ++ "', '" ++ p2 ++ "', '" ++ p3 ++ "' are collinear"

formatWarning (DegenerateConfigWarning msg) =
  "[WARNING] Degenerate configuration - " ++ msg

formatWarnings :: [ValidationWarning] -> String
formatWarnings [] = "[OK] No degeneracy issues detected"
formatWarnings warnings =
  "Validation Warnings (" ++ show (length warnings) ++ "):\n" ++
  unlines (map formatWarning warnings)

-- =============================================
-- Utility Functions
-- =============================================

-- | Check if a point is fully defined (all coordinates are constants)
isPointFullyDefined :: String -> M.Map String Poly -> Bool
isPointFullyDefined pname subMap =
  case getPointCoords pname subMap of
    Just _ -> True
    Nothing -> False

-- | Get all fully defined points
getDefinedPoints :: [String] -> M.Map String Poly -> [String]
getDefinedPoints points subMap =
  filter (\p -> isPointFullyDefined p subMap) points
