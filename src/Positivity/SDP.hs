{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Positivity.SDP
  ( checkSOS_SDP
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub, sort, sortBy, transpose, foldl')
import Data.Ord (comparing)
import Data.Ratio (denominator)
import Data.Array.IO
import Data.Array.MArray -- Need MArray interface
import Control.Monad (forM_, foldM, when)
import Control.Monad.ST
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, evaluate)
import Expr (Monomial(..), Poly(..), monomialMul, getVars, polyFromVar, polyMul, polyAdd)

-- ============================================================================
-- Burer-Monteiro SDP Solver (Low-Rank Factorization)
-- Solves: Find L such that sum (Tr(A_k L L^T) - b_k)^2 is minimized.
-- Returns: Just [L_col1, L_col2, ...] (coefficients of squares) or Nothing.
-- ============================================================================

checkSOS_SDP :: Poly -> Bool
checkSOS_SDP p = unsafePerformIO $ do
  let 
      vars = S.toList (getVars p)
      deg = polyDegree p
      halfDeg = (deg + 1) `div` 2
      basis = generateBasis vars halfDeg
      n = length basis
      
      targetCoeffs = case p of Poly m -> m
      constraintMap = M.fromListWith (++) 
        [ (monomialMul (basis !! i) (basis !! j), [(i,j)]) 
        | i <- [0..n-1], j <- [0..n-1] 
        ]
      
      maxMag = foldl' (\acc (_, val) -> max acc (abs (safeFromRational val))) 0.0 (M.toList targetCoeffs)
      scale = if maxMag == 0 then 1.0 else 1.0 / maxMag

      constraints = 
        [ (val * (doubleToRational scale), indices)
        | (m_k, val) <- M.toList targetCoeffs
        , let indices = M.findWithDefault [] m_k constraintMap
        ]
        
      unmatched = any (\(_, idxs) -> null idxs) constraints
      
  if unmatched || n > 60 then return False
  else do
      -- Solve for L (n x n, full rank approximation)
      -- We use a dense array for L
      maybeL <- _solveBM n constraints
      return (maybeL)

-- | Burer-Monteiro Solver
-- Returns True if converged to valid decomposition
_solveBM :: Int -> [(Rational, [(Int, Int)])] -> IO Bool
_solveBM n constrs = do
  -- L is n x n lower triangular (or full for simplicity)
  -- Initialize L = Identity * small factor
  lMat <- newArray ((0,0), (n-1,n-1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  forM_ [0..n-1] $ \i -> writeArray lMat (i,i) 0.1 -- Initial guess
  
  -- Pre-convert constraints to Double for speed
  let fastConstrs = [ (safeFromRational val, idxs) | (val, idxs) <- constrs ]
  
  -- Gradient Descent Loop
  let lr = 0.001 -- Learning rate
      maxIter = 2000
  
  finalErr <- _optimizeLoop 0 maxIter lr n lMat fastConstrs
  return (finalErr < 1e-4)

_optimizeLoop :: Int -> Int -> Double -> Int -> IOUArray (Int,Int) Double -> [(Double, [(Int,Int)])] -> IO Double
_optimizeLoop iter maxIter lr n lMat constrs
  | iter >= maxIter = _computeTotalError n lMat constrs
  | otherwise = do
      -- Compute Gradient: G = 4 * sum (error_k * A_k * L)
      -- A_k is sparse (indices). A_k * L adds rows of L.
      -- grad[u,v] = sum_k 4 * err_k * sum_{(i,j) in A_k} (delta_iu L_jv + delta_ju L_iv)
      
      -- 1. Compute Errors e_k = Tr(A_k X) - b_k
      -- X = L L^T. X_ij = sum_r L_ir L_jr
      errors <- mapM (computeError n lMat) constrs
      
      let totalErr = sum (map abs errors)
      if totalErr < 1e-5 then return totalErr -- Converged
      else do
        -- 2. Update L <- L - lr * Gradient
        -- We process constraints one by one to update L
        -- To be efficient, we accumulate updates or update in place?
        -- Stochastic GD: Update after each constraint? No, full batch.
        
        -- Accumulate gradient
        grad <- newArray ((0,0), (n-1,n-1)) 0.0 :: IO (IOUArray (Int,Int) Double)
        
        forM_ (zip errors constrs) $ \(err, (_, idxs)) -> do
           let factor = 4.0 * err
           forM_ idxs $ \(i,j) -> do
             -- Contribution from A_k element (i,j)
             -- A_k has 1 at (i,j).
             -- Grad_L += A_k L + A_k^T L (since symmetric A)
             -- If i==j, coeff is 1. If i!=j, coeff is 1 for (i,j) and 1 for (j,i) effectively?
             -- Our indices [(i,j)] represent the sum.
             -- If monomial x*y comes from x*y and y*x, we might have both or one?
             -- generateBasis/constraintMap generates ALL pairs i,j.
             -- So we just treat (i,j) as an entry 1 in A_k.
             -- Term in trace is L_i. * L_j.
             -- Derivative w.r.t L_uv:
             -- d/dL_uv (sum_r L_ir L_jr) = delta_iu L_jv + delta_ju L_iv
             
             -- Update row u=i: add factor * row j
             _addRowScaled n grad i j factor lMat
             -- Update row u=j: add factor * row i (if i!=j)
             when (i /= j) $ _addRowScaled n grad j i factor lMat

        -- Apply Gradient
        forM_ [0..n-1] $ \i ->
          forM_ [0..n-1] $ \j -> do
            g <- readArray grad (i,j)
            lVal <- readArray lMat (i,j)
            writeArray lMat (i,j) (lVal - lr * g)
            
        _optimizeLoop (iter+1) maxIter lr n lMat constrs

_addRowScaled :: Int -> IOUArray (Int,Int) Double -> Int -> Int -> Double -> IOUArray (Int,Int) Double -> IO ()
_addRowScaled n targetMat targetRow srcRow scale sourceMat = do
  forM_ [0..n-1] $ \col -> do
    srcVal <- readArray sourceMat (srcRow, col)
    curr <- readArray targetMat (targetRow, col)
    writeArray targetMat (targetRow, col) (curr + scale * srcVal)

computeError :: Int -> IOUArray (Int,Int) Double -> (Double, [(Int,Int)]) -> IO Double
computeError n lMat (b_k, idxs) = do
  -- Tr(A_k L L^T) = sum_{(i,j) in A_k} (L L^T)_ij
  -- (L L^T)_ij = dot(row i, row j)
  val <- foldM (\acc (i,j) -> do
                   dot <- _rowDot n lMat i j
                   return (acc + dot)
               ) 0.0 idxs
  return (val - b_k)

_rowDot :: Int -> IOUArray (Int,Int) Double -> Int -> Int -> IO Double
_rowDot n lMat i j = do
  -- Optimization: L is dense.
  let go k acc | k >= n = return acc
               | otherwise = do
                   v1 <- readArray lMat (i,k)
                   v2 <- readArray lMat (j,k)
                   go (k+1) (acc + v1*v2)
  go 0 0.0

_computeTotalError :: Int -> IOUArray (Int,Int) Double -> [(Double, [(Int,Int)])] -> IO Double
_computeTotalError n lMat constrs = do
  errs <- mapM (computeError n lMat) constrs
  return (sum (map abs errs))

-- Helpers
safeFromRational :: Rational -> Double
safeFromRational r = if d == 0 then 0.0 else fromRational r
  where d = denominator r

doubleToRational :: Double -> Rational
doubleToRational x = toRational (round (x * 1e10) :: Integer) / 1e10

generateBasis :: [String] -> Int -> [Monomial]
generateBasis vars d =
  let go 0 _ = [Monomial M.empty]
      go _ [] = [Monomial M.empty]
      go k (v:vs) = [ monomialMul (Monomial (M.singleton v (fromIntegral p))) rest | p <- [0..k], rest <- go (k-p) vs ]
  in sort $ go d vars

polyDegree :: Poly -> Int
polyDegree (Poly m) = fromIntegral $ maximum (0 : map (\(Monomial vm, _) -> sum (M.elems vm)) (M.toList m))