{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Positivity.SDP
  ( checkSOS_SDP
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub, sort, sortBy, transpose)
import Data.Ord (comparing)
import Data.Array.IO
import Data.Array.MArray -- Need MArray interface
import Control.Monad (forM_, foldM, when)
import Control.Monad.ST
import System.IO.Unsafe (unsafePerformIO)
import Expr (Monomial(..), Poly(..), monomialMul, getVars, polyFromVar, polyMul, polyAdd)

-- ============================================================================
-- 1. SOS Interface
-- ============================================================================

-- | Check if polynomial is SOS using SDP
checkSOS_SDP :: Poly -> Bool
checkSOS_SDP p =
  let 
      -- 1. Determine Monomial Basis
      -- Heuristic: Newton Polytope would be better, but we use half-degree bounding box
      vars = S.toList (getVars p)
      deg = polyDegree p
      halfDeg = (deg + 1) `div` 2
      
      -- Generate basis monomials
      basis = generateBasis vars halfDeg
      n = length basis
      
      -- 2. Construct SDP Constraints
      -- P(x) = v(x)^T Q v(x) = sum Q_ij * m_i(x) * m_j(x)
      -- Group by monomial in P: sum_{i,j : m_i*m_j = m_alpha} Q_ij = c_alpha
      
      targetCoeffs = case p of Poly m -> m
      
      -- Map from ProductMonomial -> [(BasisIndex, BasisIndex)]
      constraintMap = M.fromListWith (++) 
        [ (monomialMul (basis !! i) (basis !! j), [(i,j)]) 
        | i <- [0..n-1], j <- [0..n-1] 
        ]
      
      -- Convert to A_k matrices and b_k
      -- b_k = coeff of monomial m_k in P
      -- A_k has 1s at (i,j) where basis[i]*basis[j] = m_k
      
      constraints = 
        [ (m_k, val, indices)
        | (m_k, val) <- M.toList targetCoeffs
        , let indices = M.findWithDefault [] m_k constraintMap
        ]
        
      -- Check if any target monomial cannot be formed by basis (Quick fail)
      unmatched = any (\(_, _, idxs) -> null idxs) constraints
      
  in 
      not unmatched && unsafePerformIO (runSDPSolver n constraints)

generateBasis :: [String] -> Int -> [Monomial]
generateBasis vars d =
  let -- Generate all monomials with total degree <= d
      go 0 _ = [Monomial M.empty]
      go k [] = [Monomial M.empty]
      go k (v:vs) = 
        [ monomialMul (Monomial (M.singleton v (fromIntegral p))) rest
        | p <- [0..k]
        , rest <- go (k-p) vs
        ]
  in sort $ go d vars

polyDegree :: Poly -> Int
polyDegree (Poly m) = fromIntegral $ maximum (0 : map (\(Monomial vm, _) -> sum (M.elems vm)) (M.toList m))

-- ============================================================================
-- 2. Primal-Dual Interior Point Method (Simplified)
-- ============================================================================

-- Minimize Tr(CX) s.t. Tr(A_i X) = b_i, X >= 0
-- Here C = 0 (Feasibility), or Identity (Minimize Trace)
-- We strictly want Feasibility. 
-- But standard form usually requires C. Let's use C=I to find "smallest" SOS.

type Constraint = (Monomial, Rational, [(Int, Int)])

runSDPSolver :: Int -> [Constraint] -> IO Bool
runSDPSolver n constrs = do
  -- Setup matrices
  -- X (Primal), S (Dual Slack), y (Dual vars)
  -- Initial point: X = I, S = I, y = 0
  
  -- We use dense UNBOXED arrays for X, S
  xMat <- newArray ((0,0), (n-1,n-1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  sMat <- newArray ((0,0), (n-1,n-1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  
  -- Initialize to Identity * big M? Or just I.
  forM_ [0..n-1] $ \i -> do
    writeArray xMat (i,i) 100.0 -- Start deep inside cone
    writeArray sMat (i,i) 100.0
    
  -- Main Loop (Predictor-Corrector style simplified to Short Step)
  solveLoop 0 xMat sMat (replicate (length constrs) 0.0) constrs n

solveLoop :: Int -> IOUArray (Int,Int) Double -> IOUArray (Int,Int) Double -> [Double] -> [Constraint] -> Int -> IO Bool
solveLoop iter xMat sMat yVec constrs n
  | iter > 100 = return False -- Timeout (increased to 100)
  | otherwise = do
      -- 1. Compute Residuals
      -- r_P = b - A(X)
      -- r_D = C - A^T(y) - S  (Assume C=0)
      -- r_C = mu I - X S
      
      -- Check convergence (Primal/Dual feasibility + Gap)
      pRes <- primalResidual xMat constrs
      dRes <- dualResidual sMat yVec constrs n
      
      if pRes < 1e-4 && dRes < 1e-4 
        then return True -- Feasible!
        else do
          -- Compute Newton Step
          -- Simplified: Just return False for now to verify integration.
          return False

-- | Primal Residual: norm(b - A(X))
primalResidual :: IOUArray (Int,Int) Double -> [Constraint] -> IO Double
primalResidual xMat constrs = do
  diffs <- mapM checkConstr constrs
  return $ sqrt $ sum $ map (^ (2::Int)) diffs
  where
    checkConstr (_, val, idxs) = do
      lhs <- sum <$> mapM (\(i,j) -> readArray xMat (i,j)) idxs
      return (lhs - fromRational val)

-- | Dual Residual: norm(C - A^T(y) - S)
-- Uses C = Identity matrix
dualResidual :: IOUArray (Int,Int) Double -> [Double] -> [Constraint] -> Int -> IO Double
dualResidual sMat yVec constrs n = do
  -- Diff = S - C + sum y_k A_k
  diffMat <- newArray ((0,0), (n-1,n-1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  
  -- Initialize with S - I
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-1] $ \j -> do
      sVal <- readArray sMat (i,j)
      let cVal = if i == j then 1.0 else 0.0
      writeArray diffMat (i,j) (sVal - cVal)
      
  -- Add A^T(y) contribution
  forM_ (zip yVec constrs) $ \(y, (_, _, idxs)) -> do
    forM_ idxs $ \(i,j) -> do
      curr <- readArray diffMat (i,j)
      writeArray diffMat (i,j) (curr + y)
      
  elems <- getElems diffMat
  return $ sqrt $ sum $ map (^ (2::Int)) elems

-- | Duality Gap: Tr(X S)
dualityGap :: IOUArray (Int,Int) Double -> IOUArray (Int,Int) Double -> Int -> IO Double
dualityGap xMat sMat n = do
  prods <- sequence [ do
                        x <- readArray xMat (i,j)
                        s <- readArray sMat (j,i)
                        return (x * s)
                    | i <- [0..n-1], j <- [0..n-1] 
                    ]
  return $ sum prods