{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal Facial Reduction for Degenerate SDPs
--
-- This module implements a single-pass facial reduction algorithm based on
-- eigenvalue detection. When an SDP has its optimal value at the boundary
-- of the PSD cone (e.g., minimum = 0 for Ono's inequality), the Jacobian
-- becomes singular. Facial reduction detects near-zero eigenvalues and
-- projects the problem onto a face where strict feasibility holds.
--
-- Reference: Borwein & Wolkowicz (1981), "Facial Reduction for a Cone-Convex
-- Programming Problem"
-- Reference: Cheung, Schurr & Wolkowicz (2013), "Preprocessing and Regularization
-- for Degenerate Semidefinite Programs"

module Positivity.FacialReduction
  ( -- * Degeneracy Detection
    DegeneracyInfo(..)
  , detectDegeneracy
  , approximateEigenvalues
    -- * Facial Reduction
  , FacialReductionResult(..)
  , applyFacialReduction
  , projectToFace
    -- * Regularized Cholesky
  , choleskyRegularized
  , choleskyWithBackoff
  , solveCholeskySafe
    -- * Utilities
  , powerIteration
  , matrixNorm
  , vectorNorm
  ) where

import Data.Number.BigFloat
import Data.Array.IO
import Control.Monad (forM, forM_, when, foldM)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- =============================================
-- Types (reusing conventions from SDP.hs)
-- =============================================

type Scalar = BigFloat Prec50
type Matrix = IOArray (Int, Int) Scalar
type Vector = IOArray Int Scalar

-- | Degeneracy information from eigenvalue analysis
data DegeneracyInfo = DegeneracyInfo
  { diIsDegenerate    :: Bool
  , diNullSpaceDim    :: Int
  , diSmallestEigen   :: Scalar
  , diLargestEigen    :: Scalar
  , diNullSpaceVecs   :: [Vector]  -- Approximate null space basis
  }

-- | Result of facial reduction preprocessing
data FacialReductionResult
  = NotDegenerate              -- Problem is strictly feasible
  | ReducedProblem             -- Successfully reduced to smaller face
      { frProjection :: Matrix -- V matrix for projection
      , frReducedDim :: Int    -- Dimension of reduced problem
      }
  | HighlyDegenerate           -- Too many null directions
      { frNullDim :: Int }

-- =============================================
-- 1. Power Iteration for Eigenvalue Estimation
-- =============================================

-- | Compute Frobenius norm of a matrix
matrixNorm :: Matrix -> IO Scalar
matrixNorm m = do
  ((r0, c0), (r1, c1)) <- getBounds m
  let indices = [(i, j) | i <- [r0..r1], j <- [c0..c1]]
  sumSq <- foldM (\acc idx -> do
    v <- readArray m idx
    return (acc + v * v)) 0 indices
  return (sqrt sumSq)

-- | Compute L2 norm of a vector
vectorNorm :: Vector -> IO Scalar
vectorNorm v = do
  (lo, hi) <- getBounds v
  sumSq <- foldM (\acc i -> do
    x <- readArray v i
    return (acc + x * x)) 0 [lo..hi]
  return (sqrt sumSq)

-- | Normalize a vector in place, return the norm
normalizeVector :: Vector -> IO Scalar
normalizeVector v = do
  norm <- vectorNorm v
  when (norm > 1e-50) $ do
    (lo, hi) <- getBounds v
    forM_ [lo..hi] $ \i -> do
      x <- readArray v i
      writeArray v i (x / norm)
  return norm

-- | Matrix-vector multiplication: y = A * x
matVecMul :: Matrix -> Vector -> IO Vector
matVecMul m x = do
  ((r0, c0), (r1, c1)) <- getBounds m
  let n = r1 - r0 + 1
  y <- newArray (0, n-1) 0
  forM_ [0..n-1] $ \i -> do
    sum' <- foldM (\acc j -> do
      mij <- readArray m (i, j)
      xj <- readArray x j
      return (acc + mij * xj)) 0 [0..n-1]
    writeArray y i sum'
  return y

-- | Power iteration to find dominant eigenvalue and eigenvector
--
-- Returns (eigenvalue, eigenvector) after numIters iterations
powerIteration :: Matrix -> Int -> IO (Scalar, Vector)
powerIteration m numIters = do
  ((0, 0), (nMinus1, _)) <- getBounds m
  let n = nMinus1 + 1

  -- Initialize with random-ish vector (1, 1/2, 1/3, ...)
  v <- newArray (0, n-1) 0
  forM_ [0..n-1] $ \i -> writeArray v i (1 / fromIntegral (i + 1))
  _ <- normalizeVector v

  let iterate' iter prevLambda
        | iter >= numIters = do
            w <- matVecMul m v
            lambda <- dotProduct v w
            return (lambda, v)
        | otherwise = do
            w <- matVecMul m v
            lambda <- dotProduct v w
            -- Copy w to v and normalize
            (lo, hi) <- getBounds w
            forM_ [lo..hi] $ \i -> do
              val <- readArray w i
              writeArray v i val
            _ <- normalizeVector v
            -- Check convergence
            let diff = abs (lambda - prevLambda)
            if diff < 1e-20
              then return (lambda, v)
              else iterate' (iter + 1) lambda

  iterate' 0 0

-- | Dot product of two vectors
dotProduct :: Vector -> Vector -> IO Scalar
dotProduct v1 v2 = do
  (lo, hi) <- getBounds v1
  foldM (\acc i -> do
    x <- readArray v1 i
    y <- readArray v2 i
    return (acc + x * y)) 0 [lo..hi]

-- | Inverse power iteration to find smallest eigenvalue
-- Uses shifted inverse: (A - sigma*I)^{-1}
-- Note: This function is provided for completeness but we use
-- inversePowerIterationSafe which handles singularity more robustly.
inversePowerIteration :: Matrix -> Scalar -> Int -> IO (Scalar, Vector)
inversePowerIteration m shift numIters = do
  ((0, 0), (nMinus1, _)) <- getBounds m
  let n = nMinus1 + 1

  -- Create shifted matrix A - sigma*I
  shifted <- newArray ((0, 0), (nMinus1, nMinus1)) 0 :: IO Matrix
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-1] $ \j -> do
      val <- readArray m (i, j)
      let shiftVal = if i == j then val - shift else val
      writeArray shifted (i, j) shiftVal

  -- Try Cholesky factorization of shifted matrix
  -- If it fails, the shift is too close to an eigenvalue
  cholResult <- choleskyRegularized shifted 1e-15
  case cholResult of
    Nothing -> do
      -- Shift is near an eigenvalue - return shift as the eigenvalue
      -- and create a dummy eigenvector
      dummyV <- newArray (0, n-1) 0 :: IO Vector
      forM_ [0..n-1] $ \i -> writeArray dummyV i (1 / fromIntegral (i + 1))
      return (shift, dummyV)
    Just l -> do
      -- Initialize eigenvector
      v <- newArray (0, n-1) 0 :: IO Vector
      forM_ [0..n-1] $ \i -> writeArray v i (1 / fromIntegral (i + 1))
      _ <- normalizeVector v

      let iterate' iter prevLambda
            | iter >= numIters = do
                -- Rayleigh quotient
                w <- matVecMul m v
                lambda <- dotProduct v w
                return (lambda, v)
            | otherwise = do
                -- Solve (A - sigma*I) w = v using Cholesky
                w <- solveCholeskySafe l v
                _ <- normalizeVector w
                -- Copy back
                (lo, hi) <- getBounds w
                forM_ [lo..hi] $ \i' -> do
                  val <- readArray w i'
                  writeArray v i' val
                -- Rayleigh quotient for eigenvalue estimate
                mv <- matVecMul m v
                lambda <- dotProduct v mv
                let diff = abs (lambda - prevLambda)
                if diff < 1e-20
                  then return (lambda, v)
                  else iterate' (iter + 1) lambda

      iterate' 0 0

-- | Approximate all eigenvalues using deflation
-- Returns eigenvalues sorted in descending order
approximateEigenvalues :: Matrix -> IO [Scalar]
approximateEigenvalues m = do
  ((0, 0), (nMinus1, _)) <- getBounds m
  let n = nMinus1 + 1

  -- For efficiency, we only compute largest and smallest few eigenvalues
  -- This is sufficient for degeneracy detection

  -- Largest eigenvalue via power iteration
  (lambdaMax, _) <- powerIteration m 50

  -- Smallest eigenvalue via inverse iteration with shift 0
  -- Actually, for PSD matrices, we use a small positive shift
  (lambdaMin, _) <- inversePowerIterationSafe m 1e-12 30

  -- Return bounds (for large matrices, we don't compute all eigenvalues)
  if n <= 5
    then return [lambdaMax, lambdaMin]  -- Small matrix, approximation OK
    else return [lambdaMax, lambdaMin]

-- | Safe inverse power iteration (handles singular matrices)
inversePowerIterationSafe :: Matrix -> Scalar -> Int -> IO (Scalar, Vector)
inversePowerIterationSafe m shift numIters = do
  ((0, 0), (nMinus1, _)) <- getBounds m
  let n = nMinus1 + 1

  -- Initialize eigenvector
  v <- newArray (0, n-1) 0
  forM_ [0..n-1] $ \i -> writeArray v i (1 / fromIntegral (i + 1))
  _ <- normalizeVector v

  let iterate' iter prevLambda
        | iter >= numIters = do
            w <- matVecMul m v
            lambda <- dotProduct v w
            return (lambda, v)
        | otherwise = do
            -- Solve (A + shift*I) w = v
            -- We add a regularization term to avoid singularity
            w <- solveRegularizedSystem m shift v
            _ <- normalizeVector w
            -- Copy back
            (lo, hi) <- getBounds w
            forM_ [lo..hi] $ \i -> do
              val <- readArray w i
              writeArray v i val
            -- Rayleigh quotient
            mv <- matVecMul m v
            lambda <- dotProduct v mv
            let diff = abs (lambda - prevLambda)
            if diff < 1e-20
              then return (lambda, v)
              else iterate' (iter + 1) lambda

  iterate' 0 0

-- | Solve (A + delta*I) x = b with regularization
solveRegularizedSystem :: Matrix -> Scalar -> Vector -> IO Vector
solveRegularizedSystem a delta b = do
  ((0, 0), (nMinus1, _)) <- getBounds a
  let n = nMinus1 + 1

  -- Create regularized matrix
  aReg <- newArray ((0, 0), (nMinus1, nMinus1)) 0
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-1] $ \j -> do
      val <- readArray a (i, j)
      let regVal = if i == j then val + delta else val
      writeArray aReg (i, j) regVal

  -- Cholesky with backoff
  result <- choleskyWithBackoff aReg
  case result of
    Nothing -> do
      -- Fallback: return b (identity solve)
      bCopy <- newArray (0, n-1) 0
      forM_ [0..n-1] $ \i -> do
        val <- readArray b i
        writeArray bCopy i val
      return bCopy
    Just (l, _) -> solveCholeskySafe l b

-- =============================================
-- 2. Degeneracy Detection
-- =============================================

-- | Detect degeneracy by finding near-zero eigenvalues of Schur complement
--
-- A problem is degenerate if the Schur complement (which determines the
-- Newton direction) has eigenvalues close to zero. This indicates that
-- the optimal solution lies on the boundary of the PSD cone.
detectDegeneracy :: Matrix -> IO DegeneracyInfo
detectDegeneracy schurComplement = do
  ((0, 0), (nMinus1, _)) <- getBounds schurComplement
  let n = nMinus1 + 1

  -- Compute largest and smallest eigenvalues
  (lambdaMax, _) <- powerIteration schurComplement 50
  (lambdaMin, vMin) <- inversePowerIterationSafe schurComplement 1e-14 30

  -- Threshold for near-zero eigenvalue
  -- Use relative threshold based on largest eigenvalue
  let threshold = 1e-10 * max 1 (abs lambdaMax)
      isDegenerate = abs lambdaMin < threshold

  nullVecs <- if isDegenerate
    then return [vMin]  -- Single null vector for minimal approach
    else return []

  return DegeneracyInfo
    { diIsDegenerate = isDegenerate
    , diNullSpaceDim = if isDegenerate then 1 else 0
    , diSmallestEigen = lambdaMin
    , diLargestEigen = lambdaMax
    , diNullSpaceVecs = nullVecs
    }

-- =============================================
-- 3. Facial Reduction Preprocessing
-- =============================================

-- | Apply facial reduction to a degenerate SDP
--
-- If the problem is degenerate, we project it onto a face of the PSD cone
-- where strict feasibility holds. This is done by:
-- 1. Finding the null space of the Schur complement
-- 2. Computing the orthogonal complement V
-- 3. Projecting all matrices: A'_i = V^T A_i V
applyFacialReduction :: Matrix -> IO FacialReductionResult
applyFacialReduction schurComplement = do
  info <- detectDegeneracy schurComplement

  if not (diIsDegenerate info)
    then return NotDegenerate
    else do
      ((0, 0), (nMinus1, _)) <- getBounds schurComplement
      let n = nMinus1 + 1
          nullDim = diNullSpaceDim info

      if nullDim > n `div` 2
        then return (HighlyDegenerate nullDim)
        else do
          -- Compute orthogonal complement of null space
          let reducedDim = n - nullDim
          vProj <- computeOrthogonalComplement n (diNullSpaceVecs info)
          return (ReducedProblem vProj reducedDim)

-- | Compute orthogonal complement of given vectors using Gram-Schmidt
computeOrthogonalComplement :: Int -> [Vector] -> IO Matrix
computeOrthogonalComplement n nullVecs = do
  -- Start with identity basis
  basis <- forM [0..n-1] $ \i -> do
    v <- newArray (0, n-1) 0
    writeArray v i 1
    return v

  -- Orthogonalize each basis vector against null space
  orthoBasis <- forM basis $ \v -> do
    -- Subtract projections onto null vectors
    forM_ nullVecs $ \nullV -> do
      proj <- dotProduct v nullV
      (lo, hi) <- getBounds v
      forM_ [lo..hi] $ \i -> do
        nv <- readArray nullV i
        val <- readArray v i
        writeArray v i (val - proj * nv)
    -- Normalize
    _ <- normalizeVector v
    return v

  -- Filter out near-zero vectors (those that were in null space)
  let nonZeroThreshold = 1e-12
  validBasis <- filterM (\v -> do
    norm <- vectorNorm v
    return (norm > nonZeroThreshold)) orthoBasis

  let k = length validBasis

  -- Create projection matrix V (n x k)
  vMat <- newArray ((0, 0), (n-1, k-1)) 0
  forM_ (zip [0..] validBasis) $ \(j, v) ->
    forM_ [0..n-1] $ \i -> do
      val <- readArray v i
      writeArray vMat (i, j) val

  return vMat

-- | Project a matrix through the face: A' = V^T A V
projectToFace :: Matrix -> Matrix -> IO Matrix
projectToFace v a = do
  ((0, 0), (n1, k1)) <- getBounds v
  let n = n1 + 1
      k = k1 + 1

  -- Compute V^T A
  vt <- transposeMatrix v
  vta <- matMulGen vt a

  -- Compute (V^T A) V
  matMulGen vta v

-- | Helper: transpose a matrix
transposeMatrix :: Matrix -> IO Matrix
transposeMatrix m = do
  ((r0, c0), (r1, c1)) <- getBounds m
  let rows = r1 - r0 + 1
      cols = c1 - c0 + 1
  t <- newArray ((0, 0), (cols-1, rows-1)) 0
  forM_ [0..rows-1] $ \i ->
    forM_ [0..cols-1] $ \j -> do
      val <- readArray m (i, j)
      writeArray t (j, i) val
  return t

-- | Helper: general matrix multiplication
matMulGen :: Matrix -> Matrix -> IO Matrix
matMulGen a b = do
  ((0, 0), (r1, c1)) <- getBounds a
  ((0, 0), (r1', c1')) <- getBounds b
  let rowsA = r1 + 1
      colsA = c1 + 1
      rowsB = r1' + 1
      colsB = c1' + 1

  if colsA /= rowsB
    then error $ "Matrix dimension mismatch: " ++ show (colsA, rowsB)
    else do
      c <- newArray ((0, 0), (rowsA-1, colsB-1)) 0
      forM_ [0..rowsA-1] $ \i ->
        forM_ [0..colsB-1] $ \j -> do
          sumVal <- foldM (\acc k' -> do
            aik <- readArray a (i, k')
            bkj <- readArray b (k', j)
            return (acc + aik * bkj)) 0 [0..colsA-1]
          writeArray c (i, j) sumVal
      return c

-- =============================================
-- 4. Regularized Cholesky Decomposition
-- =============================================

-- | Cholesky decomposition with regularization
--
-- Adds delta * I to the matrix before factoring to handle
-- near-singular matrices. Returns Nothing if still fails.
choleskyRegularized :: Matrix -> Scalar -> IO (Maybe Matrix)
choleskyRegularized a delta = do
  ((0, 0), (nMinus1, _)) <- getBounds a
  let n = nMinus1 + 1

  -- Create regularized copy: A + delta*I
  aReg <- newArray ((0, 0), (nMinus1, nMinus1)) 0 :: IO Matrix
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-1] $ \j -> do
      val <- readArray a (i, j)
      let regVal = if i == j then val + delta else val
      writeArray aReg (i, j) regVal

  -- Standard Cholesky on regularized matrix
  l <- newArray ((0, 0), (nMinus1, nMinus1)) 0 :: IO Matrix

  let iter i
        | i == n = return (Just l)
        | otherwise = do
            let colLoop j
                  | j == i = do
                      -- Diagonal element
                      sumSq <- foldM (\acc k -> do
                        lik <- readArray l (i, k)
                        return (acc + lik * lik)) 0 [0..i-1]
                      aii <- readArray aReg (i, i)
                      let val = aii - sumSq
                      if val <= 0
                        then return False
                        else do
                          writeArray l (i, i) (sqrt val)
                          return True
                  | otherwise = do
                      -- Off-diagonal element
                      sumVal <- foldM (\acc k -> do
                        lik <- readArray l (i, k)
                        ljk <- readArray l (j, k)
                        return (acc + lik * ljk)) 0 [0..j-1]
                      aij <- readArray aReg (i, j)
                      ljj <- readArray l (j, j)
                      if ljj == 0
                        then return False
                        else do
                          writeArray l (i, j) ((aij - sumVal) / ljj)
                          colLoop (j + 1)

            success <- colLoop 0
            if success then iter (i + 1) else return Nothing

  iter 0

-- | Cholesky with exponential backoff on regularization
--
-- Tries increasingly larger regularization values until success:
-- 1e-15, 1e-12, 1e-9, 1e-6, 1e-3
choleskyWithBackoff :: Matrix -> IO (Maybe (Matrix, Scalar))
choleskyWithBackoff a = tryDeltas deltas
  where
    deltas = [1e-15, 1e-12, 1e-9, 1e-6, 1e-3]

    tryDeltas [] = return Nothing
    tryDeltas (d:ds) = do
      result <- choleskyRegularized a d
      case result of
        Just l -> return (Just (l, d))
        Nothing -> tryDeltas ds

-- | Solve Cholesky system safely (handles potential issues)
solveCholeskySafe :: Matrix -> Vector -> IO Vector
solveCholeskySafe l b = do
  ((0, 0), (nMinus1, _)) <- getBounds l
  let n = nMinus1 + 1

  -- Forward substitution: L y = b
  y <- newArray (0, n-1) 0 :: IO Vector
  forM_ [0..n-1] $ \i -> do
    bi <- readArray b i
    sumVal <- foldM (\acc k -> do
      lik <- readArray l (i, k)
      yk <- readArray y k
      return (acc + lik * yk)) 0 [0..i-1]
    lii <- readArray l (i, i)
    let val = if abs lii < 1e-50 then 0 else (bi - sumVal) / lii
    writeArray y i val

  -- Backward substitution: L^T x = y
  x <- newArray (0, n-1) 0 :: IO Vector
  forM_ (reverse [0..n-1]) $ \i -> do
    yi <- readArray y i
    sumVal <- foldM (\acc k -> do
      lki <- readArray l (k, i)
      xk <- readArray x k
      return (acc + lki * xk)) 0 [i+1..n-1]
    lii <- readArray l (i, i)
    let val = if abs lii < 1e-50 then 0 else (yi - sumVal) / lii
    writeArray x i val

  return x

-- =============================================
-- 5. Utility: FilterM for IO
-- =============================================

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  rest <- filterM p xs
  return (if b then x : rest else rest)
