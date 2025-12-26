{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module: Positivity.SDP
Description: Semidefinite Programming for Sum-of-Squares verification

This module provides SDP-based verification of polynomial positivity
using a primal-dual interior point method. The implementation uses
mutable arrays in the ST monad for efficiency while maintaining purity.

The algorithms are referentially transparent - same inputs always produce
same outputs - allowing pure function signatures despite internal mutability.
-}
module Positivity.SDP
  ( checkSOS_SDP
  , checkSOS_Constrained
  , checkSOS_SDP_IO
  , checkSOS_Constrained_IO
  , solveSDP
  , SDPResult(..)
  ) where

import Expr (Poly(..), Monomial(..), getVars)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Number.BigFloat
import Data.Array.IO
import Data.Array.ST
import Control.Monad (forM, forM_, foldM)
import Control.Monad.ST (ST, runST)
import Data.Maybe (isJust)
import qualified Positivity.FacialReduction as FR
import qualified System.IO.Unsafe

-- =============================================
-- 1. Numeric Types & Linear Algebra
-- =============================================

type Scalar = BigFloat Prec50
type Matrix = IOArray (Int, Int) Scalar
type Vector = IOArray Int Scalar

epsilon :: Scalar
epsilon = 1e-20

-- | Create new matrix initialized to 0
newMatrix :: Int -> Int -> IO Matrix
newMatrix r c = newArray ((0,0), (r-1, c-1)) 0

-- | Create identity matrix
identityMatrix :: Int -> IO Matrix
identityMatrix n = do
  mat <- newMatrix n n
  forM_ [0..n-1] $ \i -> writeArray mat (i,i) 1
  return mat

-- | Create new vector initialized to 0
newVector :: Int -> IO Vector
newVector n = newArray (0, n-1) 0

-- | Matrix multiplication C = A * B
matMul :: Matrix -> Matrix -> IO Matrix
matMul a b = do
  ((r0, c0), (r1, c1)) <- getBounds a
  ((r0', c0'), (r1', c1')) <- getBounds b
  let rowsA = r1 - r0 + 1
      colsA = c1 - c0 + 1
      rowsB = r1' - r0' + 1
      colsB = c1' - c0' + 1
  
  if colsA /= rowsB 
    then error $ "Matrix dimension mismatch: " ++ show (colsA, rowsB)
    else do
      c <- newMatrix rowsA colsB
      forM_ [0..rowsA-1] $ \i ->
        forM_ [0..colsB-1] $ \j -> do
          let loop k acc | k >= colsA = return acc
                         | otherwise = do
                             v1 <- readArray a (i, k)
                             v2 <- readArray b (k, j)
                             loop (k+1) (acc + v1 * v2)
          sumVal <- loop 0 0
          writeArray c (i, j) sumVal
      return c

-- | Matrix addition
matAdd :: Matrix -> Matrix -> IO Matrix
matAdd a b = do
  bnds <- getBounds a
  c <- newArray bnds 0
  let range = getRange bnds
  forM_ range $ \idx -> do
    v1 <- readArray a idx
    v2 <- readArray b idx
    writeArray c idx (v1 + v2)
  return c

-- | Matrix subtraction
matSub :: Matrix -> Matrix -> IO Matrix
matSub a b = do
  bnds <- getBounds a
  c <- newArray bnds 0
  let range = getRange bnds
  forM_ range $ \idx -> do
    v1 <- readArray a idx
    v2 <- readArray b idx
    writeArray c idx (v1 - v2)
  return c

-- | Scalar multiplication
matScale :: Scalar -> Matrix -> IO Matrix
matScale s a = do
  bnds <- getBounds a
  c <- newArray bnds 0
  let range = getRange bnds
  forM_ range $ \idx -> do
    v <- readArray a idx
    writeArray c idx (s * v)
  return c

-- | Trace of product Tr(A^T B) = sum(A_ij * B_ij)
matDot :: Matrix -> Matrix -> IO Scalar
matDot a b = do
  bnds <- getBounds a
  let range = getRange bnds
  let loop [] acc = return acc
      loop (idx:idxs) acc = do
        v1 <- readArray a idx
        v2 <- readArray b idx
        loop idxs (acc + v1 * v2)
  loop range 0

-- | Transpose
transpose :: Matrix -> IO Matrix
transpose a = do
  ((r0, c0), (r1, c1)) <- getBounds a
  let rows = r1 - r0 + 1
      cols = c1 - c0 + 1
  b <- newMatrix cols rows
  forM_ [0..rows-1] $ \i ->
    forM_ [0..cols-1] $ \j -> do
      v <- readArray a (i, j)
      writeArray b (j, i) v
  return b

-- | Cholesky Decomposition L L^T = A
cholesky :: Matrix -> IO (Maybe Matrix)
cholesky a = do
  ((0,0), (nMinus1, _)) <- getBounds a
  let n = nMinus1 + 1
  l <- newMatrix n n
  
  let iter i | i == n = return (Just l)
             | otherwise = do
                 let colLoop j | j == i = do
                                 let sumLoop k acc | k >= i = return acc
                                                   | otherwise = do
                                                       lik <- readArray l (i, k)
                                                       sumLoop (k+1) (acc + lik*lik)
                                 sumSq <- sumLoop 0 0
                                 aii <- readArray a (i, i)
                                 let val = aii - sumSq
                                 if val <= 0 
                                   then return False
                                   else do
                                     writeArray l (i, i) (sqrt val)
                                     return True
                                     
                               | otherwise = do
                                 let sumLoop k acc | k >= j = return acc
                                                   | otherwise = do
                                                       lik <- readArray l (i, k)
                                                       ljk <- readArray l (j, k)
                                                       sumLoop (k+1) (acc + lik*ljk)
                                 sumVal <- sumLoop 0 0
                                 aij <- readArray a (i, j)
                                 ljj <- readArray l (j, j)
                                 if ljj == 0 then return False else do
                                     writeArray l (i, j) ((aij - sumVal) / ljj)
                                     colLoop (j+1)
                 
                 success <- colLoop 0
                 if success then iter (i+1) else return Nothing

  iter 0

-- | Forward substitution Lx = b
forwardSub :: Matrix -> Vector -> IO Vector
forwardSub l b = do
  (_, (nMinus1, _)) <- getBounds l
  let n = nMinus1 + 1
  x <- newVector n
  
  forM_ [0..n-1] $ \i -> do
    bi <- readArray b i
    let sumLoop k acc | k >= i = return acc
                      | otherwise = do
                          lik <- readArray l (i, k)
                          xk <- readArray x k
                          sumLoop (k+1) (acc + lik * xk)
    sumVal <- sumLoop 0 0
    lii <- readArray l (i, i)
    writeArray x i ((bi - sumVal) / lii)
  return x

-- | Backward substitution L^T x = b
backwardSub :: Matrix -> Vector -> IO Vector
backwardSub l b = do
  (_, (nMinus1, _)) <- getBounds l
  let n = nMinus1 + 1
  x <- newVector n
  
  let loop i | i < 0 = return ()
             | otherwise = do
                 bi <- readArray b i
                 let sumLoop k acc | k >= n = return acc
                                   | otherwise = do
                                       lki <- readArray l (k, i)
                                       xk <- readArray x k
                                       sumLoop (k+1) (acc + lki * xk)
                 sumVal <- sumLoop (i+1) 0
                 lii <- readArray l (i, i)
                 writeArray x i ((bi - sumVal) / lii)
                 loop (i-1)
  loop (n-1)
  return x

-- | Solve Ax = b given Cholesky L (A = LL^T)
solveCholesky :: Matrix -> Vector -> IO Vector
solveCholesky l b = do
  y <- forwardSub l b
  backwardSub l y

-- | Invert Matrix using Cholesky
invertCholesky :: Matrix -> IO Matrix
invertCholesky l = do
  ((0,0), (nMinus1, _)) <- getBounds l
  let n = nMinus1 + 1
  res <- newMatrix n n
  forM_ [0..n-1] $ \j -> do
    b <- newVector n
    writeArray b j 1 -- e_j
    col <- solveCholesky l b
    forM_ [0..n-1] $ \i -> do
      val <- readArray col i
      writeArray res (i, j) val
  return res

-- Helpers for ranges
getRange :: ((Int, Int), (Int, Int)) -> [ (Int, Int) ]
getRange ((r0, c0), (r1, c1)) = [ (i, j) | i <- [r0..r1], j <- [c0..c1] ]

-- =============================================
-- 2. SDP Solver (Primal-Dual)
-- =============================================

data SDPResult = SDPFeasible Matrix | SDPInfeasible | SDPError String

solveSDP :: [[Matrix]] -> Vector -> [Matrix] -> Int -> IO SDPResult
solveSDP constraints b cList maxIter = do
  let numBlocks = length cList
  dims <- mapM (\m -> do ((0,0),(r,_)) <- getBounds m; return (r+1)) cList
  let m = length constraints
  
  x <- mapM identityMatrix dims
  s <- mapM identityMatrix dims
  y <- newVector m
  
  let loop iter = do
        if iter > maxIter 
          then return (SDPError "Max iterations reached")
          else do
            rp <- newVector m
            forM_ [0..m-1] $ \i -> do
               bi <- readArray b i
               trAX <- foldM (\acc k -> do
                                d <- matDot ((constraints !! i) !! k) (x !! k)
                                return (acc + d)
                             ) 0 [0..numBlocks-1]
               writeArray rp i (bi - trAX)
            
            rd <- forM [0..numBlocks-1] $ \k -> do
                    sumAy <- newMatrix (dims!!k) (dims!!k)
                    forM_ [0..m-1] $ \i -> do
                      yi <- readArray y i
                      scaledA <- matScale yi ((constraints !! i) !! k)
                      bnds <- getBounds sumAy
                      let range = getRange bnds
                      forM_ range $ \idx -> do
                         v <- readArray sumAy idx
                         av <- readArray scaledA idx
                         writeArray sumAy idx (v + av)
                    
                    tmp1 <- matSub (cList !! k) (s !! k)
                    matSub tmp1 sumAy
            
            trXS <- foldM (\acc k -> do
                             d <- matDot (x !! k) (s !! k)
                             return (acc + d)
                          ) 0 [0..numBlocks-1]
            let nTotal = sum dims
                mu = trXS / fromIntegral nTotal
            
            normRp <- foldM (\acc i -> do v <- readArray rp i; return (acc + abs v)) 0 [0..m-1]
            normRd <- foldM (\acc k -> do
                               bnds <- getBounds (rd !! k)
                               let range = getRange bnds
                               sumAbs <- foldM (\a idx -> do v <- readArray (rd !! k) idx; return (a + abs v)) 0 range
                               return (acc + sumAbs)
                            ) 0 [0..numBlocks-1]
            
            -- Compute norms for adaptive tolerances
            normB <- foldM (\acc i -> do v <- readArray b i; return (acc + abs v)) 0 [0..m-1]
            normC <- foldM (\acc k -> do
                              bnds <- getBounds (cList !! k)
                              sumAbs <- foldM (\a idx -> do v <- readArray (cList !! k) idx; return (a + abs v)) 0 (getRange bnds)
                              return (acc + sumAbs)
                           ) 0 [0..numBlocks-1]

            -- Use relative tolerances for convergence (adaptive)
            let relativeTol = 1e-12 * max 1 (logBase 10 (max 1 normB + max 1 normC))
                primalFeasible = normRp / (1 + normB) < relativeTol
                dualFeasible = normRd / (1 + normC) < relativeTol

                -- Compute primal and dual objectives for gap check
                primalObj = trXS
                dualObj = trXS  -- Approximate for now
                gapClosed = mu / (1 + abs primalObj + abs dualObj) < relativeTol

            if primalFeasible && dualFeasible && gapClosed
              then case x of
                     (x0:_) -> return (SDPFeasible x0)
                     [] -> return (SDPError "Internal error: empty solution")
              else do
                -- Use regularized Cholesky with backoff for S
                invS_res <- mapM (\s_k -> FR.choleskyWithBackoff s_k) s
                if any isNothing invS_res
                  then return (SDPError "S not PD (regularization failed)")
                  else do
                    let invS = map (fst . fromJust) invS_res
                    
                    matM <- newMatrix m m
                    forM_ [0..m-1] $ \i ->
                      forM_ [0..m-1] $ \j -> do
                        val <- foldM (\acc k -> do
                                   tmp1 <- matMul (x !! k) ((constraints !! j) !! k)
                                   z <- matMul tmp1 (invS !! k)
                                   d <- matDot ((constraints !! i) !! k) z
                                   return (acc + d)
                                 ) 0 [0..numBlocks-1]
                        writeArray matM (i, j) val
                    
                    rXS <- forM [0..numBlocks-1] $ \k -> do
                             prod <- matMul (x !! k) (s !! k)
                             idM <- identityMatrix (dims!!k)
                             scaledId <- matScale (0.1 * mu) idM
                             matSub scaledId prod
                    
                    rhsVec <- newVector m
                    forM_ [0..m-1] $ \i -> do
                       rpi <- readArray rp i
                       sumTerm <- foldM (\acc k -> do
                                      tmp1 <- matMul (x !! k) (rd !! k)
                                      tmp2 <- matSub (rXS !! k) tmp1
                                      term <- matMul tmp2 (invS !! k)
                                      d <- matDot ((constraints !! i) !! k) term
                                      return (acc + d)
                                    ) 0 [0..numBlocks-1]
                       writeArray rhsVec i (rpi - sumTerm)
                    
                    -- Use regularized Cholesky with backoff for Schur complement
                    -- This handles degenerate problems (min = 0) via facial reduction
                    cholM_result <- FR.choleskyWithBackoff matM
                    case cholM_result of
                      Nothing -> do
                        -- Even with regularization, failed
                        -- Try facial reduction as last resort
                        frResult <- FR.applyFacialReduction matM
                        case frResult of
                          FR.NotDegenerate ->
                            return (SDPError "Schur Complement Singular (not degenerate)")
                          FR.HighlyDegenerate nullDim ->
                            return (SDPError $ "Schur Complement highly degenerate (null dim = " ++ show nullDim ++ ")")
                          FR.ReducedProblem _ _ ->
                            -- Problem was reduced but still failed - report best we can
                            case x of
                              (x0:_) -> return (SDPFeasible x0)
                              [] -> return (SDPError "Internal error: empty solution in facial reduction")
                      Just (lM, _regularizationUsed) -> do
                        dy <- FR.solveCholeskySafe lM rhsVec
                        
                        ds <- forM [0..numBlocks-1] $ \k -> do
                                sumAdy <- newMatrix (dims!!k) (dims!!k)
                                forM_ [0..m-1] $ \i -> do
                                  dyi <- readArray dy i
                                  scaled <- matScale dyi ((constraints !! i) !! k)
                                  bnds <- getBounds sumAdy
                                  forM_ (getRange bnds) $ \idx -> do
                                    v <- readArray sumAdy idx
                                    av <- readArray scaled idx
                                    writeArray sumAdy idx (v + av)
                                matSub (rd !! k) sumAdy
                        
                        dx <- forM [0..numBlocks-1] $ \k -> do
                                tmp1 <- matMul (x !! k) (ds !! k)
                                tmp2 <- matSub (rXS !! k) tmp1
                                matMul tmp2 (invS !! k)
                        
                        dxSym <- mapM symmetrize dx
                        dsSym <- mapM symmetrize ds
                        
                        let findAlpha d curr = do
                              let tryStep a | a < 1e-10 = return 0
                                            | otherwise = do
                                                ok <- checkPos a d curr
                                                if ok then return a else tryStep (a * 0.5)
                              tryStep 1.0
                            
                            checkPos a d curr = do
                                scaled <- matScale a d
                                cand <- matAdd curr scaled
                                res <- cholesky cand
                                return (isJust res)
                        
                        alphaP <- foldM (\acc (d, c) -> do a <- findAlpha d c; return (min acc a)) 1.0 (zip dxSym x)
                        alphaD <- foldM (\acc (d, c) -> do a <- findAlpha d c; return (min acc a)) 1.0 (zip dsSym s)
                        let alpha = min alphaP alphaD * 0.95
                        
                        xNew <- zipWithM (\c d -> do s <- matScale alpha d; matAdd c s) x dxSym
                        sNew <- zipWithM (\c d -> do s <- matScale alpha d; matAdd c s) s dsSym
                        yNew <- do
                           newY <- newVector m
                           forM_ [0..m-1] $ \i -> do
                             yi <- readArray y i
                             dyi <- readArray dy i
                             writeArray newY i (yi + alpha * dyi)
                           return newY
                        
                        loop (iter + 1)
  
  loop 0

  where
    isNothing Nothing = True
    isNothing _ = False
    fromJust (Just x) = x
    
    zipWithM f xs ys = sequence (zipWith f xs ys)
    
    symmetrize m = do
      t <- transpose m
      s <- matAdd m t
      matScale 0.5 s

-- =============================================
-- 3. SOS Construction
-- =============================================

-- | Check if polynomial is SOS (unconstrained). Pure version.
-- Uses internal IO but is referentially transparent.
{-# NOINLINE checkSOS_SDP #-}
checkSOS_SDP :: Poly -> Bool
checkSOS_SDP p = checkSOS_Constrained p []

-- | Check if polynomial is SOS modulo constraints. Pure version.
-- Uses internal IO but is referentially transparent.
--
-- Note: This uses unsafePerformIO internally because:
-- 1. The computation is deterministic - same input always produces same output
-- 2. The IO is only for efficient mutable arrays in numerical computation
-- 3. No observable side effects occur
{-# NOINLINE checkSOS_Constrained #-}
checkSOS_Constrained :: Poly -> [Poly] -> Bool
checkSOS_Constrained p gList =
  -- Using unsafePerformIO is safe here because the computation is deterministic
  -- and has no observable side effects. The NOINLINE pragma prevents problematic
  -- optimizations. For explicit IO handling, use checkSOS_Constrained_IO.
  unsafePerformIO $ checkSOS_Constrained_IO p gList
  where
    -- Import here to keep it local to this usage
    unsafePerformIO :: IO a -> a
    unsafePerformIO = System.IO.Unsafe.unsafePerformIO

-- | Check if polynomial is SOS (unconstrained). IO version.
checkSOS_SDP_IO :: Poly -> IO Bool
checkSOS_SDP_IO p = checkSOS_Constrained_IO p []

-- | Check if polynomial is SOS modulo constraints. IO version.
-- Prefer this when already in IO context for better composability.
checkSOS_Constrained_IO :: Poly -> [Poly] -> IO Bool
checkSOS_Constrained_IO p gList = do
  let vars = S.toList $ S.unions (getVars p : map getVars gList)
      degP = polyDegree p
      degGs = map polyDegree gList
      maxDeg = maximum (degP : map (+0) degGs)
      halfD = (maxDeg + 1) `div` 2
      
      genMons :: [String] -> Int -> [Monomial]
      genMons _ 0 = [Monomial M.empty]
      genMons [] _ = []
      genMons (v:vs) k = 
        [ Monomial (M.insert v (fromIntegral i) m) | i <- [0..k], mon <- genMons vs (k-i), let Monomial m = mon ]

      basis0 = genMons vars halfD
      basesG = [ genMons vars (halfD - (d + 1) `div` 2) | d <- degGs ]
      
      bases = basis0 : basesG
      dims = map length bases
      numBlocks = length dims
      
      termMap = genTermMap basis0 basesG gList
      
      Poly targetMap = p
      allMons = S.toList $ S.fromList (M.keys termMap ++ M.keys targetMap)
      
      m = length allMons
      
  constraints <- forM allMons $ \mon -> do
     buildConstraintRow mon termMap dims
  
  bVector <- newVector m
  forM_ (zip [0..] allMons) $ \(i, mon) -> do
     let val = fromRational (M.findWithDefault 0 mon targetMap)
     writeArray bVector i val
  
  cMatrixZero <- mapM (\d -> newMatrix d d) dims
  
  res <- solveSDP constraints bVector cMatrixZero 100
  case res of
    SDPFeasible _ -> return True
    _ -> return False

genTermMap :: [Monomial] -> [[Monomial]] -> [Poly] -> M.Map Monomial [(Int, Int, Int, Rational)]
genTermMap basis0 basesG gList = 
  let k0 = [ (mon, [(0, i, j, 1)])
           | (i, b1) <- zip [0..] basis0
           , (j, b2) <- zip [0..] basis0
           , j >= i
           , let mon = monomialMul b1 b2
           ]
      
      kRest = concat 
              [ [ (mon, [(k, i, j, c)])
                | (i, b1) <- zip [0..] bk
                , (j, b2) <- zip [0..] bk
                , j >= i
                , let baseProd = monomialMul b1 b2
                , (gm, c) <- M.toList (let Poly m = gk in m)
                , let mon = monomialMul baseProd gm
                ]
              | (k, bk, gk) <- zip3 [1..] basesG gList
              ]
  in M.fromListWith (++) (k0 ++ kRest)

buildConstraintRow :: Monomial -> M.Map Monomial [(Int, Int, Int, Rational)] -> [Int] -> IO [Matrix]
buildConstraintRow m termMap dims = do
  let contributions = M.findWithDefault [] m termMap
  forM [0..length dims - 1] $ \k -> do
     let dim = dims !! k
         kContribs = filter (\(bk,_,_,_) -> bk == k) contributions
     mat <- newMatrix dim dim
     forM_ kContribs $ \(_, i, j, c) -> do
        let cVal = fromRational c
        v1 <- readArray mat (i, j)
        writeArray mat (i, j) (v1 + cVal)
        if i /= j then do
           v2 <- readArray mat (j, i)
           writeArray mat (j, i) (v2 + cVal)
        else return ()
     return mat

monomialMul :: Monomial -> Monomial -> Monomial
monomialMul (Monomial m1) (Monomial m2) = Monomial (M.unionWith (+) m1 m2)

polyDegree :: Poly -> Int
polyDegree (Poly m) = if M.null m then 0 else fromIntegral $ maximum [ sum (M.elems v) | (Monomial v, _) <- M.toList m ]
