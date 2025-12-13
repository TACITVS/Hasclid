{-# LANGUAGE DeriveGeneric #-}

module Positivity.SDP
  ( solveSDP
  , GramMatrix
  , MonomialVector
  ) where

import qualified Data.Map.Strict as M
import Data.List (nub, sort)
import Expr (Monomial(..))

-- | A Gram Matrix represents a quadratic form v^T Q v
-- Keys are indices (i, j) into the MonomialVector
type GramMatrix = M.Map (Int, Int) Double

-- | The vector of monomials v in v^T Q v
type MonomialVector = [Monomial]

-- | Simple Gradient Descent to find Positive Semidefinite Q such that v^T Q v = P
-- Minimize: || P - v^T (L L^T) v ||^2
-- Q = L L^T ensures PSD.
-- We optimize the entries of lower-triangular matrix L.
solveSDP :: M.Map Monomial Double -> MonomialVector -> Maybe GramMatrix
solveSDP targetPoly monomials =
  let 
      n = length monomials
      
      -- Initial guess: Identity matrix for L (Q = I)
      initialL = M.fromList [ ((i,j), if i==j then 1.0 else 0.0) | i <- [0..n-1], j <- [0..i] ]
      
      -- Optimize L
      finalL = gradientDescent targetPoly monomials initialL 1000 0.1
      
      -- Compute Q = L * L^T
      q = computeQ finalL n
      
      -- Check error
      err = errorFunc targetPoly monomials q
  in
      if err < 1e-4 then Just q else Nothing

-- | Compute Q = L * L^T
computeQ :: M.Map (Int, Int) Double -> Int -> GramMatrix
computeQ lMat n =
  M.fromList [ ((i,j), sum [ M.findWithDefault 0 (i,k) lMat * M.findWithDefault 0 (j,k) lMat | k <- [0..min i j] ])
             | i <- [0..n-1], j <- [0..n-1] 
             ]

-- | Error function: Sum of squared differences between coeffs of (v^T Q v) and P
errorFunc :: M.Map Monomial Double -> MonomialVector -> GramMatrix -> Double
errorFunc target monomials q = 
  let 
      -- Construct polynomial from Q: sum Q_ij * m_i * m_j
      reconstructed = M.fromListWith (+) 
        [ (monomialMul (monomials !! i) (monomials !! j), val)
        | ((i,j), val) <- M.toList q 
        ]
      
      -- Difference
      diff = M.unionWith (-) target reconstructed
  in 
      sum [ c*c | c <- M.elems diff ]

-- | Gradient Descent step
gradientDescent :: M.Map Monomial Double -> MonomialVector -> M.Map (Int, Int) Double -> Int -> Double -> M.Map (Int, Int) Double
gradientDescent _ _ l 0 _ = l
gradientDescent target monos l iter learningRate =
  let
      -- Very simplified numerical gradient (finite differences)
      -- For each entry L_ij, perturbation
      delta = 1e-5
      
      step (k, val) =
        let lPlus = M.insert k (val + delta) l
            lMinus = M.insert k (val - delta) l
            n = length monos
            qPlus = computeQ lPlus n
            qMinus = computeQ lMinus n
            errPlus = errorFunc target monos qPlus
            errMinus = errorFunc target monos qMinus
            grad = (errPlus - errMinus) / (2 * delta)
        in (k, val - learningRate * grad)
      
      lList = M.toList l
      newL = M.fromList (map step lList)
      
      -- Check convergence? 
      -- Just run fixed iterations for now
  in gradientDescent target monos newL (iter - 1) learningRate

-- Helper: Monomial multiplication
monomialMul :: Monomial -> Monomial -> Monomial
monomialMul (Monomial a) (Monomial b) = Monomial (M.unionWith (+) a b)
