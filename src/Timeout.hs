{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Timeout
-- Description : Cooperative timeout mechanism for long-running computations
-- Copyright   : (c) 2024-2025
-- License     : MIT
-- Stability   : stable
--
-- This module provides a cooperative timeout mechanism for the prover's
-- long-running algorithms (Groebner basis, CAD, etc.).
--
-- = Design Philosophy
--
-- Rather than using asynchronous exceptions (which can leave data in
-- inconsistent states), this module uses a cooperative approach where
-- algorithms periodically check if they have exceeded their time budget.
--
-- = Usage
--
-- @
-- import Timeout
--
-- -- Run a computation with a 30-second timeout
-- ctx <- withTimeout 30
-- result <- runTimeoutM ctx $ do
--   -- Long computation...
--   throwTimeoutError BuchbergerTimeout  -- Check timeout periodically
--   -- More computation...
--   return result
--
-- case result of
--   Left (TimeoutError errType) -> putStrLn "Computation timed out"
--   Left err -> putStrLn $ "Other error: " ++ show err
--   Right val -> putStrLn $ "Success: " ++ show val
-- @
--
-- = Error Handling
--
-- The 'TimeoutM' monad uses 'ExceptT' for proper error handling,
-- returning 'Either ProverError a' rather than throwing exceptions.

module Timeout
  ( -- * Context Management
    TimeoutContext(..)
  , withTimeout
  , noTimeout
    -- * The Timeout Monad
  , TimeoutM
  , runTimeoutM
    -- * Timeout Checking
  , checkTimeout
  , throwTimeout
  , throwTimeoutError
  ) where

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Error (ProverError(..), TimeoutErrorType(..))

-- =============================================
-- Timeout Context
-- =============================================

-- | Configuration for timeout-aware computations.
--
-- A 'TimeoutContext' specifies when a computation should be considered
-- timed out (optional deadline) and when it started (for diagnostics).
data TimeoutContext = TimeoutContext
  { deadline  :: Maybe UTCTime
    -- ^ When timeout should occur. 'Nothing' means no timeout.
  , startTime :: UTCTime
    -- ^ When the computation started (for elapsed time calculations).
  }

-- =============================================
-- Timeout Monad
-- =============================================

-- | Monad for computations with timeout support.
--
-- 'TimeoutM' combines:
--
--   * 'ReaderT' for accessing the timeout context
--   * 'ExceptT' for proper error handling (no runtime exceptions)
--   * 'IO' for time-based deadline checking
--
-- This monad stack provides cooperative timeout checking where algorithms
-- explicitly call 'checkTimeout' or 'throwTimeoutError' at appropriate points.
newtype TimeoutM a = TimeoutM { unTimeoutM :: ExceptT ProverError (ReaderT TimeoutContext IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TimeoutContext, MonadError ProverError)

-- | Run a 'TimeoutM' computation with a given context.
--
-- Returns 'Left' if the computation failed (including timeout),
-- 'Right' if it succeeded.
--
-- ==== Example
--
-- @
-- ctx <- withTimeout 60
-- result <- runTimeoutM ctx computeGroebnerBasis
-- @
runTimeoutM :: TimeoutContext -> TimeoutM a -> IO (Either ProverError a)
runTimeoutM ctx action = runReaderT (runExceptT (unTimeoutM action)) ctx

-- =============================================
-- Timeout Operations
-- =============================================

-- | Create a timeout context with a deadline.
--
-- The deadline is computed as current time plus the specified number of seconds.
--
-- ==== Example
--
-- @
-- ctx <- withTimeout 30  -- 30-second timeout
-- @
withTimeout :: Integer -> IO TimeoutContext
withTimeout seconds = do
  now <- getCurrentTime
  let deadline' = addUTCTime (fromIntegral seconds :: NominalDiffTime) now
  pure $ TimeoutContext (Just deadline') now

-- | Create a timeout context with no timeout.
--
-- Useful for interactive sessions or when timeout is not desired.
noTimeout :: IO TimeoutContext
noTimeout = do
  now <- getCurrentTime
  pure $ TimeoutContext Nothing now

-- | Check if timeout has been exceeded.
--
-- Returns 'True' if the deadline has passed, 'False' otherwise.
-- If no deadline was set, always returns 'False'.
--
-- This function does not throw an error; use 'throwTimeoutError' for that.
checkTimeout :: TimeoutM Bool
checkTimeout = do
  ctx <- ask
  case deadline ctx of
    Nothing -> return False  -- No timeout set
    Just dl -> do
      now <- liftIO getCurrentTime
      return $ now >= dl

-- | Check timeout and throw a runtime error if exceeded (legacy interface).
--
-- __Deprecated:__ Prefer 'throwTimeoutError' which uses proper error handling.
--
-- This function uses 'error' for backward compatibility with existing code
-- that expects runtime exceptions rather than 'Either' returns.
throwTimeout :: String -> TimeoutM ()
throwTimeout context = do
  timedOut <- checkTimeout
  when timedOut $ error $ "Timeout exceeded: " ++ context

-- | Check timeout and throw a typed 'ProverError' if exceeded.
--
-- This is the preferred method for timeout checking in new code.
-- It uses proper 'ExceptT' error handling rather than runtime exceptions.
--
-- ==== Example
--
-- @
-- -- In a Buchberger computation loop:
-- forM_ sPairs $ \pair -> do
--   throwTimeoutError BuchbergerTimeout
--   processSPair pair
-- @
throwTimeoutError :: TimeoutErrorType -> TimeoutM ()
throwTimeoutError errType = do
  timedOut <- checkTimeout
  when timedOut $ throwError (TimeoutError errType)
