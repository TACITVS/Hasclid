{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timeout
  ( TimeoutContext(..)
  , TimeoutM
  , runTimeoutM
  , withTimeout
  , checkTimeout
  , noTimeout
  , throwTimeout
  ) where

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)

-- =============================================
-- Timeout Context
-- =============================================

-- | Context for timeout operations
-- Contains optional deadline and start time for timeout checks
data TimeoutContext = TimeoutContext
  { deadline :: Maybe UTCTime  -- ^ When timeout should occur (Nothing = no timeout)
  , startTime :: UTCTime        -- ^ When computation started
  }

-- =============================================
-- Timeout Monad
-- =============================================

-- | Monad for computations with timeout support
-- Wraps ReaderT to carry timeout context
newtype TimeoutM a = TimeoutM { unTimeoutM :: ReaderT TimeoutContext IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TimeoutContext)

-- | Run a TimeoutM computation with a given context
runTimeoutM :: TimeoutContext -> TimeoutM a -> IO a
runTimeoutM ctx action = runReaderT (unTimeoutM action) ctx

-- =============================================
-- Timeout Operations
-- =============================================

-- | Create a timeout context with a deadline (in seconds from now)
withTimeout :: Double -> IO TimeoutContext
withTimeout seconds = do
  now <- getCurrentTime
  let deadline' = addUTCTime (realToFrac seconds :: NominalDiffTime) now
  return $ TimeoutContext (Just deadline') now

-- | Create a timeout context with no timeout
noTimeout :: IO TimeoutContext
noTimeout = do
  now <- getCurrentTime
  return $ TimeoutContext Nothing now

-- | Check if timeout has been exceeded
-- Returns True if deadline has passed, False otherwise
checkTimeout :: TimeoutM Bool
checkTimeout = do
  ctx <- ask
  case deadline ctx of
    Nothing -> return False  -- No timeout set
    Just dl -> do
      now <- liftIO getCurrentTime
      return $ now >= dl

-- | Throw an error if timeout has been exceeded
-- This is a convenience function for use in long-running computations
throwTimeout :: String -> TimeoutM ()
throwTimeout context = do
  timedOut <- checkTimeout
  when timedOut $ error $ "Timeout exceeded: " ++ context
