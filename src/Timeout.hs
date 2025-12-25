{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timeout
  ( TimeoutContext(..)
  , TimeoutM
  , runTimeoutM
  , withTimeout
  , checkTimeout
  , noTimeout
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
-- Uses ExceptT for proper error handling with ProverError
newtype TimeoutM a = TimeoutM { unTimeoutM :: ExceptT ProverError (ReaderT TimeoutContext IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TimeoutContext, MonadError ProverError)

-- | Run a TimeoutM computation with a given context
-- Returns Either ProverError a
runTimeoutM :: TimeoutContext -> TimeoutM a -> IO (Either ProverError a)
runTimeoutM ctx action = runReaderT (runExceptT (unTimeoutM action)) ctx

-- =============================================
-- Timeout Operations
-- =============================================

-- | Create a timeout context with a deadline (in seconds from now)
withTimeout :: Integer -> IO TimeoutContext
withTimeout seconds = do
  now <- getCurrentTime
  let deadline' = addUTCTime (fromIntegral seconds :: NominalDiffTime) now
  pure $ TimeoutContext (Just deadline') now

-- | Create a timeout context with no timeout
noTimeout :: IO TimeoutContext
noTimeout = do
  now <- getCurrentTime
  pure $ TimeoutContext Nothing now

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

-- | Throw an error if timeout has been exceeded (legacy interface)
-- This uses 'error' for backward compatibility with existing code
throwTimeout :: String -> TimeoutM ()
throwTimeout context = do
  timedOut <- checkTimeout
  when timedOut $ error $ "Timeout exceeded: " ++ context

-- | Throw a proper ProverError if timeout has been exceeded
-- Preferred over throwTimeout for new code
throwTimeoutError :: TimeoutErrorType -> TimeoutM ()
throwTimeoutError errType = do
  timedOut <- checkTimeout
  when timedOut $ throwError (TimeoutError errType)
