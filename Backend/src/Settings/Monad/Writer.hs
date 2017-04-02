{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.Writer where

import           Control.Concurrent
import           Control.Monad.Writer
import           Data.DList
import           Data.Time.Clock

type EnvW = DList Record

data Record = Record {
    rTime     :: UTCTime,
    rThreadId :: ThreadId,
    rEvent    :: String
} deriving (Show)

tellRecord :: (MonadWriter Record m, MonadIO m) => String -> m ()
tellRecord event = do
    rec <- liftIO $ Record <$> getCurrentTime <*> myThreadId <*> return event
    tell rec

type MonadEnvWriter m = MonadWriter EnvW m
