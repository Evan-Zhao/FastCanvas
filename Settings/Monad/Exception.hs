{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Settings.Monad.Exception (
    module Settings.Monad.Exception,
    SomeException,
    liftIO
) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class (MonadIO, liftIO)

type MonadIOE e m = (MonadIO m, MonadError e m, Exception e)

catchIOE :: MonadIOE e m => IO a -> m a
catchIOE act = liftIO (try act) >>= eitherToE

eitherToE :: MonadError e m => Either e a -> m a
eitherToE (Left e)  = throwError e
eitherToE (Right a) = return a

maybeToEWith :: MonadError e m => e -> Maybe a -> m a
maybeToEWith e = eitherToE . maybeToEither e where
    maybeToEither _ (Just x) = Right x
    maybeToEither s Nothing  = Left s
