module Settings.Monad.Global (
    module Settings.Monad.Global,
    module Settings.Monad.Exception,
    module Settings.Monad.Reader,
    module Settings.Monad.State,
    module Settings.Monad.Writer,
    module Settings.Exception.GeneralException,
    Async
) where

import           Control.Concurrent.Async
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict

import           Settings.Exception.GeneralException
import           Settings.Monad.Exception
import           Settings.Monad.Reader
import           Settings.Monad.State
import           Settings.Monad.Writer

type Global = ExceptT SomeException (RWST EnvR EnvW EnvS IO)
type Excepted a = Either SomeException a
type ExDisplayed a = Either String a

runGlobal :: Global a -> IO (Excepted a)
runGlobal g = do
    (envR, envS) <- setup
    (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS

runGlobalAsync :: Global a -> IO (EnvS, Async (Excepted a))
runGlobalAsync g = do
    (envR, envS) <- setup
    asynced <- async $ (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS
    return (envS, asynced)

setup :: IO (EnvR, EnvS)
setup = do
    maybeEnvR <- tryGetEnvR
    envR <- maybe (error "Config read fault!") return maybeEnvR
    envS <- getEnvS
    return (envR, envS)
