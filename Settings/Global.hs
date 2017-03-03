module Settings.Global (
    module Settings.Global,
    module Control.Monad.Trans.RWS.Strict,
    module Control.Monad.Trans.Except,
    module Settings.Environment.Reader,
    module Settings.Environment.State,
    liftIO,
    lift
) where

import           Control.Exception
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Except     hiding (liftCallCC)
import           Control.Monad.Trans.RWS.Strict hiding (liftCallCC)
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.DList
import           Settings.Exception

import           Settings.Environment.Reader
import           Settings.Environment.State

type Logger = DList L.ByteString

type Global = ExceptT SomeException (RWST EnvR Logger EnvS IO)

type GlobalNoE = RWST EnvR Logger EnvS IO

catchIOE :: IO a -> Global a
catchIOE = ExceptT . liftIO . try

eitherToE :: Either String a -> Global a
eitherToE = ExceptT . return . mapLeft fromString where
    mapLeft f (Left x)  = Left (f x)
    mapLeft _ (Right r) = Right r

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither _ (Just x) = Right x
maybeToEither s Nothing  = Left s

maybeToE :: String -> Maybe a -> Global a
maybeToE str = eitherToE . maybeToEither str

try' :: IO a -> IO (Either SomeException a)
try' = try
