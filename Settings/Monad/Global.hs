module Settings.Monad.Global (
    module Settings.Monad.Global,
    module Settings.Monad.Exception,
    module Settings.Monad.Reader,
    module Settings.Monad.State
) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.RWS.Strict hiding (liftCallCC)
import qualified Data.ByteString.Lazy.Char8     as L
import           Data.DList

import           Settings.Monad.Exception
import           Settings.Monad.Reader
import           Settings.Monad.State

type Logger = DList L.ByteString

type Global = ExceptT SomeException (RWST EnvR Logger EnvS IO)

type GlobalNoE = RWST EnvR Logger EnvS IO
