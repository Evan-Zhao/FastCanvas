module Settings (
    module Settings.Global,
    module Settings.Network,
    module Settings.Exception,
    LazyByteString,
    StrictByteString
) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Settings.Exception
import           Settings.Global
import           Settings.Network

type LazyByteString = L.ByteString
type StrictByteString = S.ByteString
