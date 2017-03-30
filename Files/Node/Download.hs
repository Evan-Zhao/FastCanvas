{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Files.Node.Download (
    downloadTo,
    MonadDownload
) where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Char8   as LC
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import           Data.Maybe                   (fromJust)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header
import           Settings.Monad.Exception
import           System.Console.AsciiProgress
import           Text.Printf                  (printf)

type MonadDownload e m = (IOE e m, MonadThrow m)

downloadTo :: MonadDownload e m => String -> String -> m ()
downloadTo url path = do
    request <- parseRequest url
    catchIOE $ withManager $ \mgr -> displayConsoleRegions $ do
        resp <- http request mgr
        let hs = responseHeaders resp
        let contentLength = findContentLength hs
        maybe noLengthInfo (haveLengthInfo resp) contentLength
    where
      noLengthInfo = return ()
      haveLengthInfo resp len = responseBody resp $$+- countBytes 0 len =$ CB.sinkFile path

findContentLength :: ResponseHeaders -> Maybe Integer
findContentLength headers = transform <$> lookup (CI.mk "Content-Length") headers where
    transform = fst . fromJust . LC.readInteger . L.fromStrict

countBytes :: MonadResource m => Integer -> Integer -> Conduit B.ByteString m B.ByteString
countBytes acc total = do
    bs <- await
    case bs of
        Nothing -> return ()
        Just b  -> do
            let lenInt = B.length b
            yield b
            countBytes (acc + fromIntegral lenInt) total
