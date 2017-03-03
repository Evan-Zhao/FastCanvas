{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Char8   as LC
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Maybe                   (fromJust)
import           Network                      (withSocketsDo)
import           Network.Connection           (TLSSettings (..))
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header
import           Text.Printf                  (printf)

downloadFile :: IO ()
downloadFile = do
    request <- parseUrl "http://www.baidu.com"
    withManager $ \mgr -> do
        resp <- http request mgr
        let hs = responseHeaders resp
        let contentLength = findContentLength hs
        liftIO $ printf "content length : %d \n" (findContentLength hs)
        responseBody resp $$+- countBytes 0 contentLength =$ CB.sinkFile "1.txt"

findContentLength :: ResponseHeaders -> Integer
findContentLength headers = case lookup (CI.mk "Content-Length") headers of
    Just l -> fst . fromJust . LC.readInteger $ L.fromStrict l
    _      -> -1

countBytes :: MonadResource m => Integer -> Integer -> Conduit ByteString m ByteString
countBytes acc total = do
    bs <- await
    case bs of
        Nothing -> return ()
        Just b  -> do
            let len = fromIntegral $ B.length b
            yield b
            liftIO $ printf "total: %d, consumed: %d,  %f%% \n" total acc percent
            countBytes (acc + len) total
  where
    percent :: Double
    percent = fromIntegral . floor $ (fromIntegral acc / fromIntegral total) * 100

main = downloadFile
