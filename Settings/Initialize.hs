module Settings.Initialize where

import           Control.Monad              (unless)
import           Data.Aeson
import           Data.Binary.Builder        (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           System.Directory           (createDirectoryIfMissing,
                                             doesDirectoryExist)

import           Settings.Monad.State

queryEnvS :: FilePath -> IO EnvS
queryEnvS writePath = do
    putStrLn "Settings file does not exist or is corrupted; please set your preference as follows:"
    userToken <- promptUserToken
    defaultPath <- promptDefaultPath
    let envS = EnvS { getUserToken   = userToken,
                      getDefaultPath = defaultPath }
    let configString = toLazyByteString $ fromEncoding $ toEncoding envS
    L.writeFile writePath configString
    return envS

promptUserToken :: IO String
promptUserToken = putStrLn "\nPlease input your token from Canvas:" >> promptUserToken'
  where
    tokenStandardLen = 69
    promptUserToken' = do
        token <- getLine
        if length token == tokenStandardLen
            then return token
            else putStrLn "Token has incorrect length; please double check." >> promptUserToken'

promptDefaultPath :: IO String
promptDefaultPath = putStrLn "\nYour favorite path for containing files from Canvas:"
                 >> promptDefaultPath'
  where
    promptDefaultPath' = do
        path <- getLine
        doesExist <- doesDirectoryExist path
        unless doesExist $ do
            putStrLn "Given path does not exist... creating"
            createDirectoryIfMissing True path
        return path
