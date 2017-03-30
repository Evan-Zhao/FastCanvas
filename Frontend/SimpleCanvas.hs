{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad              (mapM_, unless, void)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy       as L
import           System.Directory
import           System.FilePath            (isRelative, (</>))
import           Text.Read                  (readEither)

import qualified Course.List                as CL
import           Frontend.Course.File
import           Frontend.Initialize
import           Settings.Monad.Global

setup :: IO (EnvR, EnvS)
setup = do
    maybeEnvR <- tryGetEnvR
    envR <- maybe (error "Config read fault!") return maybeEnvR
    envS <- getEnvS
    return (envR, envS)

mainAdaptor :: Global a -> IO a
mainAdaptor g = do
    (result, envNewS, logged) <- setup >>= uncurry (runRWST $ runExceptT g)
    return $ takeout result
  where
    takeout (Right a) = a

main :: IO ()
main = do
    (result, envNewS, logged) <- setup >>=uncurry (runRWST $ runExceptT mainG)
    either printException return result

printException :: (Exception e) => e -> IO ()
printException ex = do
    print ex
    putStrLn "Press enter to quit."
    void getChar

mainG :: Global ()
mainG = do
    liftIO $ putStrLn "Fetching course list...\n"
    this <- CL.thisTermCourse
    defPath <- asks getDefaultPath
    liftIO $ putStrLn $ "Will download to " ++ defPath ++ "\n"
    isExist <- liftIO $ checkDefaultPath defPath
    unless isExist $ liftIO $ putStrLn $
        "Warning: default path " ++ defPath ++ " does not exist; creating..."
    mapM_ (downloadFromCourse defPath) this

checkDefaultPath :: FilePath -> IO Bool
checkDefaultPath path = do
    isExist <- doesDirectoryExist path
    unless isExist $ createDirectoryIfMissing True path
    return isExist
