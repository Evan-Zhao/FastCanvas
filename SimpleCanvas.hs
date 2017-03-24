{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception
import           Control.Monad              (mapM_, unless, void)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Except (runExceptT)
import           System.Directory
import           System.FilePath            (isRelative, (</>))
import           Text.Read                  (readEither)

import           Course.File
import qualified Course.List                as CL
import           Settings.Monad.Global

import qualified Data.ByteString.Lazy       as L

mainAdaptor :: Global a -> IO a
mainAdaptor g = do
    envR <- getEnvR
    envS <- getEnvS
    (result, envNewS, logged) <- runRWST (runExceptT g) envR envS
    return $ takeout result
  where
    takeout (Right a) = a

main :: IO ()
main = do
    envR <- getEnvR
    envS <- getEnvS
    (result, envNewS, logged) <- runRWST (runExceptT mainG) envR envS
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
    defPath <- getDefaultPath <$> lift get
    liftIO $ checkDefaultPath defPath
    liftIO $ putStrLn $ "Will download to " ++ defPath ++ "\n"
    mapM_ (downloadFromCourse defPath) this

checkDefaultPath :: FilePath -> IO ()
checkDefaultPath path = do
    isExist <- doesDirectoryExist path
    unless isExist $ do
        liftIO $ putStrLn $ "Warning: default path " ++ path ++ " does not exist; creating..."
        createDirectoryIfMissing True path
