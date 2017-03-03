{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad    (mapM_, unless, void)
import           System.Directory
import           System.FilePath  (isRelative, (</>))
import           Text.Read        (readEither)

import           Course.File
import qualified Course.List      as CL
import           Settings

main :: IO ()
main = do
    envR <- getEnvR
    envS <- getEnvS
    (result, envNewS, logged) <- runRWST (runExceptT mainG) envR envS
    either printException return result

printException :: SomeException -> IO ()
printException ex = do
    putStrLn $ toString ex
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
