{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad    (mapM_, unless, void)
import           System.Directory
import           System.FilePath  (isRelative, (</>))
import           Text.Read        (readEither)

import           Course.File
import qualified Course.List      as C
import           Settings

main :: IO ()
main = do
    envR <- takeRight <$> getEnvR
    envS <- takeRight <$> getEnvS
    (result, envNewS, logged) <- runRWST (runExceptT mainG) envR envS
    either printException return result
      where
        takeRight (Right x) = x

printException :: SomeException -> IO ()
printException ex = do
    putStrLn $ toString ex
    putStrLn "Press enter to quit."
    void getChar

mainG :: Global ()
mainG = do
    liftIO $ putStrLn "Fetching course list...\n"
    this <- C.thisTermCourse
    --liftIO $ putStrLn $ concatMap printCourse this
    --courseId <- liftIO $ promptAndCheckId $ map C.id this
    --liftIO $ putStrLn "Will download file of each course to corresponding subfolder."
    defPath <- getDefaultPath <$> lift ask
    liftIO $ checkDefaultPath defPath
    liftIO $ putStrLn $ "Will download to " ++ defPath ++ "\n"
    mapM_ (downloadFromCourse defPath) this

checkDefaultPath :: FilePath -> IO ()
checkDefaultPath path = do
    isExist <- doesDirectoryExist path
    unless isExist $ do
        liftIO $ putStrLn $ "Warning: default path " ++ path ++ " does not exist; creating..."
        createDirectoryIfMissing True path
