{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad      (mapM_, unless, void, (>=>))
import qualified Course.Course      as C
import           Course.CourseFile
import           Settings.Exception
import           Settings.Settings
import           System.Directory
import           System.FilePath    (isRelative, (</>))
import           Text.Read          (readEither)

main :: IO ()
main = runExceptT mainE >>= either printException return

printException :: SomeException -> IO ()
printException ex = do
    putStrLn $ toString ex
    putStrLn "Press anykey to quit."
    void getChar

mainE :: Global ()
mainE = do
    lift $ putStrLn "Fetching course list..."
    this <- C.thisTermCourse
    lift $ putStrLn $ concatMap printCourse this
    --courseId <- lift $ promptAndCheckId $ map C.id this
    lift $ putStrLn "Will download file of each course to corresponding subfolder."
    downloadDir <- lift promptAndCheckPath
    mapM_ (downloadFromCourse downloadDir) this

printCourse :: C.Course -> String
printCourse course = show (C.name course) ++ "\t\t\t\t" ++ show (C.id course) ++ "\n"

promptAndCheckId :: [C.CourseID] -> IO C.CourseID
promptAndCheckId lst = do
    putStrLn "Which course to sync the files (ID):"
    eitherIdInputed <- readEither <$> getLine
    let idValid = either (const Nothing) (\i -> if i `elem` lst then Just i else Nothing)
                  eitherIdInputed
    maybe (putStrLn "Invalid id." >> promptAndCheckId lst) return idValid

promptAndCheckPath :: IO FilePath
promptAndCheckPath = do
    path <- getCurrentDirectory
    putStrLn $ "Presently at path " ++ path
    putStrLn "Download to where (relative/absolute):"
    pathInputed <- getLine
    let absolute = if isRelative pathInputed then path </> pathInputed
                                             else pathInputed
    exist <- doesDirectoryExist absolute
    unless exist $ do
        putStrLn "Path does not exist; creating..."
        createDirectory absolute
    putStrLn $ "Will download to " ++ absolute
    return absolute
