{-# LANGUAGE OverloadedStrings #-}

module Course.CourseFile where

import           Control.Monad       (unless, when)
import qualified Course.Course       as C
import           Data.List           (find)
import           Files.Node.NodeJSON
import           Files.State
import           Files.Structure
import           Settings.Imports

getRootFromCourse :: C.CourseID -> Global (Maybe FolderJSON)
getRootFromCourse id' = do
    foldersJSON <- simpleHttpJSON $ "courses/" ++ show id' ++ "/folders"
    return $ find ((==Nothing) . parent_folder_id) (foldersJSON :: [FolderJSON])

downloadFromCourse :: FilePath -> C.Course -> Global ()
downloadFromCourse folder course = do
    liftIO $ putStrLn $ "Now downloading for course " ++ courseName
    liftIO $ putStrLn "Counting files..."
    state <- getRootFromCourse id' >>= maybe emptyCourseResponse nonemptyCourse
    liftIO $ print state
    when (isAllFail state) potentialError
      where
        courseName = C.courseShortName course
        id' = C.id course
        emptyCourseResponse = escape "Empty file list; skipping."
        nonemptyCourse root = do
            fileTree <- unfoldFileTree (Right root)
            if isSingleNode fileTree
                then escape $ "Course " ++ courseName ++ " has empty file list."
                else do
                    let fileTree' = renameRoot courseName fileTree
                    liftIO $ putStrLn $ "Files (folders) to download: " ++ show (length fileTree)
                    lift $ downloadTree folder fileTree'

escape :: String -> Global DownloadState
escape str = do
    liftIO $ putStrLn str
    return mempty

potentialError :: Global ()
potentialError = do
    liftIO $ putStrLn "It is likely that something got wrong; enter Y to continue downloading; otherwise abort."
    input <- liftIO getLine
    let continue = not (null input) && head input == 'Y'
    unless continue $ throwE $ fromString "Unknown error in downloading progress."
