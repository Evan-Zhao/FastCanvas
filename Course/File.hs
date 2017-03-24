{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Course.File where

import           Control.Monad                       (unless, when)
import           Control.Monad.Except
import qualified Course.List                         as C
import           Data.List                           (find)

import           Files.Node.NodeJSON
import           Files.State
import           Files.Structure
import           Settings.Exception.GeneralException
import           Settings.Monad.Exception
import           Settings.Network

getRootFromCourse :: MonadRIOE' m => C.CourseID -> m (Maybe FolderJSON)
getRootFromCourse id' = do
    foldersJSON <- canvasJSON $ "courses/" ++ show id' ++ "/folders"
    return $ find ((==Nothing) . parent_folder_id) (foldersJSON :: [FolderJSON])

downloadFromCourse :: MonadRIOE' m => FilePath -> C.Course -> m ()
downloadFromCourse folder course = do
    liftIO $ putStrLn $ "\nNow downloading for course " ++ courseName
    maybeRootFolder <- getRootFromCourse id'
    state <- maybe emptyCourseResponse nonemptyCourseResponse maybeRootFolder
    liftIO $ print state
    when (isAllFail state) potentialError
  where
    nonemptyCourseResponse = nonemptyCourse courseName folder
    courseName = C.courseShortName course
    id' = C.id course

escape :: MonadIO m => String -> m DownloadState
escape str = do
    liftIO $ putStrLn str
    return mempty

emptyCourseResponse :: MonadIO m => m DownloadState
emptyCourseResponse = escape "Empty file list; skipping."

nonemptyCourse :: MonadRIOE' m => String -> FilePath -> FolderJSON -> m DownloadState
nonemptyCourse courseName folder root = do
    let [fileN, folderN] = [files_count root, folders_count root]
    liftIO $ putStrLn $ "Files to download: " ++ show fileN
    fileTree <- unfoldFileTree (Right root)
    if fileN == 0 && folderN == 0
        then escape $ "Course " ++ courseName ++ " has empty file list."
        else do
            let fileTree' = renameRoot courseName fileTree
            downloadTree folder fileTree'

potentialError :: MonadIOE SomeException m => m ()
potentialError = do
    liftIO $ putStrLn "It is likely that something got wrong; enter Y to continue downloading; otherwise abort."
    input <- liftIO getLine
    let continue = not (null input) && head input == 'Y'
    unless continue $ throwError $ fromString "Unknown error in downloading progress."
