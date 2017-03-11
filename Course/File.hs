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

getRootFromCourse :: MonadRIOE e m => C.CourseID -> m (Maybe FolderJSON)
getRootFromCourse id' = do
    foldersJSON <- simpleHttpJSON $ "courses/" ++ show id' ++ "/folders"
    return $ find ((==Nothing) . parent_folder_id) (foldersJSON :: [FolderJSON])

downloadFromCourse :: MonadRIOE SomeException m => FilePath -> C.Course -> m ()
downloadFromCourse folder course = do
    liftIO $ putStrLn $ "\nNow downloading for course " ++ courseName
    liftIO $ putStrLn "Counting files..."
    state <- getRootFromCourse id' >>= maybe emptyCourseResponse (nonemptyCourse courseName folder)
    liftIO $ print state
    when (isAllFail state) potentialError
  where
    courseName = C.courseShortName course
    id' = C.id course

escape :: MonadIO m => String -> m DownloadState
escape str = do
    liftIO $ putStrLn str
    return mempty

emptyCourseResponse :: MonadIO m => m DownloadState
emptyCourseResponse = escape "Empty file list; skipping."

nonemptyCourse :: MonadIOE e m => String -> FilePath -> FolderJSON -> m DownloadState
nonemptyCourse courseName folder root = do
    fileTree <- unfoldFileTree (Right root)
    if isSingleNode fileTree
    then escape $ "Course " ++ courseName ++ " has empty file list."
    else do
        let fileTree' = renameRoot courseName fileTree
        liftIO $ putStrLn $ "Files (folders) to download: " ++ show (length fileTree)
        downloadTree folder fileTree'

potentialError :: MonadIOE SomeException m => m ()
potentialError = do
    liftIO $ putStrLn "It is likely that something got wrong; enter Y to continue downloading; otherwise abort."
    input <- liftIO getLine
    let continue = not (null input) && head input == 'Y'
    unless continue $ throwError $ fromString "Unknown error in downloading progress."
