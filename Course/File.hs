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

getRootFromCourse :: RIOE' m => C.CourseID -> m (Maybe FolderJSON)
getRootFromCourse id' = do
    foldersJSON <- canvasJSON $ "courses/" ++ show id' ++ "/folders"
    return $ find ((==Nothing) . parent_folder_id) (foldersJSON :: [FolderJSON])

downloadFromCourse :: (RIOE' m, SIO m) => FilePath -> C.Course -> m DownloadSummary
downloadFromCourse folder course = do
    x <- getRootFromCourse id'
    maybe emptyCourseResponse (nonemptyCourse courseName folder) x
  where
    courseName = C.courseShortName course
    id' = C.id course

emptyCourseResponse :: MonadIO m => m DownloadSummary
emptyCourseResponse = return mempty

nonemptyCourse :: (RIOE' m, SIO m) => String -> FilePath -> FolderJSON -> m DownloadSummary
nonemptyCourse courseName folder root = do
    fileTree <- unfoldFileTree (Right root)
    if isSingleNode fileTree
    then return mempty
    else do
        let fileTree' = renameRoot courseName fileTree
        downloadTree folder fileTree'
