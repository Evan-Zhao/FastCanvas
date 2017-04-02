{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Course.File (
    syncG,
    Sync
) where

import           Control.Monad.Except
import qualified Course.List           as CL
import           Data.List             (find)

import           Files.Node.NodeJSON
import           Files.State
import           Files.Structure
import           Settings.Monad.Global
import           Settings.Network

type Sync = [(CL.Course, DownloadSummary)]

syncG :: Global Sync
syncG = do
    this <- CL.thisTermCourse
    defPath <- lift $ asks getDefaultPath
    states <- mapM (downloadFromCourse defPath) this
    return $ zip this states

getRootFromCourse :: RIOE' m => CL.CourseID -> m (Maybe FolderJSON)
getRootFromCourse id' = do
    foldersJSON <- canvasJSON $ "courses/" ++ show id' ++ "/folders"
    return $ find ((==Nothing) . parent_folder_id) (foldersJSON :: [FolderJSON])

downloadFromCourse :: (RIOE' m, SIO m) => FilePath -> CL.Course -> m DownloadSummary
downloadFromCourse folder course = do
    x <- getRootFromCourse id'
    maybe emptyCourseResponse (nonemptyCourse courseName folder) x
  where
    courseName = CL.courseShortName course
    id' = CL.id course

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
