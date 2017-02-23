{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.CourseFile where

import           Control.Monad     (unless, when)
import qualified Course.Course     as C
import           Data.Aeson
import           Files.Downloader
import           GHC.Generics
import           Settings.Settings
import           System.Directory
import           System.FilePath   (makeValid, (</>))

type FileID = Int
type FolderID = Int

data FileJ = FileJ {
    id           :: FileID,
    folder_id    :: FolderID,
    filename     :: String,
    url          :: String,
    size         :: Int,
    display_name :: String
} deriving (Eq, Show, Generic)
instance ToJSON FileJ
instance FromJSON FileJ

getFileName :: FileJ -> String
getFileName = makeValid . display_name

data FolderJ = FolderJ {
    name             :: String,
    parent_folder_id :: Maybe FolderID,
    files_url        :: String,
    folders_url      :: String
} deriving (Eq, Show, Generic)
instance ToJSON FolderJ
instance FromJSON FolderJ

getFilesFromCourse :: C.CourseID -> MainExcept [FileJ]
getFilesFromCourse id' = simpleHttpJSON $ "courses/" ++ show id' ++ "/files"

downloadFromCourse :: FilePath -> C.CourseID -> MainExcept DownloadState
downloadFromCourse folder id' = do
    lift $ putStrLn "Counting files..."
    fileList <- getFilesFromCourse id'
    lift $ putStrLn $ "Files to download: " ++ show (length fileList)
    lift $ downloadBatchTo (map url fileList) $ map ((folder </>) . getFileName) fileList

downloadToNewFolder :: FilePath -> C.CourseID -> C.CourseShortName -> MainExcept ()
downloadToNewFolder downloadDir id' nm = do
    lift $ putStrLn $ "Now downloading for course " ++ nm
    catchMainE $ createDirectoryIfMissing False $ downloadDir </> nm
    state <- downloadFromCourse (downloadDir </> nm) id'
    lift $ print state
    when (isAllFail state) $ do
        lift $ putStrLn "It is likely that something got wrong; enter Y to continue downloading, otherwise abort."
        input <- lift getLine
        let continue = not (null input) && head input == 'Y'
        unless continue $ throwE "Unknown error in downloading progress."
