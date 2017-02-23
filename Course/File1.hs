{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module File1 where

import           Control.Monad    (unless, when, (>=>))
import qualified Course           as C
import           Data.List        (find)
import           Data.Tree
import           Downloader1
import           Settings
import           System.Directory
import           System.FilePath  (makeValid, (</>))
import           Token

type ID = Int

data FileJ = FileJ {
    id           :: ID,
    folder_id    :: ID,
    filename     :: String,
    url          :: ByteString,
    size         :: Int,
    display_name :: String
} deriving (Eq, Show, Generic)
instance ToJSON FileJ
instance FromJSON FileJ

getFileName :: FileJ -> String
getFileName = makeValid . display_name

data FolderJ = FolderJ {
    name             :: String,
    parent_folder_id :: Maybe ID,
    files_url        :: ByteString,
    folders_url      :: ByteString
} deriving (Eq, Show, Generic)
instance ToJSON FolderJ
instance FromJSON FolderJ

courseIdToReq :: C.CourseID -> Request
courseIdToReq id' = relativeParseRequest $ "courses/" ++ show id' ++ "/folders"

getParentFolder :: C.CourseID -> MainExcept FolderJ
getParentFolder id' = do
    req <- addToken $ courseIdToReq id'
    resp <- catchMainE $ getResponseBody <$> httpLBS req
    folderJ <- maybeToE "JSON parse error when getting file from course." $ decode resp
    let maybeRoot = find ((==Nothing) . parent_folder_id) (folderJ :: [FolderJ])
    maybeToE "Root folder not found (server error)." maybeRoot

-- data TreeSeed = FileSeed {
--                     fsFullPath :: String,
--                     fsUrl      :: ByteString
--                 } |
--                 FolderSeed {
--                     fsFullPath  :: String,
--                     fsFileUrl   :: ByteString,
--                     fsFolderUrl :: ByteString
--                 }

unfoldFileTree :: C.CourseID -> MainExcept (Tree WriteableNode)
unfoldFileTree = getParentFolder >=> (unfoldTreeM genTree . Right)
      where
        genTree :: WriteableNode -> MainExcept (WriteableNode, [WriteableNode])
        genTree node@(FileNode _ _) = return (node, [])
        genTree (FolderNode path file folder) = do
            fileJs <- getJSONE file
            folderJs <- getJSONE folder
            let seeds = map Left fileJs ++ map Right folderJs
            return (FolderNode $ name folderj, seeds)

getJSONE :: (FromJSON a) => ByteString -> MainExcept a
getJSONE = catchMainE . fmap getResponseBody . httpJSON . parseRequest

getFilesFromCourse :: C.CourseID -> MainExcept [FileJ]
getFilesFromCourse id' = do
    token <- tokenE
    let req = addToken token $ courseIdToReq id'
    resp <- catchMainE $ getResponseBody <$> httpLBS req
    maybeToE "JSON parse error when getting file from course." $ decode resp

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
