module Downloader1 where

import qualified Data.ByteString.Lazy.Char8 as Bs
import           Data.Foldable
import           Settings
import           System.Directory

type File = Bs.ByteString
type FileE = MainExcept File

data DownloadState = DownloadState {
    existN        :: Int,
    succeedN      :: Int,
    failedN       :: Int,
    failureSample :: String
}

instance Show DownloadState where
    show (DownloadState 0 0 0 _) = "No files to download."
    show (DownloadState _ 0 0 _) = "All file exists; no change applied."
    show (DownloadState 0 _ 0 _) = "Successfully downloaded all files."
    show (DownloadState 0 0 _ _) = "All downloads failed."
    show (DownloadState e s f r) = succ' ++ fail' ++ exist where
        succ' = "Successfully downloaded " ++ show s ++ " files."
        fail' = if f /= 0 then "\n" ++ show f ++
                               " files failed when downloading; faliure reasons include" ++ r
                          else ""
        exist = if e /= 0 then "\n" ++ show e ++ " files were already there; no change applied." else ""

emptyState :: DownloadState
emptyState = DownloadState 0 0 0 ""

isAllFail :: DownloadState -> Bool
isAllFail (DownloadState 0 0 f _) = f /= 0
isAllFail _                       = False

data WriteableNode = FolderNode {
                        wnFullPath  :: String,
                        wnFileUrl   :: ByteString,
                        wnFolderUrl :: ByteString
                     } |
                     FileNode   {
                        wnFullPath :: String,
                        wnUrl      :: String
                     }

download :: String -> MainExcept Bs.ByteString
download url = catchMainE $ getResponseBody <$> httpLBS (parseRequest_ url)

downloadTo :: String -> String -> MainExcept ()
downloadTo url path = do
    file <- download url
    catchMainE $ Bs.writeFile path file

writeNode :: WriteableNode -> MainExcept ()
writeNode (FolderNode name _ _) = lift $ createDirectoryIfMissing False name
writeNode (FileNode name url)   = downloadTo url name

downloadTree :: (Foldable t) => t WriteableNode -> IO DownloadState
downloadTree = foldlM writeAndCount emptyState

writeAndCount :: DownloadState -> WriteableNode -> IO DownloadState
writeAndCount st node = do
    exist <- doesPathExist path
    if exist then return $ addEx st else do
        putStrLn $ "Writing on path " ++ path ++ "..."
        result <- runExceptT $ writeNode node
        return $ either (addFaWith st) (const $ addSu st) result
  where
    path = wnName node
    addEx (DownloadState ex succ' fail' s) = DownloadState (ex+1) succ' fail' s
    addSu (DownloadState ex succ' fail' s) = DownloadState ex (succ'+1) fail' s
    addFaWith (DownloadState ex succ' fail' "") str = DownloadState ex succ' (fail'+1) str
    addFaWith (DownloadState ex succ' fail' s) _ = DownloadState ex succ' (fail'+1) s
