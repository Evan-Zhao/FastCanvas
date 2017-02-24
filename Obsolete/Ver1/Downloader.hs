module Files.Downloader where

import           Control.Monad              (foldM)
import qualified Data.ByteString.Lazy.Char8 as Bs
import           Network.HTTP.Simple
import           Settings.Settings
import           System.Directory
import           System.FilePath            ((</>))

type File = Bs.ByteString
type FileE = Global File

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

data WriteableNode = FolderNode { wnName :: String } |
                     FileNode   { wnName :: String, wnUrl :: String }

writeNode :: FilePath -> WriteableNode -> Global ()
writeNode fp (FolderNode name)   = lift $ createDirectoryIfMissing False $ fp </> name
writeNode fp (FileNode name url) = downloadTo url (fp </> name)

download :: String -> Global Bs.ByteString
download url = catchE' $ getResponseBody <$> httpLBS (parseRequest_ url)

downloadTo :: String -> String -> Global ()
downloadTo url path = do
    file <- download url
    catchE' $ Bs.writeFile path file

downloadBatchTo :: [String] -> [String] -> IO DownloadState
downloadBatchTo urls paths = foldM writeAndCount emptyState $ zip urls paths

writeAndCount :: DownloadState -> (String, String) -> IO DownloadState
writeAndCount st (url, name) = do
    exist <- doesFileExist name
    if exist then return $ addEx st else do
        putStrLn $ "Writing file " ++ name ++ "..."
        result <- runExceptT $ downloadTo url name
        return $ either (addFaWith st) (const $ addSu st) result
  where
    addEx (DownloadState ex succ' fail' s) = DownloadState (ex+1) succ' fail' s
    addSu (DownloadState ex succ' fail' s) = DownloadState ex (succ'+1) fail' s
    addFaWith (DownloadState ex succ' fail' "") str = DownloadState ex succ' (fail'+1) str
    addFaWith (DownloadState ex succ' fail' s) _ = DownloadState ex succ' (fail'+1) s
