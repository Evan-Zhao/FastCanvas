module Files.Node.FSNode where

import qualified Data.ByteString.Lazy.Char8 as Bs
import           Settings.Imports
import           System.Directory
import           System.FilePath            ((</>))

type File = Bs.ByteString

downloadTo :: String -> String -> Global ()
downloadTo url path = do
    file <- simpleHttpLBS' url
    catchIOE $ Bs.writeFile path file

data FSNode = FolderNode {
                relativePath  :: String
              } |
              FileNode   {
                relativePath :: String,
                nodeUrl      :: String
              } deriving (Eq, Show)

writeNode :: FilePath -> FSNode -> Global ()
writeNode parent (FolderNode rel)   = liftIO $ createDirectoryIfMissing False $ parent </> rel
writeNode parent (FileNode rel url) = downloadTo url (parent </> rel)

doesWrite :: FilePath -> FSNode -> GlobalNoE Bool
doesWrite path node = liftIO $ doesPathExist $ path </> relativePath node
