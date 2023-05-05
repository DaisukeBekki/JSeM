module JSeM.Directory (
  findFilesFromDirectory
  ) where

import System.Environment (getArgs)          --base
import System.FilePath ((</>),isExtensionOf) --filepath
import qualified System.Directory as D       --directory

-- | コマンドライン引数から与えられたディレクトリにあるファイルのうち、
-- | 拡張子がextであるもののフルパスのリストを返す
findFilesFromDirectory :: String -> IO (FilePath,[FilePath])
findFilesFromDirectory ext = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder
  fileNames <- D.listDirectory dataFolder
  let filePaths = map (dataFolder </>) $ filter (isExtensionOf ext) $ fileNames
  return (dataFolder, filePaths)
