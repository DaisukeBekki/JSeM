-- | tsvフォルダのtsvファイルのうち、同名のxmlが存在しないものについてxmlに変換。
-- | statを実行
-- | README.mdを更新

import Control.Monad (forM_,when)       --base
import System.FilePath ((</>),isExtensionOf,replaceExtensions) --filepath
import qualified System.Directory as D  --directory
import qualified Data.Text.Lazy.IO as T --text
import qualified JSeM.TSV2XML as J      --jsem

dataFolder :: FilePath
dataFolder = "/home/bekki/program/jsem/data"

-- | dataFolderにある拡張子.txtファイルのすべてについて、
main :: IO()
main = do
  tsvFiles <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  forM_ tsvFiles $ \tsvFile' -> do 
    let tsvFile = dataFolder </> tsvFile'
        xmlFile = dataFolder </> replaceExtensions tsvFile "xml"
    xmlFileExists <- D.doesFileExist xmlFile
    if (not xmlFileExists)
      then do
        putStrLn $ "Creating " ++ xmlFile ++ "..."
        J.tsvFile2XML tsvFile >>= T.writeFile xmlFile
      else do
        tsvTime <- D.getModificationTime tsvFile
        xmlTime <- D.getModificationTime xmlFile
        when (tsvTime > xmlTime) $ do -- 対応するXMLファイルが存在しないか、tsvが更新されている時
          putStrLn $ "Updating " ++ xmlFile ++ "..."
          J.tsvFile2XML tsvFile >>= T.writeFile xmlFile