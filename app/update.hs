-- | tsvフォルダのtsvファイルのうち、同名のxmlが存在しないものについてxmlに変換。
-- | statを実行
-- | README.mdを更新
-- | Usage: stack exec update -- data

import Control.Monad (forM_,when)       --base
import System.FilePath ((</>),isExtensionOf,replaceExtensions) --filepath
import System.Environment (getArgs)     --base
import qualified System.Directory as D  --directory
import qualified Data.Text.Lazy.IO as T --text
import qualified JSeM.TSV2XML as J      --jsem

-- | dataFolderにある拡張子.txtファイルのすべてについて、
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  tsvFileNames <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  let tsvFiles = map (dataFolder </>) tsvFileNames
  -- | TSVデータの整合性をチェック。エラーがあれば終了する。
  J.validateTsvFiles tsvFiles
  -- | XMLデータへの変換
  forM_ tsvFiles $ \tsvFile -> do 
    let xmlFile = replaceExtensions tsvFile "xml"
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
