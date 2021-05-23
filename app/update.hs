{-# LANGUAGE OverloadedStrings #-}

-- | tsvフォルダのtsvファイルのうち、同名のxmlが存在しないものについてxmlに変換。
-- | statを実行
-- | README.mdを更新
-- | Usage: stack exec update -- data/v1.0

import Control.Monad (forM_,when)   --base
import System.FilePath ((</>),isExtensionOf,replaceExtensions) --filepath
import System.Environment (getArgs)      --base
import qualified System.Directory as D   --directory
import qualified Data.Text.IO as StrictT --text
import qualified JSeM.XML as J           --jsem
import qualified JSeM.TSV as J           --jsem
import qualified JSeM.Cmd as J           --jsem

-- | dataFolderにある拡張子.txtファイルのすべてについて、
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  tsvFiles <- map (dataFolder </>) <$> filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  -- | TSVデータの整合性をチェック。エラーがあれば終了する。
  J.validateTsvFiles tsvFiles
  -- | XMLデータへの変換
  forM_ tsvFiles $ \tsvFile -> do 
    let xmlFile = replaceExtensions tsvFile "xml"
    xmlFileExists <- D.doesFileExist xmlFile
    if (not xmlFileExists)
      then do
        putStrLn $ "Creating " ++ xmlFile ++ "..."
        J.readFileUtf8 tsvFile >>= J.tsv2jsemData >>= J.jsemData2xml >>= StrictT.writeFile xmlFile
      else do
        tsvTime <- D.getModificationTime tsvFile
        xmlTime <- D.getModificationTime xmlFile
        when (tsvTime > xmlTime) $ do -- 対応するXMLファイルが存在しないか、tsvが更新されている時
          putStrLn $ "Updating " ++ xmlFile ++ "..."
          J.readFileUtf8 tsvFile >>= J.tsv2jsemData >>= J.jsemData2xml >>= StrictT.writeFile xmlFile
  putStrLn "tsv->xml done"
  J.getStat dataFolder >>= StrictT.writeFile ("stat.log")
  putStrLn "Statistics recorded to stat.log"
