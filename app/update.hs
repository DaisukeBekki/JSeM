{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_,when)   --base
import System.FilePath (replaceExtensions) --filepath
import qualified System.Directory as D   --directory
import qualified Data.Text.IO as StrictT --text
import JSeM.Directory (findFilesFromDirectory) --jsem
import qualified JSeM.XML as J           --jsem
import qualified JSeM.TSV as J           --jsem
import qualified JSeM.Cmd as J           --jsem

-- | 与えられたフォルダ（新データ用）にある（拡張子.txtの）TSVファイルのうち
-- | 同名のxmlが存在しないものについてxmlに変換し、同フォルダに保存する。
-- | その後、statを実行し、
-- | README.mdを更新？
-- | Usage:   stack run update -- data/v1.0
main :: IO()
main = do
  (dataFolder,tsvFiles) <- findFilesFromDirectory "txt"
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
