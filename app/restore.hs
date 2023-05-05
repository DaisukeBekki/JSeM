{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_,when)        --base
import System.FilePath (replaceExtensions) --filepath
import qualified Data.Text.IO as T       --text
import qualified System.Directory as D   --directory
import JSeM.Directory (findFilesFromDirectory) --jsem
import qualified JSeM.XML as J           --jsem
import qualified JSeM.TSV as J           --jsem

-- | 与えられたフォルダ（旧データ用）にあるxml形式のJSeMファイルのうち
-- | 自身より新しい同名のtsvファイルが存在しないものについて、
-- | tsv形式（拡張子は.txt）に変換し、同フォルダに保存する。
-- | Usage:   stack run restore -- data/JSeM_beta
main :: IO()
main = do
  (dataFolder,xmlFiles) <- findFilesFromDirectory "xml"
  forM_ xmlFiles $ \xmlFile -> do
    let tsvFile = replaceExtensions xmlFile "txt"
    tsvFileExists <- D.doesFileExist tsvFile
    if (not tsvFileExists)
      then do
        putStrLn $ "Creating " ++ tsvFile ++ "..."
        T.writeFile tsvFile =<< J.jsemData2tsv <$> (J.xmlFile2jsemData xmlFile)
      else do
        xmlTime <- D.getModificationTime xmlFile
        tsvTime <- D.getModificationTime tsvFile
        when (xmlTime > tsvTime) $ do -- 対応するTSVファイルが存在しないか、xmlが更新されている時
          putStrLn $ "Updating " ++ tsvFile ++ "..."
          T.writeFile tsvFile =<< J.jsemData2tsv <$> (J.xmlFile2jsemData xmlFile)
  putStrLn $ "Converting xml->tsv done in " ++ dataFolder
