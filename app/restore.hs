{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_,when)        --base
import System.FilePath ((</>),isExtensionOf,replaceExtensions) --filepath
import System.Environment (getArgs)      --base
import qualified Data.Text.IO as StrictT --text
import qualified System.Directory as D   --directory
import qualified JSeM as J               --jsem
import qualified JSeM.XML as J           --jsem

-- | xml2tsv  与えられたフォルダ（旧データ用）にあるxml形式のJSeMファイルを
-- |          すべてtsv形式に変換し保存する。
-- | Usage:   stack exec xml2tsv -- data/JSeM_beta
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder
  xmlFileNames <- filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  let xmlFiles = map (dataFolder </>) xmlFileNames
  forM_ xmlFiles $ \xmlFile -> do
    let tsvFile = replaceExtensions xmlFile "txt"
    tsvFileExists <- D.doesFileExist tsvFile
    if (not tsvFileExists)
      then do
        putStrLn $ "Creating " ++ tsvFile ++ "..."
        StrictT.writeFile tsvFile =<< J.jsemData2Tsv <$> (J.xmlFile2JSeMData xmlFile)
      else do
        xmlTime <- D.getModificationTime xmlFile
        tsvTime <- D.getModificationTime tsvFile
        when (xmlTime > tsvTime) $ do -- 対応するTSVファイルが存在しないか、xmlが更新されている時
          putStrLn $ "Updating " ++ tsvFile ++ "..."
          StrictT.writeFile tsvFile =<< J.jsemData2Tsv <$> (J.xmlFile2JSeMData xmlFile)
      
