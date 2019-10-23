{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)             --base
import System.FilePath ((</>),isExtensionOf,replaceExtensions) --filepath
import System.Environment (getArgs)      --base
import qualified Data.Text.IO as StrictT --text
import qualified System.Directory as D   --directory
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
    StrictT.writeFile tsvFile =<< J.jsemData2Tsv <$> (J.xmlFile2JSeMData xmlFile)
