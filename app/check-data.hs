{-# LANGUAGE OverloadedStrings #-}

import System.FilePath ((</>), isExtensionOf) --filepath
import System.Directory as D            --directory
import qualified Data.Text.Lazy.IO as T --text
import qualified JSeM.TSV2XML as J      --jsem

dataFolder :: FilePath
dataFolder = "/home/bekki/program/jsem/data"

-- | dataFolderにある拡張子.txtファイルのすべてについて、XMLに変換して標準出力に出力する。
-- | （引数なし）
main :: IO()
main = do
  tsvFiles <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  xmls <- mapM (\filename -> J.tsvFile2XML $ dataFolder </> filename) tsvFiles
  mapM_ T.putStrLn xmls
