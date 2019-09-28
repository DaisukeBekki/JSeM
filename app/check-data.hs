{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)                  --base
import System.FilePath ((</>), isExtensionOf) --filepath
import System.Directory as D                  --directory
import qualified JSeM.TSV2XML as J            --jsem

dataFolder :: FilePath
dataFolder = "/home/bekki/program/jsem/data"

-- | dataFolderにある拡張子.txtファイルのすべてについて、XMLに変換して標準出力に出力する。
-- | （引数なし）
main :: IO()
main = do
  tsvFileNames <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  forM_ tsvFileNames $ \tsvFileName -> do
          putStr $ "Checking " ++ tsvFileName ++ "..."
          _ <- J.tsvFile2XML $ dataFolder </> tsvFileName
          putStrLn $ "ok"
