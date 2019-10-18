{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)                  --base
import System.Environment (getArgs)           --base
import System.FilePath ((</>), isExtensionOf) --filepath
import qualified System.Directory as D        --directory
import qualified Data.Text as StrictT         --text
import qualified Data.Text.IO as StrictT      --text
--x1import qualified JSeM.TSV2XML as J            --jsem
import qualified JSeM.Cmd as J                --jsem

-- | dataFolderにある拡張子.txtファイルのすべてについて、XMLに変換して標準出力に出力する。
-- | Usage: stack exec check-data -- /data
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder
  tsvFileNames <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  forM_ tsvFileNames $ \tsvFileName -> do
          putStr $ "Checking " ++ tsvFileName ++ "..."
          jsemTxt <- J.readFileUtf8 $ dataFolder </> tsvFileName
          forM_ (tail $ StrictT.lines jsemTxt) $ \jsemLine -> do
            let entryNum = length $ StrictT.split (=='\t') jsemLine
            if (entryNum < 9)
              then do
                StrictT.putStrLn jsemLine
                putStrLn "the above entry has less than 9 columns"
              else return ()
          putStrLn "ok"
