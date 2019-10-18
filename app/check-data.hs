{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)                  --base
import System.Environment (getArgs)           --base
import System.FilePath ((</>), isExtensionOf) --filepath
import qualified System.Directory as D        --directory
import qualified Data.Text as StrictT         --text
import qualified Data.Text.IO as StrictT      --text
--x1import qualified JSeM.TSV2XML as J            --jsem
import qualified JSeM.Cmd as J                --jsem

-- | 引数で与えたフォルダ（相対パス指定）にある拡張子.txtファイルのすべてについて、
-- |   データ形式をチェックする（現在はコラム数が9以上であることを確認のみ）
-- | Usage: stack exec check-data -- /data
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder
  tsvFileNames <- filter (isExtensionOf "txt") <$> D.listDirectory dataFolder
  forM_ tsvFileNames $ \tsvFileName -> do
          putStr $ "Checking entries less than 9 columns in " ++ tsvFileName ++ "..."
          jsemTxt <- J.readFileUtf8 $ dataFolder </> tsvFileName
          forM_ (zip [1..] $ tail $ StrictT.lines jsemTxt) $ \(lineNum,jsemLine) -> do
            let entryNum = length $ StrictT.split (=='\t') jsemLine
            if (entryNum < 9)
              then do
                putStr $ show (lineNum::Int) ++ ": "
                StrictT.putStrLn jsemLine
              else return ()
          putStrLn "done"
