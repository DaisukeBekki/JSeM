{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)           --base
import System.Environment (getArgs)    --base
import qualified Data.Text as T        --text: this is required in order to substitute "Data.Text.Internal" in Text.XML(.Cursor)
import qualified Data.Text.IO as T     --text
import qualified JSeM as J             --jsem
import qualified JSeM.XML as J         --jsem

-- | xml2txt  JSeMからのテキスト抜き出し：
-- |          標準出力からJSeM XMLを受け取り、
-- |          "script"タグの内容を順に、１行１文のテキストとして標準出力に出力する。
-- | Usage:   cat data/JSeM_Toritate.xml | stack exec xml2txt
main :: IO()
main = do
  (xmlfile:_) <- getArgs
  jsemdata <- J.xmlFile2JSeMData xmlfile
  forM_ jsemdata $ \j -> do
    T.putStrLn $ T.concat ["[", J.jsem_id j, "]"]
    mapM_ T.putStrLn $ J.premises j
    T.putStrLn $ J.hypothesis j
    print $ J.answer j
    putChar '\n'

