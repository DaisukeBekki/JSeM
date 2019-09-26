{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)            --base
import System.Environment (getArgs)    --base
import qualified Data.Text as T        --text: this is required in order to substitute "Data.Text.Internal" in Text.XML(.Cursor)
import qualified Data.Text.IO as T     --text
import qualified Text.XML as X         --xml-conduit
import qualified Text.XML.Cursor as X  --xml-conduit
import Data.Char

-- | JSeMからのテキスト抜き出し：JSeM XMLファイル名を受け取り、
-- |                        １行１文のテキストファイルを出力する。
-- | usage: stack exec jsem2text -- data/JSeM_Toritate.xml
main :: IO()
main = do
  args <- getArgs
  when (length args == 0) $ fail "Needs a tsv filepath as an argument"
  doc <- X.readFile X.def $ head args
  let scripts = X.descendant (X.fromDocument doc) >>= X.element "script" >>= X.child >>= X.content
  mapM_ (T.putStrLn . (T.filter (\c -> not (isSpace c)
                                       && c /= '。'
                                       && c/= '、')
                                       )) scripts

