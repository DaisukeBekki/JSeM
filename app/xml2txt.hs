{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B --bytestring
import qualified Data.Text as T        --text: this is required in order to substitute "Data.Text.Internal" in Text.XML(.Cursor)
import qualified Data.Text.IO as T     --text
import qualified Text.XML as X         --xml-conduit
import qualified Text.XML.Cursor as X  --xml-conduit
import Data.Char

-- | xml2txt: JSeMからのテキスト抜き出し：
-- |   標準出力からJSeM XMLを受け取り、
-- |   "script"タグの内容を順に、１行１文のテキストとして標準出力に出力する。
-- | usage: cat data/JSeM_Toritate.xml | stack exec xml2txt
main :: IO()
main = do
  xml <- B.getContents
  mapM_ (T.putStrLn . (T.filter (\c -> not (isSpace c)))) $
    X.descendant (X.fromDocument $ X.parseLBS_ X.def xml) >>= X.element "script" >>= X.child >>= X.content
