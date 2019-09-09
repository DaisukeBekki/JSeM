{-# LANGUAGE OverloadedStrings #-}

-- | JSeMからのテキスト抜き出し：JSeM XMLファイルを受け取り、
-- |                        １行１文のテキストファイルを出力する。
-- |   to build: ghc -package text-1.2.1.1 JSem2Text.hs 

import qualified Data.Text as T       --text: this is required in order to substitute "Data.Text.Internal" in Text.XML(.Cursor)
import qualified Data.Text.IO as T    --text
import qualified Text.XML as X        --xml-conduit
import qualified Text.XML.Cursor as X --xml-conduit
import Data.Char

main :: IO()
main = do
  doc <- X.readFile X.def "JSeM_beta_150415.xml"
  let scripts = X.descendant (X.fromDocument doc) >>= X.element "script" >>= X.child >>= X.content
  mapM_ (T.putStrLn . (T.filter (\c -> not (isSpace c)
                                       && c /= '。'
                                       && c/= '、')
                                       )) scripts

