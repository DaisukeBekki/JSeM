{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)      --base
import qualified Data.Text.IO as StrictT --text
import qualified JSeM.XML as J           --jsem

-- | xml2tsv  xml形式のJSeMファイルを受け取り、
-- |          tsv形式に変換、標準出力から出力する。
-- | Usage:   stack exec xml2tsv -- data/Toritate.xml
main :: IO()
main = do
  (xmlfile:_) <- getArgs
  StrictT.putStr =<< J.jsemData2Tsv <$> (J.xmlFile2JSeMData xmlfile)
