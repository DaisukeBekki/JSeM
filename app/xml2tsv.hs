{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as StrictT --text
import qualified JSeM as J               --jsem
import qualified JSeM.XML as J           --jsem

-- | xml2tsv  標準入力からxml形式のJSeMファイルを受け取り、
--            tsv形式に変換し、標準出力から出力する。
--            Usage:   cat Toritate.xml | nkf -w -Lu | stack exec xml2tsv
main :: IO()
main = StrictT.putStr =<< J.jsemData2Tsv <$> (J.xml2JSeMData =<< StrictT.getContents)
