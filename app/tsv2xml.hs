{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T   --text
import qualified JSeM.TSV as J       --jsem
import qualified JSeM.XML as J       --jsem

-- | tsv2xml  標準入力からtsv形式のJSeMデータを受け取り、
--            XML形式に変換し、標準出力から出力する。
--            Usage:   cat Toritate.tsv | nkf -w -Lu | stack exec tsv2xml
main :: IO()
main = T.putStrLn =<< J.jsemData2xml =<< J.tsv2jsemData =<< T.getContents
