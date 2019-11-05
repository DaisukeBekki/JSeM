{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as StrictT    --text
import qualified Data.Text.Lazy as LazyT --text
import qualified JSeM.TSV as J              --jsem
import qualified JSeM.Cmd as J              --jsem

-- | tsv2xml  標準入力からtsv形式のJSeMデータを受け取り、
--            XML形式に変換し、標準出力から出力する。
--            Usage:   cat Toritate.tsv | nkf -w -Lu | stack exec tsv2xml
main :: IO()
main = StrictT.putStrLn =<< J.tidy =<< LazyT.toStrict <$> (J.tsv2XML =<< StrictT.getContents)
