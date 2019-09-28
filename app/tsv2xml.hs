{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as StrictT    --text
import qualified Data.Text.Lazy.IO as LazyT --text
import qualified JSeM.TSV2XML as J          --jsem

-- | tsv2xml  標準入力からtsv形式のJSeMデータを受け取り、
-- |          XML形式に変換し、
-- |          標準出力から出力する。
-- | Usage:   cat data/JSeM_Toritate.tsv | tsv2xml.sh
main :: IO()
main = LazyT.putStrLn =<< J.tsv2XML =<< StrictT.getContents
