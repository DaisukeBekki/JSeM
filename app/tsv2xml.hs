{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)              --base
import qualified System.Environment as E --base
import qualified JSeM.TSV2XML as J       --jsem

-- | tsv2xml  標準入力からtsv形式（タブ区切りテキスト）のJSeMデータ（のファイル名）を受け取り、
-- |          XML形式のJSeMデータを出力する。
-- | usage:   ./tsv2xml.sh data/JSeM_Toritate.tsv
main :: IO()
main = do
  args <- E.getArgs
  when (length args == 0) $ fail "Needs a tsv filepath as an argument"
  J.printXML =<< J.tsvFile2XML (head args)

