{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)         --base
import System.Environment (getArgs) --base
import qualified JSeM.TSV2XML as J  --jsem

-- | tsv2xml  標準入力からtsv形式（タブ区切りテキスト）のJSeMファイル名を受け取り、
-- |          XML形式のJSeMデータを出力する。
-- | usage:   stack exec tsv2xml -- data/JSeM_Toritate.tsv
main :: IO()
main = do
  args <- getArgs
  when (length args == 0) $ fail "Needs a tsv filepath as an argument"
  J.printXML =<< J.tsvFile2XML (head args)

