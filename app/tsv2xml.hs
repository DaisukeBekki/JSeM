{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T --text
import qualified JSeM.TSV2XML as J              --jsem

-- | tsv2xml  標準入力からtsv形式（タブ区切りテキスト）のJSeMデータを受け取り、
-- |          XML形式のJSeMデータを出力する。
-- | usage:   ./TSV2XML.sh GQ追加分....txt
main :: IO()
main = T.putStrLn =<< J.tsv2XML <$> T.getContents

