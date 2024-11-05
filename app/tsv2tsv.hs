{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T   --text
import qualified JSeM.TSV as J       --jsem
import qualified JSeM.XML as J       --jsem

-- | tsv2tsv  標準入力からtsv形式のJSeMデータを受け取り、
--            pandas形式のtsvに変換し、標準出力から出力する。
--            Usage:   cat Toritate.tsv | nkf -w -Lu | stack exec tsv2tsv
main :: IO()
main = (T.putStrLn . J.jsemData2tsv') =<< J.tsv2jsemData =<< T.getContents

-- | Convert a list of JSeMData into Data.TEXT (in Python-like TSV format)
jsemData2tsv' :: [JSeMData] -> StrictT.Text
jsemData2tsv' jsemdata =
  StrictT.concat [
    "answer\tconclusion\tpremise1\tpremise2\n",
    StrictT.intercalate "\n" (map (\j ->
      StrictT.intercalate "\t" ([
        StrictT.pack $ show $ answer j
        ] ++ [hypothesis j] ++ (premises j) 
        )) jsemdata)
        ]
