{-|
Module      : JSeM
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM (
  JSeMData(..),
  JSeMLabel(..),
  jsemData2Tsv
  ) where

import qualified Data.List as L           --base
import qualified Data.Map as M            --container
import qualified Data.Text as StrictT     --text
import qualified Data.Text.IO as StrictT  --text
import qualified Data.Text.Lazy as LazyT  --text

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id :: StrictT.Text,
  answer :: JSeMLabel,
  phenomena :: [StrictT.Text],
  inference_type :: [StrictT.Text],
  note :: StrictT.Text,
  premises :: [StrictT.Text],
  hypothesis :: StrictT.Text
  } deriving (Eq, Show)

-- | Three answers: yes, no, unknown
data JSeMLabel = YES | NO | UNKNOWN | UNDEF deriving (Eq,Show)

-- |
jsemData2Tsv :: [JSeMData] -> StrictT.Text
jsemData2Tsv jsemdata =
  StrictT.intercalate "\n" (map (\j ->
    StrictT.intercalate "\t" ([
      jsem_id j,
      "",
      "",
      StrictT.pack $ show $ answer j,
      if phenomena j == [] then "" else StrictT.concat ["\"", StrictT.intercalate ", " $ phenomena j, "\""],
      if inference_type j == [] then "" else StrictT.concat ["\"", StrictT.intercalate ", " $ inference_type j, "\""],
      StrictT.concat $ StrictT.lines $ note j
      ] ++ (premises j) ++ [hypothesis j]
      )) jsemdata)
