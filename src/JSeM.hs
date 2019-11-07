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

import qualified Data.Text as StrictT     --text

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id :: StrictT.Text,
  resource :: StrictT.Text,
  linkid :: StrictT.Text,
  translation :: StrictT.Text,
  same_phenomena :: StrictT.Text,
  description :: StrictT.Text,
  answer :: JSeMLabel,
  phenomena :: [StrictT.Text],
  inference_type :: [StrictT.Text],
  note :: StrictT.Text,
  premises :: [StrictT.Text],
  hypothesis :: StrictT.Text
  } deriving (Eq, Show)

-- | Three labels as answers to a given inference data (yes, no, unknown) plus four extra labels (undef, unacceptable, weaklyaceptable, infelicitous) for syntactic, semantic and pragmatic anomaly.
data JSeMLabel = YES | NO | UNKNOWN | UNDEF | UNACCEPTABLE | WEAKACCEPTABLE | INFELICITOUS deriving (Eq)

instance Show JSeMLabel where
  show YES = "yes"
  show NO = "no"
  show UNKNOWN = "unknown"
  show UNDEF = "undef"
  show UNACCEPTABLE = "unacceptable"
  show WEAKACCEPTABLE = "weakacceptable"
  show INFELICITOUS = "infelicitous"

-- | Convert a list of JSeMData into Data.TEXT (in TSV format)
jsemData2Tsv :: [JSeMData] -> StrictT.Text
jsemData2Tsv jsemdata =
  StrictT.intercalate "\n" (map (\j ->
    StrictT.intercalate "\t" ([
      jsem_id j,
      resource j,
      linkid j,
      translation j,
      same_phenomena j,
      description j,
      "",
      StrictT.pack $ show $ answer j,
      if phenomena j == [] then "" else StrictT.concat ["\"", StrictT.intercalate ", " $ phenomena j, "\""],
      if inference_type j == [] then "" else StrictT.concat ["\"", StrictT.intercalate ", " $ inference_type j, "\""],
      StrictT.concat $ StrictT.lines $ note j
      ] ++ (premises j) ++ [hypothesis j]
      )) jsemdata)

