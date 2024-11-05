{-|
Module      : JSeM
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM (
  JSeMData(..),
  JSeMLink(..),
  --DevTest(..),
  YesNo(..),
  readYesNo,
  JSeMLabel(..),
  readJSeMLabel,
  jsemLabel2YesNo,
  jsemData2stat
) where

import qualified Data.List as L       --base
import qualified Data.Map as M        --container
import qualified Data.Text as StrictT --text

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id     :: StrictT.Text,    -- ^ 通し番号。
  link        :: JSeMLink,        -- ^ Link型（下記）の値。
  description :: StrictT.Text,    -- ^ テストの記述。    例：GQ間の関係「ある」系-「すべて」系。
  answer      :: JSeMLabel,       -- ^ 出力の正解。      例：yes/no/unknown
  phenomena   :: [StrictT.Text],  -- ^ 含まれる言語現象。 例："Toritate, -nado (toritate particule)"|
  inference_type :: StrictT.Text, -- ^ 推論の分類。      例： entailment  
  note        :: StrictT.Text,    -- ^ 備考欄。
  premises    :: [StrictT.Text],  -- ^ 前提文（複数可）。 例：ある社員が異動を希望している。
  hypothesis  :: StrictT.Text     -- ^ 帰結文。          例：すべての社員が異動を希望している。
  } deriving (Eq, Show)

-- | A data type for a "link" value.
data JSeMLink = Link {
  resource       :: StrictT.Text, -- ^ リンク先リソース 例：FraCaS 
  link_id        :: StrictT.Text, -- ^ リンク先の対応id 
  translation    :: YesNo,        -- ^ 対訳レベルで一致か
  same_phenomena :: YesNo         -- ^ 現象レベルで一致か
  } | NoLink
  deriving (Eq, Show)

-- | A data type for {"yes" | "no"}
data YesNo = Yes | No | Unk | Other deriving (Eq, Enum, Bounded)
instance Show YesNo where
  show Yes = "yes"
  show No = "no"
  show Unk = "unknown"
  show Other = ""

readYesNo :: StrictT.Text -> YesNo
readYesNo yn =
  case StrictT.toLower yn of
    "yes" -> Yes
    "no" -> No
    "unknown" -> Unk
    _ -> Other

-- | Three labels as answers to a given inference data (yes, no, unknown)
-- | plus four extra labels (undef, unacceptable, weaklyaceptable, infelicitous)
-- | for syntactic, semantic and pragmatic anomaly (respectively).
data JSeMLabel = YES | NO | UNKNOWN | UNDEF | UNACCEPTABLE | WEAKACCEPTABLE | INFELICITOUS | OTHER deriving (Eq, Enum, Bounded)

instance Show JSeMLabel where
  show YES = "yes"
  show NO = "no"
  show UNKNOWN = "unknown"
  show UNDEF = "undef"
  show UNACCEPTABLE = "unacceptable"
  show WEAKACCEPTABLE = "weakacceptable"
  show INFELICITOUS = "infelicitous"
  show OTHER = ""

readJSeMLabel :: StrictT.Text -> JSeMLabel
readJSeMLabel j =
  case StrictT.toLower j of
    "yes" -> YES
    "no" -> NO
    "unknown" -> UNKNOWN
    "undef" -> UNDEF
    "unacceptable" -> UNACCEPTABLE
    "*" -> UNACCEPTABLE
    "weakacceptable" -> WEAKACCEPTABLE
    "?" -> WEAKACCEPTABLE
    "infelicitous" -> INFELICITOUS
    "#" -> INFELICITOUS
    _ -> OTHER

jsemLabel2YesNo :: JSeMLabel -> YesNo
jsemLabel2YesNo YES = Yes
jsemLabel2YesNo NO = No
jsemLabel2YesNo UNKNOWN = Unk
jsemLabel2YesNo UNDEF = Other
jsemLabel2YesNo UNACCEPTABLE = Other
jsemLabel2YesNo WEAKACCEPTABLE = Other
jsemLabel2YesNo INFELICITOUS = Other
jsemLabel2YesNo OTHER = Other

-- | takes a list of "problem" nodes and returns a statistics (in a text format).
jsemData2stat :: [JSeMData] -> StrictT.Text
jsemData2stat [] = StrictT.empty
jsemData2stat jsemdata =
  let phenos = map StrictT.strip $ concat $ map phenomena jsemdata in
  --putStrLn "(0) Comments contained in <problem> tags:"
  --mapM_ StrictT.putStrLn unexpectedcomments
  StrictT.append
    (StrictT.pack $ "(1) The number of <problem> tags: "
      ++ (show $ L.length jsemdata)
      ++ "\n  which contains a <link> (to FraCaS dataset) tag: "
      ++ (show $ L.length $ L.filter (\j -> case link j of
                                              Link _ _ _ _ -> True
                                              NoLink -> False
                                              ) jsemdata)
      ++ "\n(2) The number of yes in translation attr.: "
      ++ (show $ L.length $ L.filter (\j -> case link j of
                                              Link _ _ Yes _ -> True
                                              _ -> False
                                              ) jsemdata)
      ++ "\n(3) The number of occurrences of each phenomena:\n")
    (StrictT.intercalate "\n" $ L.map (\(t,x) -> StrictT.concat ["- ", t,": ",(StrictT.pack $ show x)]) $ M.toList $ M.fromListWith (+) $ L.zip phenos (repeat (1::Integer)))

{-
-- | A data type for a "dev_test" value.
data DevTest = Dev | Test deriving (Eq)
instance Show DevTest where
  show Dev = "dev"
  show Test = "test"
-}
