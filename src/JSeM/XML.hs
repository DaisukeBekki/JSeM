{-|
Module      : JSeM.XML
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.XML (
  JSeMData(..),
  JSeMLabel(..),
  xmlFile2JSeMData,
  jsemData2Tsv
  ) where

import qualified Data.Text as StrictT     --text
import qualified Data.Text.IO as StrictT     --text
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit

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
      note j
      ] ++ (premises j) ++ [hypothesis j]
      )) jsemdata)

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
xmlFile2JSeMData :: FilePath -> IO([JSeMData])
xmlFile2JSeMData filepath = do
  cursor <- X.fromDocument <$> X.readFile X.def filepath
  mapM problem2JSeMData $ X.child cursor >>= X.element "problem" 

-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  
-- Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, 
-- and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), 
-- which is a standard format of text data in lightblue.
problem2JSeMData :: X.Cursor -> IO(JSeMData)
problem2JSeMData problem = do
  let jsem_id = StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id"
      phenomena = map (StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==',')
      inference_type = map (StrictT.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= StrictT.split (==',')
      note = StrictT.concat $ [problem] >>= X.child >>= X.element "note" >>= X.child >>= X.content
      premises = map (StrictT.strip . StrictT.replace "\r\n" "") $ [problem] >>= X.child >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      hypothesis = StrictT.concat $ map (StrictT.strip . StrictT.replace "\r\n" "") $ [problem] >>= X.child >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      answertext = StrictT.concat $ [problem] >>= X.laxAttribute "answer"
  answer <- case answertext of
              "yes" -> return YES
              "no" -> return NO
              "unknown" -> return UNKNOWN
              "undef" -> return UNDEF
              _ -> fail $ StrictT.unpack $ StrictT.concat ["#", jsem_id, " has an undefined answer: ", answertext]
  return $ JSeMData jsem_id answer phenomena inference_type note premises hypothesis
