{-|
Module      : JSeM.XML2JSemData
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.XML2JSeMData (
  JSeMData(..),
  JSeMLabel(..),
  xmlFile2JSeMData
  ) where

import qualified Data.Text as StrictT     --text
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit

-- | A data type for each JSeM entry.
data JSeMData = JSeMData {
  jsem_id :: StrictT.Text,
  answer :: JSeMLabel,
  phenomena :: [StrictT.Text],
  inference_type :: [StrictT.Text],
  premise :: [StrictT.Text],
  hypothesis :: StrictT.Text
  } deriving (Eq, Show)

-- | Three answers: yes, no, unknown
data JSeMLabel = YES | NO | UNKNOWN | UNDEF deriving (Eq,Show)

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
xmlFile2JSeMData :: LazyT.Text -> IO([JSeMData])
xmlFile2JSeMData filepath = 
  let cursor = X.fromDocument $ X.parseText_ X.def filepath
      problemNodes = X.child cursor >>= X.element "problem" in
  mapM problem2JSeMData problemNodes

-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  
-- Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, 
-- and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), 
-- which is a standard format of text data in lightblue.
problem2JSeMData :: X.Cursor -> IO(JSeMData)
problem2JSeMData problem = do
  let children = [problem] >>= X.child
      answertext = StrictT.concat $ [problem] >>= X.laxAttribute "answer"
      idtext = StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id"
  ans <- case answertext of
           "yes" -> return YES
           "no" -> return NO
           "unknown" -> return UNKNOWN
           "undef" -> return UNDEF
           _ -> ioError $ userError $ StrictT.unpack $ StrictT.concat ["#", idtext, " has an undefined answer: ", answertext]
  return JSeMData {
    jsem_id = idtext,
    answer = ans,
    phenomena = map (StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==','),
    inference_type = map (StrictT.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= StrictT.split (==','),
    premise = map (StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content,
    hypothesis = StrictT.concat $ map (StrictT.strip . StrictT.replace "\r\n" "") $ children >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
    }

