{-|
Module      : JSeM.XML
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.XML (
  xml2JSeMData,
  xmlFile2JSeMData,
  problems2stat
  ) where

import Control.Monad ((>=>))              --base
import qualified Data.List as L           --base
import qualified Data.Map as M            --container
import qualified Data.Text as StrictT     --text
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit
import JSeM as J                          --jsem
import qualified JSeM.Cmd as J            --jsem

--xmlFile2problems :: FilePath -> IO([X.Cursor])
--xmlFile2problems xmlfile = do
--  cursor <- X.fromDocument <$> X.parseText_ X.def <$> LazyT.fromStrict <$> J.readFileUtf8 xmlfile
--  return $ X.child cursor >>= X.element "problem"

-- | takes a JSeM text (XML format) and returns a list of 'JSeMData'.
xml2JSeMData :: StrictT.Text -> IO([JSeMData])
xml2JSeMData = xml2problems >=> mapM problem2JSeMData

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
xmlFile2JSeMData :: FilePath -> IO([JSeMData])
xmlFile2JSeMData = J.readFileUtf8 >=> xml2problems >=> mapM problem2JSeMData

-- | takes a JSeM text (XML format) and returns a list of "problem" nodes.
xml2problems :: StrictT.Text -> IO([X.Cursor])
xml2problems xml = do
  let cursor = X.fromDocument $ X.parseText_ X.def $ LazyT.fromStrict xml
  return $ X.child cursor >>= X.element "problem"
  
-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  
-- Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, 
-- and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), 
-- which is a standard format of text data in lightblue.
problem2JSeMData :: X.Cursor -> IO(JSeMData)
problem2JSeMData problem = do
  let j_jsem_id = StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id"
      j_phenomena = map (StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==',')
      j_inference_type = map (StrictT.strip) $ [problem] >>= X.laxAttribute "inference_type" >>= StrictT.split (==',')
      j_note = StrictT.concat $ [problem] >>= X.child >>= X.element "note" >>= X.child >>= X.content
      j_premises = map StrictT.strip $ [problem] >>= X.child >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      j_hypothesis = StrictT.concat $ map StrictT.strip $ [problem] >>= X.child >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      answertext = StrictT.concat $ [problem] >>= X.laxAttribute "answer"
  j_answer <- case answertext of
              "yes" -> return YES
              "no" -> return NO
              "unknown" -> return UNKNOWN
              "undef" -> return UNDEF
              "unacceptable" -> return UNACCEPTABLE
              "weakacceptable" -> return WEAKACCEPTABLE
              "infelicitous" -> return INFELICITOUS
              _ -> fail $ StrictT.unpack $ StrictT.concat ["#", j_jsem_id, " has an undefined answer: ", answertext]
  return $ JSeMData j_jsem_id j_answer j_phenomena j_inference_type j_note j_premises j_hypothesis

-- | takes a list of "problem" nodes and returns a statistics (in a text format).
problems2stat :: [X.Cursor] -> StrictT.Text
problems2stat [] = StrictT.empty
problems2stat problems =
  let linkNodes = problems >>= X.child >>= X.element "link"
      --unexpectedcomments = problems >>= X.child >>= X.checkNode isComment
      transYes = linkNodes >>= X.laxAttribute "translation"
      linked = linkNodes >>= X.laxAttribute "link_id" 
      phenomena_ = problems >>= X.laxAttribute "phenomena" >>= StrictT.split (==',')
      phen2 = L.map StrictT.strip phenomena_ in
  --putStrLn "(0) Comments contained in <problem> tags:"
  --mapM_ StrictT.putStrLn unexpectedcomments
  StrictT.append
    (StrictT.pack $ "(1) The number of <problem> tags: "
      ++ (show $ L.length $ problems)
      ++ "\n  which contains a <link> (to FraCaS dataset) tag with link_id: "
      ++ (show $ L.length $ linked)
      ++ "\n(2) The number of yes in translation attr.: "
      ++ (show $ L.length $ L.filter (=="yes") transYes)
      ++ "\n(3) The number of occurrences of each phenomena:\n")
    (StrictT.intercalate "\n" $ L.map (\(t,x) -> StrictT.concat ["- ", t,": ",(StrictT.pack $ show x)]) $ M.toList $ M.fromListWith (+) $ L.zip phen2 (repeat (1::Integer)))

