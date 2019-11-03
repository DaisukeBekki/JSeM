{-|
Module      : JSeM.XML
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.XML (
  xmlFile2JSeMData,
  xmlFile2problems,
  problems2stat
  ) where

import qualified Data.List as L           --base
import qualified Data.Map as M            --container
import qualified Data.Text as StrictT     --text
--import qualified Data.Text.IO as StrictT  --text
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit
import JSeM as J                          --jsem
import qualified JSeM.Cmd as J            --jsem

xmlFile2problems :: FilePath -> IO([X.Cursor])
xmlFile2problems xmlfile = do
  cursor <- X.fromDocument <$> X.parseText_ X.def <$> LazyT.fromStrict <$> J.readFileUtf8 xmlfile
  return $ X.child cursor >>= X.element "problem"

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
xmlFile2JSeMData :: FilePath -> IO([JSeMData])
xmlFile2JSeMData filepath = xmlFile2problems filepath >>= mapM problem2JSeMData

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
      premises = map (StrictT.concat . StrictT.lines) $ [problem] >>= X.child >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      hypothesis = StrictT.concat $ map (StrictT.concat . StrictT.lines) $ [problem] >>= X.child >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content
      answertext = StrictT.concat $ [problem] >>= X.laxAttribute "answer"
  answer <- case answertext of
              "yes" -> return YES
              "no" -> return NO
              "unknown" -> return UNKNOWN
              "undef" -> return UNDEF
              _ -> fail $ StrictT.unpack $ StrictT.concat ["#", jsem_id, " has an undefined answer: ", answertext]
  return $ JSeMData jsem_id answer phenomena inference_type note premises hypothesis

problems2stat :: [X.Cursor] -> StrictT.Text
problems2stat [] = StrictT.empty
problems2stat problems =
  let linkNodes = problems >>= X.child >>= X.element "link"
      --unexpectedcomments = problems >>= X.child >>= X.checkNode isComment
      transYes = linkNodes >>= X.laxAttribute "translation"
      linked = linkNodes >>= X.laxAttribute "link_id" 
      phenomena = problems >>= X.laxAttribute "phenomena" >>= StrictT.split (==',')
      phen2 = L.map StrictT.strip phenomena in
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

