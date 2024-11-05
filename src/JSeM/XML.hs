{-|
Module      : JSeM.XML
Copyright   : (c) Daisuke Bekki, 2017
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.XML (
  jsemData2xml,
  xml2jsemData,
  xmlFile2jsemData,
  getStat
  ) where

import Prelude hiding (read)              --base
import Control.Monad ((<=<))              --base
--import Text.Read (read)                     --base
import System.FilePath ((</>),isExtensionOf) --filepath
import qualified System.Directory as D    --directory
import qualified Data.Map as M            --container
import qualified Data.Text as StrictT     --text
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X            --xml-conduit
import qualified Text.XML.Cursor as X     --xml-conduit
import JSeM (JSeMData(..),JSeMLink(..),jsemData2stat,readJSeMLabel,readYesNo)   --jsem
import qualified JSeM.Cmd as J            --jsem

-- | タグ名をXMLタグ名に変換する。
tag :: StrictT.Text -> X.Name
tag tagname = X.Name tagname Nothing Nothing

-- | タグ名、ノードのリストを受け取り、XML形式のテキストに変換。
jsemData2xml :: [JSeMData] -> IO(StrictT.Text)
jsemData2xml jsemdata = do
  nodes <- mapM jsemData2xmlNode jsemdata
  J.tidy $ LazyT.toStrict $ nodes2xml "jsem-dataset" nodes
  where nodes2xml name nodes =
          X.renderText X.def $ X.Document 
                         (X.Prologue
                            []
                            (Just $ X.Doctype "jsem-dataset SYSTEM \"jsem.dtd\"" Nothing) -- <!DOCTYPE jsem-dataset SYSTEM "jsem.dtd">
                            [X.MiscInstruction $ X.Instruction "xml-stylesheet" "type=\"text/xsl\" href=\"jsem.xsl\"" ] -- <?xml-stylesheet type="text/xsl" href="jsem.xsl"?>
                            ) 
                         (X.Element (tag name) (M.fromList []) nodes)
                         []

jsemData2xmlNode :: JSeMData -> IO(X.Node)
jsemData2xmlNode j = do
  return $ X.NodeElement $ X.Element 
                    (tag "problem")
                    (M.fromList 
                       [("jsem_id", jsem_id j),
                        ("answer", StrictT.pack $ show $ answer j),
                        ("language","jp"),
                        ("phenomena", StrictT.intercalate "," $ phenomena j),
                        ("inference_type",inference_type j)
                       ])
                    ((case link j of
                        Link resource' link_id' translation' same_phenomena' ->
                          [X.NodeElement $ X.Element 
                               (tag "link")
                               (M.fromList [("resource", resource'),
                                            ("link_id", link_id'),
                                            ("translation", StrictT.pack $ show $ translation'),
                                            ("same_phenomena", StrictT.pack $ show $ same_phenomena')
                                            ])
                               []]
                        NoLink -> []
                     ) ++
                     [X.NodeElement $ X.Element
                        (tag "description")
                        (M.fromList [])
                        [X.NodeContent $ description j]
                     ] ++
                     ((flip map) (zip (premises j) [1..]) $ \(premise,i) ->  
                       X.NodeElement $ X.Element
                          (tag "p")
                          (M.fromList [("idx",StrictT.pack $ show (i::Int))])
                          [X.NodeElement $ X.Element
                             (tag "script")
                             (M.fromList [])
                             [X.NodeContent premise]  
                          ]
                     ) ++  
                     [X.NodeElement $ X.Element
                        (tag "h")
                        (M.fromList [])
                        [X.NodeElement $ X.Element
                           (tag "script")
                           (M.fromList [])
                           [X.NodeContent $ hypothesis j]
                        ],
                      X.NodeElement $ X.Element
                        (tag "note")
                        (M.fromList [])
                        [X.NodeContent $ note j]
                      ])

-- | takes a JSeM text (XML format) and returns a list of 'JSeMData'.
xml2jsemData :: StrictT.Text -> IO([JSeMData])
xml2jsemData = (mapM problem2jsemData) <=< xml2problems

-- | takes a file path of a JSeM file (XML format) and returns a list of 'JSeMData'.
xmlFile2jsemData :: FilePath -> IO([JSeMData])
xmlFile2jsemData = (mapM problem2jsemData) <=< xml2problems <=< J.readFileUtf8

-- | takes a JSeM text (XML format) and returns a list of "problem" nodes.
xml2problems :: StrictT.Text -> IO([X.Cursor])
xml2problems xml = do
  let cursor = X.fromDocument $ X.parseText_ X.def $ LazyT.fromStrict xml
  return $ X.child cursor >>= X.element "problem"
  
-- | takes a "problem" node in a JSeM file and translates it to a 'JSeMData'.  
-- Note that the xml-conduit package uses Data.Text (=strict texts) as internal format of text data, 
-- and `problem2JSeMData` function converts them to Data.Text.Lazy (=lazy texts), 
-- which is a standard format of text data in lightblue.
problem2jsemData :: X.Cursor -> IO(JSeMData)
problem2jsemData problem = do
  let linktag = [problem] >>= X.child >>= X.element "link" 
  return $ JSeMData
             (StrictT.concat $ [problem] >>= X.laxAttribute "jsem_id")
             (Link 
               (StrictT.concat $ linktag >>= X.laxAttribute "resource")
               (StrictT.concat $ linktag >>= X.laxAttribute "link_id")
               (readYesNo $ StrictT.concat $ linktag >>= X.laxAttribute "translation")
               (readYesNo $ StrictT.concat $ linktag >>= X.laxAttribute "same_phenomena"))
             (StrictT.concat $ [problem] >>= X.child >>= X.element "description" >>= X.child >>= X.content)
             (readJSeMLabel $ StrictT.concat $ [problem] >>= X.laxAttribute "answer")
             (map (StrictT.strip) $ [problem] >>= X.laxAttribute "phenomena" >>= StrictT.split (==','))
             (StrictT.concat $ [problem] >>= X.laxAttribute "inference_type")
             (StrictT.concat $ [problem] >>= X.child >>= X.element "note" >>= X.child >>= X.content)
             (map StrictT.strip $ [problem] >>= X.child >>= X.element "p" >>= X.child >>= X.element "script" >>= X.child >>= X.content)
             (StrictT.concat $ map StrictT.strip $ [problem] >>= X.child >>= X.element "h" >>= X.child >>= X.element "script" >>= X.child >>= X.content)

-- | JSeM統計情報出力プログラム
-- |   標準入力からxmlフォーマットのJSeMファイルを受け取り、以下を標準出力に出力する：
-- | (1)全<problem>要素のうち、内部に<link>要素のあるものとないものの出現数と割合
-- | (2)全<link>要素のうち、translation属性がyesのものとそうでないものの出現数と割合
-- | (3)<problem>要素内のphenomena属性の値のリストと出現数
getStat :: FilePath -> IO(StrictT.Text)
getStat dataFolder = do
  _ <- D.doesDirectoryExist dataFolder 
  xmlFiles <- map (dataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  jsemData2stat <$> concat <$> mapM xmlFile2jsemData xmlFiles 


{-
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
-}
