module JSeM (
  tsvline2XMLnode
  ) where

import qualified Data.Map as M        --container
import qualified Data.Text.Lazy as T  --text
import qualified Text.XML as X        --xml-conduit

-- | tsv形式（タブ区切りテキスト）のJSeMデータを受け取り、
-- | XML形式のJSeMデータを出力する。
tsvline2XMLnode :: T.Text -> T.Text
tsvline2XMLnode tsvlines  = 
  let nodes = map (entry2Node . (T.split (=='\t'))) $ tail $ T.lines tsvlines
      xmldoc = X.Document 
                (X.Prologue [] Nothing []) 
                (X.Element (myname "jsem-problems") (M.fromList []) nodes)
                []
  in X.renderText X.def xmldoc
{-  
  where myRenderSetting = RenderSettings 
                           { isPretty = True
                           , rsNamespaces = []             
                           , rsAttrOrder = X.orderAttrs [("problem",["jsem_id","answer","language","phenomena","inference_type"]),("link",["resource","link_id","translation","same_phenomena"])]                 
                           , rsUseCDATA = const False  
                           }
-}

myname :: T.Text -> X.Name
myname t = X.Name (T.toStrict t) Nothing Nothing 

entry2Node :: [T.Text] -> X.Node
entry2Node entry = 
  -- | entry!!0  1    
  -- | entry!!1  GQ間の関係：「ある」系-「すべて」系		
  -- | entry!!2  
  -- | entry!!3  unknown
  -- | entry!!4  generalized quantifier
  -- | entry!!5  entailment
  -- | entry!!6  P1 ある社員が異動を希望している。	
  -- | entty!!7  P2 （空欄の場合はは<p idx="2">タグはなし）
  -- | entry!!8  Hすべての社員が異動を希望している。				      
  -- | entry!!9  note  
  X.NodeElement $ X.Element 
                    (myname "problem")
                    (M.fromList 
                       [("jsem_id",T.toStrict (entry!!0)),
                        ("answer",T.toStrict (entry!!3)),
                        ("language","ja"),
                        ("phenomena",T.toStrict (entry!!4)),
                        ("inference_type",T.toStrict (entry!!5))
                       ])
                    ([X.NodeElement $ X.Element 
                        (myname "link")
                        (M.fromList 
                           [("resource","fracas"),
                            ("link_id",""),
                            ("translation",""),
                            ("same_phenomena","")
                           ])
                        [],
                     X.NodeElement $ X.Element
                        (myname "p")
                        (M.fromList [("idx","1")])
                        [X.NodeElement $ X.Element
                           (myname "script")
                           (M.fromList [])
                           [X.NodeContent (T.toStrict (entry!!6))]                        
                        ]
                      ] ++  
                      (if entry!!7 /= T.empty
                       then 
                         [X.NodeElement $ X.Element
                           (myname "p")
                           (M.fromList [("idx","2")])
                           [X.NodeElement $ X.Element
                             (myname "script")
                             (M.fromList [])
                             [X.NodeContent (T.toStrict (entry!!7))]
                           ]
                         ]  
                       else [])
                      ++ [  
                      X.NodeElement $ X.Element
                        (myname "h")
                        (M.fromList [])
                        [X.NodeElement $ X.Element
                           (myname "script")
                           (M.fromList [])
                           [X.NodeContent (T.toStrict (entry!!8))]
                        ],
                      X.NodeElement $ X.Element
                        (myname "note")
                        (M.fromList [])
                        [X.NodeContent (T.toStrict (entry!!9))]
                      ])
