{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : JSeM.TSV2XML
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.TSV2XML (
  tsv2XML
  ) where

import qualified Data.Map as M        --container
import qualified Data.Text.Lazy as LazyT  --text
import qualified Text.XML as X        --xml-conduit

-- | tsv形式（タブ区切りテキスト）のJSeMデータを受け取り、
-- | XML形式のJSeMデータを出力する。
tsv2XML :: LazyT.Text -> LazyT.Text
tsv2XML tsvlines  = 
  let nodes = map (tsvLine2xmlNode . (LazyT.split (=='\t'))) $ tail $ LazyT.lines tsvlines
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

myname :: LazyT.Text -> X.Name
myname t = X.Name (LazyT.toStrict t) Nothing Nothing 

tsvLine2xmlNode :: [LazyT.Text] -> X.Node
tsvLine2xmlNode entry = 
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
                       [("jsem_id",LazyT.toStrict (entry!!0)),
                        ("answer",LazyT.toStrict (entry!!3)),
                        ("language","ja"),
                        ("phenomena",LazyT.toStrict (entry!!4)),
                        ("inference_type",LazyT.toStrict (entry!!5))
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
                           [X.NodeContent (LazyT.toStrict (entry!!6))]                        
                        ]
                      ] ++  
                      (if entry!!7 /= LazyT.empty
                       then 
                         [X.NodeElement $ X.Element
                           (myname "p")
                           (M.fromList [("idx","2")])
                           [X.NodeElement $ X.Element
                             (myname "script")
                             (M.fromList [])
                             [X.NodeContent (LazyT.toStrict (entry!!7))]
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
                           [X.NodeContent (LazyT.toStrict (entry!!8))]
                        ],
                      X.NodeElement $ X.Element
                        (myname "note")
                        (M.fromList [])
                        [X.NodeContent (LazyT.toStrict (entry!!9))]
                      ])
