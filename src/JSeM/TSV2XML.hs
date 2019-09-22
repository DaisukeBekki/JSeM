{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : JSeM.TSV2XML
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.TSV2XML (
  tsv2XML,
  tsvFile2XML,
  checkTsvFile
  ) where

import qualified Data.Map as M                   --container
import qualified Data.Text as StrictT            --text
import qualified Data.Text.Lazy as LazyT         --text
import qualified Text.XML as X                   --xml-conduit
import qualified Shelly as S                     --shelly

-- | tsv形式のJSeMデータをTextとして受け取り、
-- | XML形式のJSeMデータを出力する。
tsv2XML :: LazyT.Text -> LazyT.Text
tsv2XML tsvlines  = 
  nodes2XML "jsem-problems" $ map (tsvLine2xmlNode . (LazyT.split (=='\t'))) $ tail $ LazyT.lines tsvlines

nkf :: FilePath -> IO(StrictT.Text)
nkf filepath = S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.concat ["cat '", StrictT.pack filepath, "'| nkf -w -Lu"]
-- | tsv形式のJSeMデータのファイル名を受け取り、開いてutf8形式のテキストにデコードする
--decodeFile :: FilePath -> IO(LazyT.Text)
--decodeFile file = E.decodeUtf8 <$> nkf file

-- | tsv形式のJSeMデータのファイル名を受け取り、XML形式のJSeMデータを出力する。
tsvFile2XML :: FilePath -> IO(LazyT.Text)
tsvFile2XML tsvFile = tsv2XML <$> LazyT.fromStrict <$> nkf tsvFile 

-- | tsv形式のJSeMデータのファイル名を受け取り、形式をチェックする。
checkTsvFile :: FilePath -> IO()
checkTsvFile tsvFile = do
  txt <- nkf tsvFile
  mapM_ (putStrLn . show . length . LazyT.split (=='\t')) $ LazyT.lines $ LazyT.fromStrict txt
  
-- | タグ名をXMLタグ名に
myname :: LazyT.Text -> X.Name
myname t = X.Name (LazyT.toStrict t) Nothing Nothing

-- | タグ名、ノードのリストを受け取り、XML形式のテキストに変換。
nodes2XML :: LazyT.Text -> [X.Node] -> LazyT.Text
nodes2XML tagname nodes =
  X.renderText X.def $ X.Document 
                         (X.Prologue [] Nothing []) 
                         (X.Element (myname tagname) (M.fromList []) nodes)
                         []
{-
  where myRenderSetting = RenderSettings 
                           { isPretty = True
                           , rsNamespaces = []             
                           , rsAttrOrder = X.orderAttrs [("problem",["jsem_id","answer","language","phenomena","inference_type"]),("link",["resource","link_id","translation","same_phenomena"])]                 
                           , rsUseCDATA = const False  
                           }
-}

-- | TSV形式のJSeMテキストをJSeM式のXMLノードに変換
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

