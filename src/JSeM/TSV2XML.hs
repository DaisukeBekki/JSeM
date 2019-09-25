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

import Control.Monad (when)              --base
import qualified Data.Map as M           --container
import qualified Data.Text as StrictT    --text
import qualified Data.Text.IO as StrictT    --text
import qualified Data.Text.Lazy as LazyT --text
import qualified Text.XML as X           --xml-conduit
import qualified Shelly as S             --shelly
import Text.Parsec as P                  --parsec
import Text.Parsec.Text as P             --parsec
--import Text.Parsec.Char as P             --parsec

-- | tsv形式のJSeMデータをTextとして受け取り、
-- | XML形式のJSeMデータを出力する。
tsv2XML :: LazyT.Text -> IO(LazyT.Text)
tsv2XML tsvlines  = 
  nodes2XML "jsem-problems" <$> (mapM (tsvLine2xmlNode . (StrictT.split (=='\t')) . LazyT.toStrict) $ tail $ LazyT.lines tsvlines)

nkf :: FilePath -> IO(StrictT.Text)
nkf filepath = S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.concat ["cat '", StrictT.pack filepath, "'| nkf -w -Lu"]
-- | tsv形式のJSeMデータのファイル名を受け取り、開いてutf8形式のテキストにデコードする
--decodeFile :: FilePath -> IO(LazyT.Text)
--decodeFile file = E.decodeUtf8 <$> nkf file

-- | tsv形式のJSeMデータのファイル名を受け取り、XML形式のJSeMデータを出力する。
tsvFile2XML :: FilePath -> IO(LazyT.Text)
tsvFile2XML tsvFile = tsv2XML =<< LazyT.fromStrict <$> nkf tsvFile 

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
tsvLine2xmlNode :: [StrictT.Text] -> IO(X.Node)
tsvLine2xmlNode entry = do
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
  when (length entry < 11) $ do
                             StrictT.putStrLn $ StrictT.concat [ StrictT.intercalate " " entry]
                             fail "the above entry has less than 10 columns"
  -- | JSeMのphenomena名はexcelのtsv変換で前後にダブルクオーテーションが付くので外す。
  let entry4 = entry!!4
  phenomena <- case entry4 of
                    "" -> return ""
                    _ -> case parse phenomenaParser "" entry4 of
                           Left err -> do
                                       StrictT.putStrLn entry4
                                       fail $ show err
                           Right p -> return $ StrictT.intercalate "," p
  return $ X.NodeElement $ X.Element 
                    (myname "problem")
                    (M.fromList 
                       [("jsem_id",entry!!0),
                        ("answer",entry!!3),
                        ("language","ja"),
                        ("phenomena", phenomena),
                        ("inference_type",entry!!5)
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
                           [X.NodeContent (entry!!6)]                        
                        ]
                      ] ++  
                      (if entry!!7 /= StrictT.empty
                       then 
                         [X.NodeElement $ X.Element
                           (myname "p")
                           (M.fromList [("idx","2")])
                           [X.NodeElement $ X.Element
                             (myname "script")
                             (M.fromList [])
                             [X.NodeContent (entry!!7)]
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
                           [X.NodeContent (entry!!8)]
                        ],
                      X.NodeElement $ X.Element
                        (myname "note")
                        (M.fromList [])
                        [X.NodeContent (entry!!9)]
                      ])

phenomenaParser :: Parser [StrictT.Text]
phenomenaParser = do
  char '\"'
  phenomena <- sepBy (many1 $ noneOf ",\"") (string "," <|> string ", ")
  char '\"'  
  return $ map StrictT.pack phenomena

-- test

testp :: StrictT.Text
testp = "\"Toritate, -sika (toritate particle), Negation\""
  
main :: IO()
main = do
  case parse phenomenaParser "" testp of
    Left _ -> putStrLn "parse error"
    Right p -> mapM_ StrictT.putStrLn p
