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
  validateTsvFiles
  ) where

import Control.Monad (when,forM,forM_)   --base
import qualified Data.Map as M           --container
import qualified Data.Text as StrictT    --text
import qualified Data.Text.IO as StrictT --text
import qualified Data.Text.Lazy as LazyT --text
import qualified Text.XML as X           --xml-conduit
import Text.Parsec as P                  --parsec
import Text.Parsec.Text as P             --parsec
import qualified JSeM.Cmd as J           --jsem

-- | tsv形式のJSeMデータをTextとして受け取り、
-- | XML形式のJSeMデータを出力する。
-- | （ここではnkfやtidyを使わない。必要な場合はshellから呼ぶこと）
tsv2XML :: StrictT.Text -> IO(LazyT.Text)
tsv2XML tsv  = 
  nodes2XML "jsem-problems" <$> (mapM (tsvLine2xmlNode . chop . (StrictT.split (=='\t')) ) $ tail $ StrictT.lines tsv)

-- | tsv形式のJSeMデータのファイル名を受け取り、XML形式のJSeMデータを出力する。
tsvFile2XML :: FilePath -> IO(LazyT.Text)
tsvFile2XML tsvFile =
  J.readFileUtf8 tsvFile
  >>= tsv2XML

-- | タグ名をXMLタグ名に
tag :: StrictT.Text -> X.Name
tag tagname = X.Name tagname Nothing Nothing

-- | タグ名、ノードのリストを受け取り、XML形式のテキストに変換。
nodes2XML :: StrictT.Text -> [X.Node] -> LazyT.Text
nodes2XML name nodes =
  X.renderText X.def $ X.Document 
                         (X.Prologue
                            []
                            -- | <!DOCTYPE jsem-problems SYSTEM "jsem.dtd">
                            (Just $ X.Doctype "jsem-problems SYSTEM \"jsem.dtd\"" Nothing)
                            -- |<?xml-stylesheet type="text/xsl" href="jsem.xsl"?>
                            [X.MiscInstruction $ X.Instruction "xml-stylesheet" "type=\"text/xsl\" href=\"jsem.xsl\"" ]
                            ) 
                         (X.Element (tag name) (M.fromList []) nodes)
                         []

-- | 与えたtsvファイルのすべてについて、データ形式をチェック（現在はコラム数が9以上であることを確認のみ）
validateTsvFiles :: [FilePath] -> IO()
validateTsvFiles tsvFiles = 
  forM_ tsvFiles $ \tsvFile -> do
          putStr $ "Checking " ++ tsvFile ++ "..."
          jsemTxt <- J.readFileUtf8 tsvFile
          flags <- forM (zip [1..] $ tail $ StrictT.lines jsemTxt) $ \(lineNum,jsemLine) -> 
                          if (length $ chop $ StrictT.split (=='\t') jsemLine) < 9
                            then do
                                 putStr $ "\n" ++ show (lineNum::Int) ++ ": "
                                 StrictT.putStr jsemLine
                                 return True  -- Error found
                            else return False -- Error not found
          if or flags
            then fail "\nAbove entries have less than 9 columns."
            else putStrLn "done"

-- | 末尾の空白データを削除する
chop :: [StrictT.Text] -> [StrictT.Text]
chop [] = []
chop lst = if last lst == StrictT.empty
             then chop $ init lst
             else lst

-- | TSV形式のJSeMテキストをJSeM式のXMLノードに変換
tsvLine2xmlNode :: [StrictT.Text] -> IO(X.Node)
tsvLine2xmlNode entry = do
  -- | entry!!0 :jsem_id        通し番号。      例：1    
  -- | entry!!1 :?              テストの記述。   例：GQ間の関係「ある」系-「すべて」系		
  -- | entry!!2 :?              dev/testの区別。
  -- | entry!!3 :answer         出力の正解。     例：yes/no/unknown
  -- | entry!!4 :phenomena      含まれる言語現象。例："Toritate, -nado (toritate particule)"
  -- | entry!!5 :inference_type 推論の分類。     例： entailment
  -- | entry!!6 :note           備考欄。
  -- | entry!!7 :P1             前提文。        例：ある社員が異動を希望している。	
  -- | entty!!8 :P2             前提文。        例：（空欄の場合はは<p idx="2">タグはなし）
  -- | entry!!9 :H              帰結文。        例：すべての社員が異動を希望している。
  -- | （entry7以降は前提文が任意個並び、最後の一つを帰結文とする仕様）
  when (length entry < 9) $ do
                             StrictT.putStrLn $ StrictT.concat [ StrictT.intercalate " " entry]
                             fail "the above entry has less than 9 columns"
  let (jsem_id:(_:(_:(answer:(entry4:(inference_type:(note:ph))))))) = entry
      premises = init ph
      hypothesis = last ph
  phenomena <- case entry4 of
                    "" -> return ""
                    _ -> case parse phenomenaParser "" entry4 of
                           Left err -> do
                                       StrictT.putStrLn entry4
                                       fail $ show err
                           Right p -> return $ StrictT.intercalate "," p
  return $ X.NodeElement $ X.Element 
                    (tag "problem")
                    (M.fromList 
                       [("jsem_id",jsem_id),
                        ("answer",answer),
                        ("language","ja"),
                        ("phenomena", phenomena),
                        ("inference_type",inference_type)
                       ])
                    ([X.NodeElement $ X.Element 
                        (tag "link")
                        (M.fromList 
                           [("resource","fracas"),
                            ("link_id",""),
                            ("translation",""),
                            ("same_phenomena","")
                           ])
                        []
                     ] ++
                     ((flip map) (zip premises [1..]) $ \(premise,i) ->  
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
                           [X.NodeContent hypothesis]
                        ],
                      X.NodeElement $ X.Element
                        (tag "note")
                        (M.fromList [])
                        [X.NodeContent note]
                      ])

-- | "phenomena"タグのための記述内容をパーズし、現象名のリストを得る。
phenomenaParser :: Parser [StrictT.Text]
phenomenaParser = do
  _ <- optional $ char '\"'
  phenomena <- sepBy (many1 $ noneOf ",\"") (string "," <|> string ", ")
  _ <- optional $ char '\"'  
  return $ map StrictT.pack phenomena

-- | Test用コード
main :: IO()
main = 
  case parse phenomenaParser "" testp of
    Left _ -> putStrLn "parse error"
    Right p -> mapM_ StrictT.putStrLn p
  where testp = "\"Toritate, -sika (toritate particle), Negation\""


{-
  where myRenderSetting = RenderSettings 
                           { isPretty = True
                           , rsNamespaces = []             
                           , rsAttrOrder = X.orderAttrs [("problem",["jsem_id","answer","language","phenomena","inference_type"]),("link",["resource","link_id","translation","same_phenomena"])]                 
                           , rsUseCDATA = const False  
                           }
-}
