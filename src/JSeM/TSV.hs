{-|
Module      : JSeM.TSV
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.TSV (
  tsv2XML,
  validateTsvFiles
  ) where

import Control.Monad (when,forM)         --base
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
-- | （ここではnkfを使わない。必要な場合はshellから呼ぶこと）
tsv2XML :: StrictT.Text -> IO(StrictT.Text)
tsv2XML tsv  = do
  problems <- mapM (tsvLine2xmlNode . chop . (StrictT.split (=='\t')) ) $ StrictT.lines tsv
  J.tidy $ LazyT.toStrict $ nodes2XML "jsem-dataset" problems

--- | tsv形式のJSeMデータのファイル名を受け取り、XML形式のJSeMデータを出力する。
-- tsvFile2XML :: FilePath -> IO(StrictT.Text)
-- tsvFile2XML = J.readFileUtf8 >=> tsv2XML

-- | タグ名をXMLタグ名に変換する。
tag :: StrictT.Text -> X.Name
tag tagname = X.Name tagname Nothing Nothing

-- | タグ名、ノードのリストを受け取り、XML形式のテキストに変換。
nodes2XML :: StrictT.Text -> [X.Node] -> LazyT.Text
nodes2XML name nodes =
  X.renderText X.def $ X.Document 
                         (X.Prologue
                            []
                            -- <!DOCTYPE jsem-problems SYSTEM "jsem.dtd">
                            (Just $ X.Doctype "jsem-dataset SYSTEM \"jsem.dtd\"" Nothing)
                            -- <?xml-stylesheet type="text/xsl" href="jsem.xsl"?>
                            [X.MiscInstruction $ X.Instruction "xml-stylesheet" "type=\"text/xsl\" href=\"jsem.xsl\"" ]
                            ) 
                         (X.Element (tag name) (M.fromList []) nodes)
                         []

-- | TSVファイルの最小コラム数。
numberOfColumns :: Int
numberOfColumns = 13

-- | 与えたtsvファイルのすべてについて、データ形式をチェック
-- （現在はコラム数が numberOfColumns 以上であることを確認のみ）
validateTsvFiles :: [FilePath] -> IO()
validateTsvFiles tsvFiles = do
  flags <- forM tsvFiles $ \tsvFile -> do
    putStrLn $ "Checking " ++ tsvFile ++ "..."
    jsemTxt <- J.readFileUtf8 tsvFile
    forM (zip [1..] $ tail $ StrictT.lines jsemTxt) $ \(lineNum,jsemLine) -> 
      if (length $ chop $ StrictT.split (=='\t') jsemLine) < numberOfColumns
        then do
             putStr $ "\n" ++ show (lineNum::Int) ++ ": "
             StrictT.putStr jsemLine
             return True  -- Error found
        else return False -- Error not found
  if (or $ concat flags)
    then fail $ "Above entries have less than " ++ (show numberOfColumns) ++ " columns."
    else putStrLn "tsv check done"

-- | 末尾の（一つまたは複数の）空白コラムを削除する
chop :: [StrictT.Text] -> [StrictT.Text]
chop [] = []
chop lst = if last lst == StrictT.empty
             then chop $ init lst
             else lst

-- | TSV形式のJSeMテキストをJSeM式のXMLノードに変換
-- | entry!!x
-- +--------+---------------+---------------+------------------------------------------+
-- | 1      |jsem_id        |通し番号。      |例：1                                      |
-- | 2      |resource       |リンク先リソース |例：FraCaS                                 |
-- | 3      |link_id        |リンク先の対応id |                                          |
-- | 4      |translation    |対訳レベルで一致 |                                          |
-- | 5      |same_phenomena |現象レベルで一致 |                                          |
-- | 6      |desc           |テストの記述。   |例：GQ間の関係「ある」系-「すべて」系         |	
-- | 7      |devtest        |dev/test の区別。|                                         |
-- | 8      |answer         |出力の正解。     |例：yes/no/unknown                        |
-- | 9      |phenomena      |含まれる言語現象。|例："Toritate, -nado (toritate particule)"|
-- | 10     |inference_type |推論の分類。     |例： entailment                           |
-- | 11     |note           |備考欄。        |                                          |
-- | 12     |P1             |前提文。        |例：ある社員が異動を希望している。	       |
-- | 13以降  |H              |帰結文。        |例：すべての社員が異動を希望している。        |
-- +--------+---------------+---------------+------------------------------------------+
-- | （entry7以降は前提文が任意個並び、最後の一つを帰結文とする仕様）
tsvLine2xmlNode :: [StrictT.Text] -> IO(X.Node)
tsvLine2xmlNode entry = do
  when (length entry < numberOfColumns) $ do
                             StrictT.putStrLn $ StrictT.concat [ StrictT.intercalate " " entry]
                             fail $ "the above entry has less than " ++ (show numberOfColumns) ++ " columns"
  let (jsem_id:(resource:(linkid:(trans:(same:(desc:(_:(answer:(entry4:(inference_type:(note:ph))))))))))) = entry
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
                        ("language","jp"),
                        ("phenomena", phenomena),
                        ("inference_type",inference_type)
                       ])
                    ((if linkid == ""
                        then []
                        else [X.NodeElement $ X.Element 
                               (tag "link")
                               (M.fromList [("resource",resource),
                                            ("link_id",linkid),
                                            ("translation",trans),
                                            ("same_phenomena",same)
                                            ])
                               []]
                     ) ++
                     [X.NodeElement $ X.Element
                        (tag "description")
                        (M.fromList [])
                        [X.NodeContent desc]
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

{-
-- | Test用コード
main :: IO()
main = 
  case parse phenomenaParser "" testp of
    Left _ -> putStrLn "parse error"
    Right p -> mapM_ StrictT.putStrLn p
  where testp = "\"Toritate, -sika (toritate particle), Negation\""
-}

{-
  where myRenderSetting = RenderSettings 
                           { isPretty = True
                           , rsNamespaces = []             
                           , rsAttrOrder = X.orderAttrs [("problem",["jsem_id","answer","language","phenomena","inference_type"]),("link",["resource","link_id","translation","same_phenomena"])]                 
                           , rsUseCDATA = const False  
                           }
-}
