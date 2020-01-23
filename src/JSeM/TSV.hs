{-|
Module      : JSeM.TSV
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.TSV (
  jsemData2tsv,
  tsv2jsemData,
  validateTsvFiles
  ) where

import Control.Monad (when,forM)         --base
import qualified Data.Text as StrictT    --text
import qualified Data.Text.IO as StrictT --text
import Text.Parsec as P                  --parsec
import Text.Parsec.Text as P             --parsec
import JSeM (JSeMData(..),JSeMLink(..),readJSeMLabel,readYesNo)  --jsem
import qualified JSeM.Cmd as J           --jsem

-- | TSVファイルの最小コラム数。
numberOfColumns :: Int
numberOfColumns = 13

-- | Convert a list of JSeMData into Data.TEXT (in TSV format)
jsemData2tsv :: [JSeMData] -> StrictT.Text
jsemData2tsv jsemdata =
  StrictT.intercalate "\n" (map (\j ->
    StrictT.intercalate "\t" ([
      jsem_id j,
      resource $ link j,
      link_id $ link j,
      StrictT.pack $ show $ translation $ link j,
      StrictT.pack $ show $ same_phenomena $ link j,
      description j,
      "",
      StrictT.pack $ show $ answer j,
      if phenomena j == [] then "" else StrictT.concat ["\"", StrictT.intercalate ", " $ phenomena j, "\""],
      inference_type j,
      StrictT.concat $ StrictT.lines $ note j
      ] ++ (premises j) ++ [hypothesis j]
      )) jsemdata)

-- | tsv形式のJSeMデータをTextとして受け取り、
-- | XML形式のJSeMデータを出力する。
-- | （ここではnkfを使わない。必要な場合はshellから呼ぶこと）
tsv2jsemData :: StrictT.Text -> IO([JSeMData])
tsv2jsemData tsv = mapM (tsvLine2jsemData . chop . (StrictT.split (=='\t')) ) $ StrictT.lines tsv

--- | tsv形式のJSeMデータのファイル名を受け取り、XML形式のJSeMデータを出力する。
-- tsvFile2XML :: FilePath -> IO(StrictT.Text)
-- tsvFile2XML = J.readFileUtf8 >=> tsv2XML

-- | TSV形式のJSeMテキストをJSeM式のXMLノードに変換
tsvLine2jsemData :: [StrictT.Text] -> IO(JSeMData)
tsvLine2jsemData e = do
  when (length e < numberOfColumns) $ do
                             StrictT.putStrLn $ StrictT.concat [ StrictT.intercalate " " e]
                             fail $ "the above entry has only " ++ (show $ length e) ++ " columns"
  let jsem_id'        = e!!0
      link_id'        = e!!1
      resource'       = e!!2
      trans'          = readYesNo $ e!!3
      same'           = readYesNo $ e!!4
      description'    = e!!5
      --devtest'        = e!!6
      answer'         = readJSeMLabel $ e!!7
      phenomena'      = e!!8
      inference_type' = e!!9
      note'           = e!!10 
      ph = drop 11 e
      premises'   = init ph
      hypothesis' = last ph 
      link' = if link_id' == ""
                then NoLink
                else Link link_id' resource' trans' same'
  phenomena'' <- case phenomena' of
                    "" -> return []
                    _ -> case parse phenomenaParser "" phenomena' of
                           Left err -> do
                                       StrictT.putStrLn phenomena'
                                       fail $ show err
                           Right p -> return p
  return $ JSeMData jsem_id' link' description' answer' phenomena'' inference_type' note' premises' hypothesis'

-- | "phenomena"タグのための記述内容をパーズし、現象名のリストを得る。
phenomenaParser :: Parser [StrictT.Text]
phenomenaParser = do
  _ <- optional $ char '\"'
  pheno <- sepBy (many1 $ noneOf ",\"") (string "," <|> string ", ")
  _ <- optional $ char '\"'  
  return $ map StrictT.pack pheno

-- | 与えたtsvファイルのすべてについて、データ形式をチェック
-- （現在はコラム数が numberOfColumns 以上であることを確認のみ）
validateTsvFiles :: [FilePath] -> IO()
validateTsvFiles tsvFiles = do
  flags <- forM tsvFiles $ \tsvFile -> do
    putStrLn $ "Checking " ++ tsvFile ++ "..."
    jsemTxt <- J.readFileUtf8 tsvFile
    forM (zip [1..] $ tail $ StrictT.lines jsemTxt) $ \(lineNum,jsemLine) ->
      let num = length $ chop $ StrictT.split (=='\t') jsemLine in
      if num < numberOfColumns
        then do
             putStr $ "Line " ++ (show (lineNum::Int)) ++ " (" ++ (show num) ++ " cols): "
             StrictT.putStrLn jsemLine
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

{-
  where myRenderSetting = RenderSettings 
                           { isPretty = True
                           , rsNamespaces = []             
                           , rsAttrOrder = X.orderAttrs [("problem",["jsem_id","answer","language","phenomena","inference_type"]),("link",["resource","link_id","translation","same_phenomena"])]                 
                           , rsUseCDATA = const False  
                           }
-}
