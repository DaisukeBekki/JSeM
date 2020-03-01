{-|
Module      : JSeM.Cmd
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.Cmd (
  readFileUtf8,
  tidy,
  xmllint
  ) where

import qualified Data.Text as StrictT --text
import qualified Shelly as S          --shelly

readFileUtf8 :: FilePath -> IO(StrictT.Text)
readFileUtf8 filepath =
  S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.concat ["cat ", StrictT.pack filepath, " | nkf -w -Lu"]

--  TextがUTF8でない場合は使えないので、このコマンドは意味がないか
--  任意の文字コードのテキストを受け取り、utf8形式に変換する
-- nkf :: StrictT.Text -> IO(StrictT.Text)
-- nkf text = 
--  S.shelly $ do
--             S.setStdin text
--             S.silently $ S.escaping False $ S.cmd "nkf -w -Lu"

-- | XMLテキストを整形
-- | tidyのインストール：sudo apt-get install tidy
tidy :: StrictT.Text -> IO(StrictT.Text)
tidy xml = 
  S.shelly $ do
             S.setStdin xml
             S.silently $ S.escaping False $ S.cmd "tidy --tab-size 2 -xml -utf8 -indent -quiet"

-- | XMLテキストを検証
-- | xmllintのインストール：sudo apt-get install libxml2-utils
xmllint :: FilePath -> IO()
xmllint xmlFile = 
  S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.pack $ "xmllint --valid --noout " ++ xmlFile


