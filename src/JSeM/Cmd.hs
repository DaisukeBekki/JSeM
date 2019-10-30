{-|
Module      : JSeM.Cmd
Copyright   : (c) Daisuke Bekki, 2019
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module JSeM.Cmd (
  readFileUtf8,
  nkf,
  tidy,
  xmllint
  ) where

import qualified Data.Text as StrictT --text
import qualified Shelly as S          --shelly

readFileUtf8 :: FilePath -> IO(StrictT.Text)
readFileUtf8 filepath =
  S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.concat ["cat ", StrictT.pack filepath, " | nkf -w -Lu"]

-- | 任意の文字コードのテキストを受け取り、utf8形式に変換する
nkf :: StrictT.Text -> IO(StrictT.Text)
nkf text = 
  S.shelly $ do
             S.setStdin text
             S.silently $ S.escaping False $ S.cmd "nkf -w -Lu"

-- | XMLテキストを整形
tidy :: StrictT.Text -> IO(StrictT.Text)
tidy xml = 
  S.shelly $ do
             S.setStdin xml
             S.silently $ S.escaping False $ S.cmd "tidy --tab-size 2 -xml -utf8 -indent -quiet"

-- | XMLテキストを検証
xmllint :: String -> IO()
xmllint xmlFile = 
  S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ StrictT.pack $ "xmllint --valid --noout " ++ xmlFile


