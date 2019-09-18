-- | tsvフォルダのtsvファイルのうち、同名のxmlが存在しないものについてxmlに変換。
-- | statを実行
-- | README.mdを更新

import System.FilePath ((</>),(<.>))      --filepath
import qualified System.Environment as E   --base
import qualified Data.ByteString.Lazy as B hiding (putStrLn) --bytestring
import qualified Data.ByteString.Lazy.Char8 as B --bytestring
--import qualified JSeM.Dataset as J       --jsem

main :: IO()
main = do
  (filename:_) <- E.getArgs
  let filepath = "/home/bekki/program/jsem/data/JSeM_beta/JSeM_beta_" ++ filename <.> "xml"
  B.putStrLn =<< B.readFile filepath

