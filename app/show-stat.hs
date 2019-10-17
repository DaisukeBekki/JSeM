{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B  --bytestring
import qualified Data.List as L             --base
import qualified Data.Map as M              --container
import qualified Data.Text as T             --text
import qualified Data.Text.IO as T          --text
import qualified Text.XML as X              --xml-conduit
import qualified Text.XML.Cursor as X       --xml-conduit

-- | JSeM統計情報出力プログラム
-- |   標準入力からxmlフォーマットのJSeMファイルを受け取り、以下を標準出力に出力する：
-- | (1)全<problem>要素のうち、内部に<link>要素のあるものとないものの出現数と割合
-- | (2)全<link>要素のうち、translation属性がyesのものとそうでないものの出現数と割合
-- | (3)<problem>要素内のphenomena属性の値のリストと出現数
main :: IO()
main = do
  xml <- B.getContents
  let cursor = X.fromDocument $ X.parseLBS_ X.def xml
      problemNodes = X.child cursor >>= X.element "problem"
      linkNodes = problemNodes >>= X.child >>= X.element "link"
      --unexpectedcomments = problemNodes >>= X.child >>= X.checkNode isComment
      transYes = linkNodes >>= X.laxAttribute "translation"
      phenomena = problemNodes >>= X.laxAttribute "phenomena" >>= T.split (==',')
      phen2 = L.map T.strip phenomena
  --putStrLn "(0) Comments contained in <problem> tags:"
  --mapM_ T.putStrLn unexpectedcomments
  putStrLn $ "(1) The number of <problem> tags: "
               ++ (show $ L.length $ problemNodes)
               ++ "\n  which contains a <link> (to FraCaS dataset) tag: "
               ++ (show $ L.length $ linkNodes)
               ++ "\n(2) The number of yes in translation attr.: "
               ++ (show $ L.length $ L.filter (=="yes") transYes)
               ++ "\n(3) The number of occurrences of each phenomena:"
  mapM_ T.putStrLn $ L.map (\(t,x) -> T.concat ["- ", t,": ",(T.pack $ show x)]) $ M.toList $ M.fromListWith (+) $ L.zip phen2 (repeat (1::Integer))


