{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)                 --base
import System.Environment (getArgs)         --base
import qualified System.Directory as D      --directory
import System.FilePath ((</>),isExtensionOf) --filepath
--import qualified Data.ByteString.Lazy as B --bytestring
import qualified Data.Text.IO as T          --text
import qualified Text.XML as X              --xml-conduit
import qualified Text.XML.Cursor as X       --xml-conduit
import qualified JSeM.XML as J              --jsem

-- | JSeM統計情報出力プログラム
-- |   標準入力からxmlフォーマットのJSeMファイルを受け取り、以下を標準出力に出力する：
-- | (1)全<problem>要素のうち、内部に<link>要素のあるものとないものの出現数と割合
-- | (2)全<link>要素のうち、translation属性がyesのものとそうでないものの出現数と割合
-- | (3)<problem>要素内のphenomena属性の値のリストと出現数
main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  xmlFiles <- map (dataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  stat <- J.problems2stat <$> concat <$> forM xmlFiles (\xmlFile -> do
    cursor <- X.fromDocument <$> X.readFile X.def xmlFile 
    return $ X.child cursor >>= X.element "problem"
    )
  T.putStrLn stat
