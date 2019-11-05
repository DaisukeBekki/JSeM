import System.FilePath ((</>),isExtensionOf) --filepath
import System.Environment (getArgs)     --base
import qualified System.Directory as D  --directory
import qualified JSeM.Cmd as J          --jsem

main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  xmlFiles <- map (dataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  mapM_ J.xmllint xmlFiles
