import Control.Monad (forM_)            --base
import System.FilePath ((</>),isExtensionOf) --filepath
import System.Environment (getArgs)     --base
import qualified System.Directory as D  --directory
import qualified JSeM.Cmd as J          --jsem

main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  xmlFileNames <- filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  let xmlFiles = map (dataFolder </>) xmlFileNames
  forM_ xmlFiles J.xmllint
