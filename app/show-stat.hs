{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)             --base
import System.Environment (getArgs)      --base
import System.FilePath.Posix ((</>),isExtensionOf,takeBaseName) --filepath
import qualified System.Directory as D   --directory
import qualified Data.Text.IO as T       --text
import qualified JSeM.XML as J           --jsem

main :: IO()
main = do
  (dataFolder:_) <- getArgs
  _ <- D.doesDirectoryExist dataFolder 
  T.putStrLn =<< J.getStat dataFolder
  ---
  putStrLn "Number of problems in each section:"
  xmlFiles <- map (dataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory dataFolder
  num <- forM xmlFiles $ \xmlFile -> do
    putStr $ takeBaseName xmlFile
    putStr ": "
    jsemData <- J.xmlFile2jsemData xmlFile
    let n = length jsemData
    putStrLn $ show n
    return n
  putStr "Total: "
  putStrLn $ show $ sum num
