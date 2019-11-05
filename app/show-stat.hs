{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)         --base
import qualified Data.Text.IO as T          --text
import qualified JSeM.XML as J              --jsem

main :: IO()
main = do
  (dataFolder:_) <- getArgs
  T.putStrLn =<< J.getStat dataFolder
