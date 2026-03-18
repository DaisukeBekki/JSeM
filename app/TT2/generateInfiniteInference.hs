{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Usage: stack run generateInfiniteInference -- 3

import qualified System.Environment as E -- base
import qualified System.IO as S          -- base
import qualified Data.Text.IO as T       -- text
import TuringTest2.InfiniteInference (jsemHeader, jsemFooter, inference2prompt, inference2jsem, infOf, int2nat, prompt) -- jsem

basePath :: String
basePath = "./app/TT2/log/"

main :: IO ()
main = do
  (depthString:_) <- E.getArgs
  let depth = int2nat $ ((read depthString)::Int)
      jsemFilePath = basePath ++ "level" ++ depthString ++ ".xml"
      promptPath = basePath ++ "level" ++ depthString ++ ".prompt"
  -- | print as JSeM data
  S.withFile jsemFilePath S.WriteMode $ \h -> do
    T.hPutStrLn h jsemHeader
    mapM_ (T.hPutStrLn h . inference2jsem) $ take 100 $ infOf depth
    T.hPutStrLn h jsemFooter
  -- | print as prompt
  S.withFile promptPath S.WriteMode $ \h -> do
    S.hPutStrLn h prompt
    mapM_ (T.hPutStrLn h . inference2prompt) $ take 100 $ infOf depth
