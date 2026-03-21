{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Usage: stack run generateInfiniteInference -- 3

import qualified System.Environment as E -- base
import qualified System.IO as S          -- base
import qualified Data.Text.IO as T       -- text
import TuringTest2.InfiniteInference (jsemHeader, jsemFooter, inference2prompt, inference2jsem, infOf, int2nat) -- jsem
import TuringTest2.Prompt (geminiPrompt) -- jsem

basePath :: String
basePath = "./app/TT2/log/"

main :: IO ()
main = do
  (depthString:_) <- E.getArgs
  let dataset = take 20 $ infOf $ int2nat $ ((read depthString)::Int)
      jsemFilePath = basePath ++ "level" ++ depthString ++ ".xml"
      promptPath = basePath ++ "level" ++ depthString ++ ".prompt"
  S.withFile jsemFilePath S.WriteMode $ \h -> do -- | print as JSeM data
    T.hPutStrLn h jsemHeader
    mapM_ (T.hPutStrLn h . inference2jsem) dataset
    T.hPutStrLn h jsemFooter
  S.withFile promptPath S.WriteMode $ \h -> do   -- | print as prompt
    S.hPutStr h geminiPrompt
    mapM_ (T.hPutStrLn h . inference2prompt) dataset
