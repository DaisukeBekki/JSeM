{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified GHC.Generics          as G --base
import qualified Data.Aeson            as A --aeson
import qualified Data.Yaml             as Y --yaml
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified System.Environment as E -- base
import qualified Data.Text.Lazy as T     -- text
import qualified Data.Text.Lazy.IO as T  -- text
-- import qualified Shelly as S             -- shelly
-- import Text.RawString.QQ (r)             -- raw-strings-qq
-- import Data.Time.LocalTime (getZonedTime) -- time
-- import Data.Time.Format (formatTime, defaultTimeLocale) -- time
-- import Data.Nat -- nats
import qualified JSeM.XML as J
import qualified JSeM as J
import qualified ML.Exp.Classification.Bounded as NLP --nlp-tools

default(T.Text)

-- | Usage:
-- | stack run evaluateLLM -- level1

data AnswerLabel = YES | UNK | OTHER deriving (Eq, Show, G.Generic, Enum, Bounded)

instance A.FromJSON AnswerLabel

data Output = Output {
  reasoning :: String,
  label :: AnswerLabel
  } deriving (Show, G.Generic)

instance A.FromJSON Output

data InfData = InfData {
  premise :: String,
  hypothesis :: String,
  output :: Output
  } deriving (Show, G.Generic)

instance A.FromJSON InfData

data LLManswer = LLManswer {
  inference :: InfData
  } deriving (Show, G.Generic)

instance A.FromJSON LLManswer

basePath :: String
basePath = "./app/TT2/log/"

main :: IO ()
main = do
  (fileName:_) <- E.getArgs
  geminiAnswers <- B.readFile $ basePath ++ "level" ++ fileName ++ ".yaml"
  let parsedAnswers = Y.decodeEither' geminiAnswers :: Either Y.ParseException [LLManswer]
  parsedAnswers' <- case parsedAnswers of
                      Left parse_exception -> 
                        error $ "Could not parse yaml file for " ++ (show fileName) ++ ": " ++ (show parse_exception)
                      Right infData -> return infData
  -- print answers
  jsemXML <- T.readFile $ basePath ++ "level" ++ fileName ++ ".xml"
  parsedJSeM <- J.xml2jsemData $ T.toStrict jsemXML
  -- print parsedJSeM -- J.JSeMData
  let groundTruth = map (\a -> case J.answer a of
                                 J.YES -> YES
                                 J.UNKNOWN -> UNK
                                 _     -> OTHER
                                 ) parsedJSeM
      predictions = map (label . output . inference) parsedAnswers' 
  let pairs = zip predictions groundTruth 
  T.putStrLn $ T.fromStrict $ NLP.showClassificationReport pairs
  print "Finished."  