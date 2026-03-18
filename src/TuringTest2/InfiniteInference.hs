{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : TuringTest2.InfiniteInference
Copyright   : (c) Daisuke Bekki, 2026
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module TuringTest2.InfiniteInference (
  jsemHeader, 
  jsemFooter, 
  inference2jsem, 
  inference2prompt,
  nat2int,
  int2nat,
  infOf,
  prompt
  ) where

import qualified Data.Text    as T -- text
import Control.Monad (guard) --base
import Data.Nat -- nats
import Text.RawString.QQ (r) -- raw-strings-qq

data CommonNoun = HOUSE | MALT | RAT | CAT | DOG | COW | MAIDEN | MAN | PRIEST | COCK | CONE | FARMER deriving (Eq, Show)

cn2text :: CommonNoun -> T.Text
cn2text c = case c of
  HOUSE -> "house"
  MALT -> "malt"
  RAT -> "rat"
  CAT -> "cat"
  DOG -> "dog"
  COW -> "cow"
  MAIDEN -> "maiden"
  MAN -> "man"
  PRIEST -> "priest"
  COCK -> "cock"
  CONE -> "cone"
  FARMER -> "farmer"

type Token = T.Text

-- relativise :: CommonNoun -> [Token] -> [Token]
-- relativise cn ts = (cn2text cn):("that"):ts

tokens2Text :: [Token] -> T.Text
tokens2Text ts = T.intercalate " " ts

data Inference = Inf {
  lvl :: Nat
  , pheno :: T.Text
  , gapCN :: CommonNoun
  , premise :: [Token]
  , conclusion :: [Token]
  } deriving (Eq, Show)

basicInferenceData :: [Inference]
basicInferenceData = [
  Inf {
    lvl = Z
    , pheno = "Disjunction"
    , gapCN = DOG
    , premise    = ["chased John"]
    , conclusion = ["chased John or Mary"]
    },
  Inf {
    lvl = Z
    , pheno = "Disjunction"
    , gapCN = CAT
    , premise    = ["loves John and Mary"]
    , conclusion = ["loves John"]
    },
  Inf {
    lvl = Z
    , pheno = "Disjunction"
    , gapCN = MAIDEN
    , premise    = ["sings"]
    , conclusion = ["sings or dances"]
    },
  Inf {
    lvl = Z
    , pheno = "Conjunction"
    , gapCN = MAIDEN
    , premise    = ["sings and dances"]
    , conclusion = ["sings"]
    }
  ]

data Rel = Rel {
  relName :: T.Text,
  sbjCN :: CommonNoun,
  objCN :: CommonNoun
  }

relations :: [Rel]
relations = [
  Rel "inhabited by" HOUSE FARMER
  , Rel "lay in" MALT HOUSE
  , Rel "ate" RAT MALT
  , Rel "killed" CAT RAT
  , Rel "worried" DOG CAT
  , Rel "tossed" COW DOG
  , Rel "miled" MAIDEN COW
  , Rel "kissed" MAN MAIDEN
  , Rel "married" PRIEST MAN
  , Rel "waked" COCK PRIEST
  , Rel "kept" CONE COCK
  , Rel "sew" FARMER CONE
  ]

data Monotonicity = Upward | Downward deriving (Eq, Show)
data QMon = QMon T.Text Monotonicity Monotonicity deriving (Eq, Show)

quantifiers :: [QMon]
quantifiers = [
  QMon "every"     Downward Upward,
  QMon "some"      Upward   Upward,
  QMon "no"        Downward Downward,
  QMon "not every" Upward   Downward
  ]

infOf :: Nat -> [Inference]
infOf Z = basicInferenceData
infOf (S n) = do
  inf <- infOf n
  rel <- relations
  let o = objCN rel
  guard $ o == gapCN inf
  QMon qname left _ <- quantifiers
  let (premiseTokens, conclusionTokens) = case left of
        Downward -> (conclusion inf, premise inf)
        Upward   -> (premise inf, conclusion inf)
  let premiseN    = (relName rel):qname:(cn2text o):("that"):premiseTokens
      conclusionN = (relName rel):qname:(cn2text o):("that"):conclusionTokens
  return $ Inf (S $ lvl inf) (pheno inf) (sbjCN rel) premiseN conclusionN 
  

inference2jsem :: Inference -> T.Text
inference2jsem Inf{..} = T.concat [
  "<problem answer=\"yes\" inference_type=\"entailment\" language=\"en\" phenomena=\"level",
  T.pack $ show $ nat2int lvl,
  ", ",
  pheno,
  "\"><p idx=\"1\"><script>",
  tokens2Text $ ("A"):(cn2text gapCN):premise,
  "</script></p><h><script>",
  tokens2Text $ ("A"):(cn2text gapCN):conclusion,
  "</script></h></problem>"
  ]

inference2prompt :: Inference -> T.Text
inference2prompt Inf{..} = T.concat [
  -- "level",
  -- T.pack $ show $ nat2int lvl,
  "\n- inference:\n    premise: ",
  tokens2Text $ ("A"):(cn2text gapCN):premise,
  "\n    hypothesis: ",
  tokens2Text $ ("A"):(cn2text gapCN):conclusion,
  "\n    output:\n      reasoning:\n      label:"
  ]

jsemHeader :: T.Text
jsemHeader = "<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE jsem-dataset SYSTEM \"jsem.dtd\"><?xml-stylesheet type=\"text/xsl\" href=\"jsem.xsl\"?><jsem-dataset>"

jsemFooter :: T.Text
jsemFooter = "</jsem-dataset>"

nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S n) = (nat2int n) + 1

int2nat :: Int -> Nat
int2nat n 
  | n > 0  = S $ int2nat $ n-1
  | otherwise = Z

prompt :: String
prompt = [r|
You are an expert in Natural Language Processing (NLP) and logical reasoning.

Your task is to perform a Natural Language Inference (NLI) task. Based strictly on the provided "premise", determine whether the "hypothesis" logically holds true.

Restrict your output label to one of the following two options:
- YES: The premise entails the hypothesis (i.e., if the premise is true, the hypothesis is undeniably true).
- UNK: The premise does not provide enough information to definitively prove the hypothesis is true. (This includes neutral cases where information is missing, as well as cases where the hypothesis contradicts the premise).

[Important Rules]
1. Do not use any external or prior knowledge. Rely completely and solely on the information explicitly stated in the "premise".
2. To maximize accuracy, you must write out your step-by-step logical reasoning process before stating the final label.

[Output Format]
You must output strictly in the following YAML format:

- inference:
    premise: The given premise sentence.  
    hypothesis: The given hypothesis sentence.
    output: 
      reasoning: Step-by-step logical verification of whether the hypothesis can be derived from the premise.
      label: "YES" or "UNK"

[Examples]
- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: A dog is playing outside
    output:
      reasoning: "The premise states that the dog is 'running on the green grass chasing a frisbee', which inherently describes a situation of playing outside. Therefore, the hypothesis can be logically derived from the premise."
      label: "YES"

- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: The dog is a poodle
    output: 
      reasoning: "The premise describes the dog's color (black) and actions, but contains absolutely no information regarding its breed. Therefore, it is impossible to determine if the hypothesis is true based solely on the premise."
      label: "UNK"

- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: A cat is sleeping
    output:
      reasoning: "The premise is exclusively about a dog's actions. There is no information provided about a cat sleeping (it is completely unrelated or contradictory). Therefore, the hypothesis cannot be derived from the premise."
      label: "UNK"

[Input Data]
|]
