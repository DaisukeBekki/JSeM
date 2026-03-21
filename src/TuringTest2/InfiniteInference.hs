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
  infOf
  ) where

import Data.Char (toUpper) -- base
import Data.List (nub)     -- base
import qualified Data.Text    as T -- text
import Control.Monad (guard) -- base
import Data.Nat -- natsi
import JSeM as J -- jsem

data CommonNoun = HOUSE | MALT | RAT | CAT | DOG | COW | MAIDEN | MAN | PRIEST | COCK | CORN | FARMER deriving (Eq, Show)

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
  CORN -> "kernel of corn"
  FARMER -> "farmer"

type Token = T.Text

-- relativise :: CommonNoun -> [Token] -> [Token]
-- relativise cn ts = (cn2text cn):("that"):ts

tokens2Text :: [Token] -> T.Text
tokens2Text tokens = 
  case T.uncons $ T.intercalate " " tokens of
    Nothing -> T.empty
    Just (c,t) -> T.snoc (T.cons (toUpper c) t) '.' -- | Capitalizing the first char, adding "."

data Subsumption = Subsumption {
  lv :: Nat
  , phenomenon :: T.Text
  , gapCN :: CommonNoun
  , subset :: [Token]
  , superset :: [Token]
  } deriving (Eq, Show)

data Inference = Inference {
  lvl :: Nat
  , phenomena :: [T.Text]
  , label :: J.JSeMLabel
  , premise :: [Token]
  , conclusion :: [Token]
  } deriving (Eq, Show)

basicSubsumption :: [Subsumption]
basicSubsumption = [
  Subsumption {
    lv = Z
    , phenomenon = "Disjunction"
    , gapCN = DOG
    , subset    = ["chased John"]
    , superset = ["chased John or Mary"]
    },
  Subsumption {
    lv = Z
    , phenomenon = "Disjunction"
    , gapCN = CAT
    , subset    = ["loves John and Mary"]
    , superset = ["loves John"]
    },
  Subsumption {
    lv = Z
    , phenomenon = "Disjunction"
    , gapCN = MAIDEN
    , subset    = ["sings"]
    , superset = ["sings or dances"]
    },
  Subsumption {
    lv = Z
    , phenomenon = "Conjunction"
    , gapCN = MAIDEN
    , subset    = ["jogs and runs"]
    , superset = ["jogs"]
    }
  ]

data Rel = Rel {
  relName :: T.Text,
  sbjCN :: CommonNoun,
  objCN :: CommonNoun
  }

relations1 :: [Rel]
relations1 = [
  Rel "inhabited by" HOUSE FARMER
  , Rel "lay in" MALT HOUSE
  , Rel "ate" RAT MALT
  , Rel "killed" CAT RAT
  , Rel "worried" DOG CAT
  , Rel "tossed" COW DOG
  , Rel "milked" MAIDEN COW
  , Rel "kissed" MAN MAIDEN
  , Rel "married" PRIEST MAN
  , Rel "waked" COCK PRIEST
  , Rel "kept" CORN COCK
  , Rel "sowed" FARMER CORN
  ]

relations2 :: [Rel]
relations2 = [
  Rel "sheltered" HOUSE FARMER
  , Rel "filled" MALT HOUSE
  , Rel "devoured" RAT MALT
  , Rel "cornered" CAT RAT
  , Rel "chased" DOG CAT
  , Rel "kicked" COW DOG
  , Rel "tended" MAIDEN COW
  , Rel "wooed" MAN MAIDEN
  , Rel "blessed" PRIEST MAN
  , Rel "startled" COCK PRIEST
  , Rel "attracted" CORN COCK
  , Rel "planted" FARMER CORN
  ]

data Monotonicity = Upward | Downward deriving (Eq, Show)
data Quantifier = Quantifier T.Text Monotonicity Monotonicity deriving (Eq, Show)

quantifiers :: [Quantifier]
quantifiers = [
  Quantifier   "every"     Downward Upward
  , Quantifier "some"      Upward   Upward
  , Quantifier "no"        Downward Downward
  , Quantifier "not every" Upward   Downward
  ]

subsumption :: Nat -> [Rel] -> [Subsumption]
subsumption Z _ = basicSubsumption
subsumption (S n) rels = do
  sub <- subsumption n rels
  rel <- rels
  let o = objCN rel
  guard $ o == gapCN sub
  Quantifier qname left _ <- quantifiers
  let (subsetTokens, supersetTokens) = case left of
        Downward -> (superset sub, subset sub)
        Upward   -> (subset sub, superset sub)
  let subsetN   = (relName rel):qname:(cn2text o):("that"):subsetTokens
      supersetN = (relName rel):qname:(cn2text o):("that"):supersetTokens
  return $ Subsumption (S $ lv sub) (phenomenon sub) (sbjCN rel) subsetN supersetN 
  
infOf :: Nat -> [Inference]
infOf n = do
  sbjS <- subsumption n relations1
  objS <- subsumption n relations2
  guard $ phenomenon sbjS /= phenomenon objS
  guard $ gapCN sbjS == gapCN objS 
  Quantifier qname left right <- quantifiers
  label <- [J.YES, J.UNKNOWN]
  let (sbjset1, sbjset2, objset1, objset2) = 
        case (label, left, right) of
          (YES, Upward, Upward)     -> (subset, superset, subset, superset)
          (YES, Upward, Downward)   -> (subset, superset, superset, subset)
          (YES, Downward, Upward)   -> (superset, subset, subset, superset)
          (YES, Downward, Downward) -> (superset, subset, superset, subset)
          (_, Upward, Upward)     -> (superset, subset, superset, subset)
          (_, Upward, Downward)   -> (superset, subset, subset, superset)
          (_, Downward, Upward)   -> (subset, superset, superset, subset)
          (_, Downward, Downward) -> (subset, superset, subset, superset)
  return $ Inference 
             (lv sbjS) 
             [phenomenon sbjS,phenomenon objS] 
             label
             (qname:(cn2text $ gapCN sbjS):("that"):(sbjset1 sbjS) ++ (objset1 objS))
             (qname:(cn2text $ gapCN sbjS):("that"):(sbjset2 sbjS) ++ (objset2 objS))

inference2jsem :: Inference -> T.Text
inference2jsem Inference{..} = T.concat [
  "<problem answer=\"",
  T.pack $ show label,
  "\" inference_type=\"entailment\" language=\"en\" phenomena=\"",
  T.intercalate ", " $ nub $ (T.concat ["level", T.pack $ show $ nat2int lvl]):phenomena,
  "\"><p idx=\"1\"><script>",
  tokens2Text premise,
  "</script></p><h><script>",
  tokens2Text conclusion,
  "</script></h></problem>"
  ]

inference2prompt :: Inference -> T.Text
inference2prompt Inference{..} = T.concat [
  -- "level",
  -- T.pack $ show $ nat2int lvl,
  "\n- inference:\n    premise: ",
  tokens2Text premise,
  "\n    hypothesis: ",
  tokens2Text conclusion,
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

