module FNLP.Common
  (
  
    module Data.FNLP.Common
  , module Data.FNLP.Freq
  , module FNLP.External.OldFS
  , module FNLP.External.TriGramsDB
  
  , sampleCorpus
  , toyDB
  , split
  , storeProfiles

  ) where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Pipes
import qualified Pipes.Prelude as P

import FNLP

import Data.FNLP.Common
import Data.FNLP.Freq
import FNLP.External.OldFS
import FNLP.External.TriGramsDB

sampleCorpus :: [Meta Corpus]
sampleCorpus = [("eng", (corpus . pack) "Hello World!")]

toyDB :: ReadOnly Corpus
toyDB = each sampleCorpus

split :: Int -> Meta Corpus -> (Meta Corpus, Meta Corpus)
split = undefined

storeProfiles :: Monad m 
              => Int  
              -> Producer (Meta Corpus) m ()
              -> Consumer (Meta (FreqList TriGram)) m ()
              -> Consumer (Meta (FreqList TriGram)) m ()
              -> Effect m ()
storeProfiles i pro cMain cTest = 
  let trigs = (>->) (P.map (fmap features))
  in share (split i) pro (trigs cMain) (trigs cTest)
