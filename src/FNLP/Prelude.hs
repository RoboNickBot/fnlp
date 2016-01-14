module FNLP.Prelude where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Pipes
import qualified Pipes.Prelude as P

import FNLP

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
