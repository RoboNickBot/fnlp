module FNLP.Common
  (
  
    module Data.FNLP.Common
  , module Data.FNLP.Freq
  , module FNLP.External.FileSystem.SampSentsDB
  , module FNLP.External.Sqlite.TrigramsCosineDB

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
import FNLP.External.FileSystem.SampSentsDB
import FNLP.External.Sqlite.TrigramsCosineDB

sampleCorpus :: [Classified Corpus]
sampleCorpus = [(convert "eng", (corpus . pack) "Hello World!")]

toyDB :: Provider (Classified Corpus)
toyDB = Provider (return (each sampleCorpus))

split :: Int -> Classified Corpus -> (Classified Corpus, Classified Corpus)
split = undefined

storeProfiles :: Monad m 
              => Int  
              -> Producer (Classified Corpus) m ()
              -> Consumer (Classified (FreqList TriGram)) m ()
              -> Consumer (Classified (FreqList TriGram)) m ()
              -> Effect m ()
storeProfiles i pro cMain cTest = 
  let trigs = (>->) (P.map (fmap features))
  in share (split i) pro (trigs cMain) (trigs cTest)
