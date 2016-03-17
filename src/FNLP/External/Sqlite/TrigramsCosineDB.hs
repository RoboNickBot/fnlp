{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module FNLP.External.Sqlite.TrigramsCosineDB
  ( 

    TrigramDB  
  , trigramDB
  , classifier
  , crossCheck

  ) where

----------------------------------------------------------------------

import Data.Text (Text, pack, unpack)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Pipes
import qualified Pipes.Prelude as P

import Database.SimpleDB

import FNLP
import FNLP.Classes

import Data.FNLP.Common
import Data.FNLP.Freq

----------------------------------------------------------------------

defaultChunkSize = 100

defaultCardinality = 50

defaultTesting = 30

----------------------------------------------------------------------

data TrigramDB = TrigramDB { _conn :: Connection
                           , _training' :: Chan
                           , _testing' :: Chan }

data Chan = Chan { _p :: Producer (Ann (FreqList TriGram)) IO ()
                 , _c :: Consumer (Ann (FreqList TriGram)) IO ()
                 , _dropAction :: IO () } 

trigramDB :: FilePath -> IO TrigramDB
trigramDB path = connect path >>= populateDB

populateDB :: Connection -> IO TrigramDB
populateDB conn = TrigramDB conn 
             <$> mkChan conn "training" 
             <*> mkChan conn "testing"

mkChan :: Connection -> String -> IO Chan
mkChan conn chan = let tbl = getChan chan conn
                   in Chan 
                      <$> (openProducer =<< tbl) 
                      <*> (openConsumer <$> tbl)
                      <*> (dropTable    <$> tbl)


closeDB :: TrigramDB -> IO ()
closeDB = disconnect . _conn

instance External TrigramDB IO where
  wipeAction db = _dropAction (_training' db) 
                  >> _dropAction (_testing' db) 

  
  refresh = populateDB . _conn

instance Learner TrigramDB (Ann Corpus) IO where
  learner (TrigramDB _ (Chan _ trC _) (Chan _ tsC _)) = 
    splitStore defaultTesting (p trC) (p tsC)
    where p c = P.map (fmap features) >-> c


splitStore :: Monad m 
           => Int
           -> Consumer (Ann Corpus) m ()
           -> Consumer (Ann Corpus) m ()
           -> Consumer (Ann Corpus) m ()
splitStore per trC tsC = P.mapM_ (runEffect . mash (divCorpus' per) trC tsC)

divCorpus' per (tag, c) = let (training, testing) = divCorpus per c
                          in ((tag, training), (tag, testing))

classifier :: Featuring a (FreqList TriGram) 
           => TrigramDB
           -> Pipe a [Ann Double] IO ()
classifier db = P.mapM (classifier' db . features)

classifier' :: TrigramDB -> FreqList TriGram -> IO [Ann Double]
classifier' db a = L.sortBy sf <$> P.toListM (p a)
  where p a = (_p (_training' db)) >-> P.map (fmap $ cosine a)
        sf (t1,s1) (t2,s2) = compare s2 s1

crossCheck :: TrigramDB -> Producer (Ann (Maybe Tag)) IO ()
crossCheck db = (_p (_testing' db)) 
                >-> P.mapM (mapM (classifier' db)) 
                >-> P.map (fmap (fmap fst . listToMaybe))

getChan :: String -> Connection -> IO (Table TriGramRow)
getChan chan conn = getTable conn (trigramsSchema chan)

openConsumer :: Table TriGramRow -> Consumer (Ann (FreqList TriGram)) IO ()
openConsumer table = let chunkSize = defaultChunkSize
                         cpipe = chunkRows chunkSize
                         cData = insertConsumer table
                     in cpipe >-> cData

openProducer :: Table TriGramRow -> IO (Producer (Ann (FreqList TriGram)) IO ())
openProducer table = let card = defaultCardinality
                         chunkSize = defaultChunkSize
                     in spoutCat 
                        <$> mkSelector table (chunks card) 
                        <*> (mkSelector table listChunks >>= \s -> select s ())


----------------------------------------------------------------------


type ChunkID = Int

type Cardinality = Int



data TriGramRow = TriGramRow ChunkID
                             Tag
                             TriGram 
                             Frequency
                             Cardinality


instance Convertible SqlValue Frequency where
  safeConvert = convertVia (undefined::Int)

instance Convertible Frequency SqlValue where
  safeConvert = convertVia (undefined::Int)


instance Convertible SqlValue TriGram where
  safeConvert = convertVia (undefined::Text)

instance Convertible TriGram SqlValue where
  safeConvert = convertVia (undefined::Text)
  

instance Convertible Tag SqlValue where
  safeConvert = convertVia (undefined::Text)
  
instance Convertible SqlValue Tag where
  safeConvert = convertVia (undefined::Text)


----------------------------------------------------------------------
-- Table Schema
----------------------------------------------------------------------

trigramsSchema :: String -> TableSchema TriGramRow
trigramsSchema name = TableSchema name rows mkRow
  where rows = ["chunkid     INT          NOT NULL"
               ,"class       TEXT         NOT NULL"
               ,"trigram     TEXT         NOT NULL"
               ,"frequency   INT          NOT NULL"
               ,"cardinality INT          NOT NULL"]
        mkRow (TriGramRow ch cl tr fr cr) = 
          ch /: cl /: (convert :: TriGram -> SqlValue) tr /: fr /: cr /: []


----------------------------------------------------------------------
-- Selectors
----------------------------------------------------------------------

noArgs = const []

distinct :: (Convertible SqlValue v) => v -> String -> SelectionSchema () [v]
distinct _ f = SelectionSchema noArgs (map sqlsBare) ["DISTINCT " ++ f] ""

listChunks :: SelectionSchema () [ChunkID]
listChunks = distinct undefined "chunkid"

chunks :: Cardinality -> SelectionSchema ChunkID Chunk
chunks c = SelectionSchema (\i -> i /: c /: [])
                           (mkChunk . map sqlsTup3)
                           ["class","trigram","frequency"]
                           s
  where s = "chunkid = ? AND cardinality < ?"


type Chunk = [Ann (FreqList TriGram)]

mkChunk :: [(Tag, TriGram, Frequency)] -> Chunk
mkChunk = map mkFreq . combLs . map tupSep
  where tupSep (a,b,c) = (a,(b,c))

        combLs :: [(Tag, (TriGram, Frequency))] -> [(Tag, [(TriGram, Frequency)])]
        combLs = M.toList . L.foldl' ap M.empty
          where ap m (l,f) = let fs = case M.lookup l m of
                                        Just a -> a
                                        _ -> []
                             in M.insert l (f:fs) m

        mkFreq :: (Tag, [(TriGram, Frequency)]) -> (Tag, FreqList TriGram)
        mkFreq (l,f) = (l, FreqList . M.fromList $ f)
        

chunkRows :: Monad m 
          => Int 
          -> Pipe (Tag, FreqList TriGram) [TriGramRow] m ()
chunkRows size = pipe 0 size
  where pipe c s = do (l,t) <- await
                      yield (mkTGRows (c,l,t))
                      if s <= 1
                         then pipe (c + 1) size
                         else pipe c (s - 1)

mkTGRows :: (ChunkID, Tag, FreqList TriGram) -> [TriGramRow]
mkTGRows (ch,cl,fs) = let row ((tg,fr),c) = TriGramRow ch cl tg fr c
                      in map row (zip (freqList fs) [0,1..])


insertConsumer :: Table r -> Consumer [r] IO ()
insertConsumer t = P.mapM_ (insert t)

