{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module FNLP.External.Sqlite.TrigramsCosineDB
  ( 
  
    trigramsCosineDB

  ) where

----------------------------------------------------------------------

import Data.Text (Text, pack, unpack)
import qualified Data.Map as M
import qualified Data.List as L
import Pipes
import qualified Pipes.Prelude as P

import Database.SimpleDB

import FNLP

import Data.FNLP.Common
import Data.FNLP.Freq

----------------------------------------------------------------------

defaultChunkSize = 100

defaultCardinality = 50

type ChunkID = Int

type Cardinality = Int



data TriGramRow = TriGramRow ChunkID
                             Class
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
  

instance Convertible Class SqlValue where
  safeConvert = convertVia (undefined::Text)
  
instance Convertible SqlValue Class where
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


type Chunk = [Classified (FreqList TriGram)]

mkChunk :: [(Class, TriGram, Frequency)] -> Chunk
mkChunk = map mkFreq . combLs . map tupSep
  where tupSep (a,b,c) = (a,(b,c))

        combLs :: [(Class, (TriGram, Frequency))] -> [(Class, [(TriGram, Frequency)])]
        combLs = M.toList . L.foldl' ap M.empty
          where ap m (l,f) = let fs = case M.lookup l m of
                                        Just a -> a
                                        _ -> []
                             in M.insert l (f:fs) m

        mkFreq :: (Class, [(TriGram, Frequency)]) -> (Class, FreqList TriGram)
        mkFreq (l,f) = (l, FreqList . M.fromList $ f)
        

chunkRows :: Monad m 
          => Int 
          -> Pipe (Class, FreqList TriGram) [TriGramRow] m ()
chunkRows size = pipe 0 size
  where pipe c s = do (l,t) <- await
                      yield (mkTGRows (c,l,t))
                      if s <= 1
                         then pipe (c + 1) size
                         else pipe c (s - 1)

mkTGRows :: (ChunkID, Class, FreqList TriGram) -> [TriGramRow]
mkTGRows (ch,cl,fs) = let row ((tg,fr),c) = TriGramRow ch cl tg fr c
                      in map row (zip (freqList fs) [0,1..])


insertConsumer :: Table r -> Consumer [r] IO ()
insertConsumer t = P.mapM_ (insert t)


trigramsCosineDB :: FilePath -> String -> Depot (Classified (FreqList TriGram))
trigramsCosineDB path chan = Depot (openAccepter path chan) (openProvider path chan)

openAccepter :: FilePath -> String -> Accepter (Classified (FreqList TriGram))
openAccepter path chan = 
  Accepter (do conn <- connect path
               t <- getTable conn (trigramsSchema chan)
               let chunkSize = 100
                   cpipe = chunkRows chunkSize
                   cData = insertConsumer t
               return (cpipe >-> cData))

openProvider :: FilePath -> String -> Provider (Classified (FreqList TriGram))
openProvider path chan = 
  Provider (do conn <- connect path
               t <- getTable conn (trigramsSchema chan)
               chunkIDs <- mkSelector t listChunks >>= \s -> select s ()
               let card = defaultCardinality
                   chunkSize = defaultChunkSize 
               sData <- mkSelector t (chunks card)
               return (spoutCat sData chunkIDs))
