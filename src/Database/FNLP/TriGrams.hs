{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.FNLP.TriGrams where

import qualified Data.Map as M
import Data.Convertible
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.List as L
import System.Directory (getDirectoryContents)
import System.FilePath
import System.IO (hPutStrLn, hFlush, stderr)
import qualified Data.Text.IO as TIO (readFile)
import qualified System.IO.Strict as Strict
import Control.Monad (forever)

import Pipes
import qualified Pipes.Prelude as Pipes

import Data.FNLP
import Database.FNLP.SimpleDB

type Language = String

type Dataset = String

type Cardinality = Int

type Length = Int

type ChunkID = Int


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Convertible String TriGram where
  safeConvert a = case readMaybe a of
                    Just b -> Right b
                    _ -> convError "DOES NOT COMPUTE" a

instance Convertible TriGram String where
  safeConvert = Right . show


instance Convertible SqlValue TriGram where
  safeConvert = convertVia (undefined::T.Text)

instance Convertible TriGram SqlValue where
  safeConvert = convertVia (undefined::T.Text)


instance Convertible Int Frequency where
  safeConvert = Right . Frequency
  
instance Convertible SqlValue Frequency where
  safeConvert = convertVia (undefined::Int)

instance Convertible Frequency Int where
  safeConvert = Right . frequency
  
instance Convertible Frequency SqlValue where
  safeConvert = convertVia (undefined::Int)


----------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------

data TriGramRow = TriGramRow Dataset
                             ChunkID
                             Language 
                             TriGram 
                             Frequency
                             Cardinality
                  deriving Show

trigrams :: TableSchema TriGramRow
trigrams = TableSchema "trigrams" rows mkRow
  where rows = ["dataset     TEXT         NOT NULL"
               ,"chunkid     INT          NOT NULL"
               ,"language    TEXT         NOT NULL"
               ,"trigram     TEXT         NOT NULL"
               ,"frequency   INT          NOT NULL"
               ,"cardinality INT          NOT NULL"]
        mkRow (TriGramRow d ch l t f c) = 
          d /: ch /: l /: (convert :: TriGram -> SqlValue) t /: f /: c /: []

data LengthRow = LengthRow Dataset Language Length 
                 deriving Show

lengths :: TableSchema LengthRow
lengths = TableSchema "lengths" rows mkRow
  where rows = ["dataset  TEXT NOT NULL"
               ,"language TEXT NOT NULL"
               ,"length   INT  NOT NULL"]
        mkRow (LengthRow d lng lth) = 
          d /: lng /: lth /: []


----------------------------------------------------------------------
-- Selectors
----------------------------------------------------------------------

noArgs = const []

getLengths :: SelectionSchema Dataset (M.Map Language Length)
getLengths = SelectionSchema (/: []) 
                             packLengths
                             ["language","length"]
                             "dataset = ?"

packLengths :: [[SqlValue]] -> M.Map Language Length
packLengths = M.fromList . map tup
  where tup (lng:lth:[]) = (fromSql lng, fromSql lth)

getLang :: SelectionSchema 
             (Dataset, Language, Cardinality) 
             (FreqList TriGram)
getLang = SelectionSchema (\(d,l,c) -> d /: l /: c /: [])
                          (FreqList . M.fromList . map sqlsTup2)
                          ["trigram","frequency"]
                          s
  where s = "dataset = ? AND language = ? AND cardinality < ?"

getAllTrigs :: SelectionSchema () [(Language,TriGram,Frequency,Cardinality)]
getAllTrigs = SelectionSchema noArgs
                              (map sqlsTup4) 
                              ["language","trigram","frequency","cardinality"]
                              "dataset = \"main\""

distinct :: (Convertible SqlValue v) => v -> String -> SelectionSchema () [v]
distinct _ f = SelectionSchema noArgs (map sqlsBare) ["DISTINCT " ++ f] ""

listChunks :: SelectionSchema () [ChunkID]
listChunks = distinct undefined "chunkid"

listLangs :: SelectionSchema () [Language]
listLangs = distinct undefined "language"

getChunk :: SelectionSchema (Dataset, ChunkID, Cardinality) Chunk
getChunk = SelectionSchema (\(d,i,c) -> d /: i /: c /: [])
                           (mkChunk . map sqlsTup3)
                           ["language","trigram","frequency"]
                           s
  where s = "dataset = ? AND chunkid = ? AND cardinality < ?"

chunks :: Cardinality -> Dataset -> SelectionSchema ChunkID Chunk
chunks c d = SelectionSchema (\i -> d /: i /: c /: [])
                             (mkChunk . map sqlsTup3)
                             ["language","trigram","frequency"]
                             s
  where s = "dataset = ? AND chunkid = ? AND cardinality < ?"

mkChunk :: [(Language, TriGram, Frequency)] -> Chunk
mkChunk = map mkFreq . combLs . map tupSep
  where tupSep (a,b,c) = (a,(b,c))

        combLs :: [(Language, (TriGram, Frequency))] -> [(Language, [(TriGram, Frequency)])]
        combLs = M.toList . L.foldl' ap M.empty
          where ap m (l,f) = let fs = case M.lookup l m of
                                        Just a -> a
                                        _ -> []
                             in M.insert l (f:fs) m

        mkFreq :: (Language, [(TriGram, Frequency)]) -> (Language, FreqList TriGram)
        mkFreq (l,f) = (l, FreqList . M.fromList $ f)


----------------------------------------------------------------------
-- Producers
----------------------------------------------------------------------

type Chunk = [(Language, FreqList TriGram)]

spout :: Show a => Selector r a b -> [a] -> Producer b IO ()
spout s as = each as >-> Pipes.mapM dbg >-> Pipes.mapM (select s)
  where dbg a = hPutStrLn stderr ("spouting item \"" 
                                  ++ show a 
                                  ++ "\" ...")
                >> hFlush stderr
                >> return a

spoutCat :: Show a => Selector r a [b] -> [a] -> Producer b IO ()
spoutCat s as = spout s as >-> Pipes.concat

chunkPipe :: Selector TriGramRow
                      (Dataset, ChunkID, Cardinality)
                      Chunk
          -> Cardinality
          -> Dataset
          -> [ChunkID]
          -> Producer Chunk IO ()
chunkPipe s c d is = each is >-> Pipes.mapM dbg >-> Pipes.mapM get
  where get i = select s (d,i,c)
        dbg i = hPutStrLn stderr ("serving chunk " ++ show i ++ " ...")
                >> hFlush stderr
                >> return i

breakChunks :: Monad m => Pipe [a] a m ()
breakChunks = Pipes.concat

langPipe' :: Selector TriGramRow 
                      (Dataset, Language, Cardinality) 
                      (FreqList TriGram)
          -> Dataset
          -> [Language] 
          -> Producer (Language, FreqList TriGram) IO ()
langPipe' s d ls = each ls >-> Pipes.mapM dbg >-> Pipes.mapM get
  where get l = (,) l <$> select s (d,l,50)
        dbg l = hPutStrLn stderr ("serving lang " ++ l ++ " ...") 
                >> hFlush stderr
                >> return l

langPipe s ls = langPipe' s "data" ls

getRealDirectoryContents = 
  fmap (filter (\a -> (a /= ".") && (a /= ".."))) 
  <$> getDirectoryContents

crubadanFiles :: FilePath -> IO [(Language,FilePath)]
crubadanFiles r = fmap (\d -> (d, r </> d </> "SAMPSENTS")) 
                  <$> getRealDirectoryContents r

crubadanNames = getRealDirectoryContents

readFilesP :: [String] -> Producer T.Text IO ()
readFilesP (p:ps) = do lift (deb p)
                       text <- lift (TIO.readFile p)
                       yield text
                       readFilesP ps
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 
                >> hFlush stderr 
readFilesP _ = return ()

readLangFiles :: Pipe (Language, FilePath) (Language, T.Text) IO ()
readLangFiles = Pipes.mapM (mapM (\f -> deb f >> TIO.readFile f))
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 


----------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------

chunkRows :: Monad m 
          => Int 
          -> Pipe (Dataset, Language, T.Text) [TriGramRow] m ()
chunkRows size = pipe 0 size
  where pipe c s = do (d,l,t) <- await
                      yield (mkTGRows'' (c,d,l,t))
                      if s <= 1
                         then pipe (c + 1) size
                         else pipe c (s - 1)

mkTGRows'' :: (ChunkID, Dataset, Language, T.Text) -> [TriGramRow]
mkTGRows'' (cid,d,l,s) = 
  let fs = (freqList . features) (corpus s)
      row ((tg,fr),c) = TriGramRow d cid l tg fr c
  in map row (zip fs [0,1..])

mkTGRows' :: (Dataset, Language, T.Text) -> [TriGramRow]
mkTGRows' (d,l,s) = mkTGRows'' (0,d,l,s)

mkTGRows (l,s) = mkTGRows' ("data",l,s)

insertConsumer :: Table r -> Consumer [r] IO ()
insertConsumer t = Pipes.mapM_ (insert t)

trigramstest2 :: String -> IO ()
trigramstest2 r = do conn <- connect "trigramsTest2.sqlite3"
                     table <- getEmptyTable conn trigrams
                     files <- crubadanFiles r
                     runEffect (buildDB 1 0 (files) table)
                     sel <- mkSelector table getLang
                     res <- select sel ("main","en",20) 
                     disconnect conn
                     sequence_ (map print (freqList res))

buildTrigramsTable :: Connection -> Int -> Int -> String -> IO ()
buildTrigramsTable conn chunkSize p r = 
  do table <- getEmptyTable conn trigrams
     files <- crubadanFiles r
     runEffect (buildDB chunkSize p files table) 

buildDB :: Int -> Int -> [(Language,FilePath)] -> Table TriGramRow -> Effect IO ()
buildDB chunkSize p fs t = each fs 
                           >-> readLangFiles
                           >-> splitLangFiles p
                           >-> chunkRows chunkSize -- Pipes.map mkTGRows'
                           >-> insertConsumer t

splitLangFiles :: Monad IO
               => Int 
               -> Pipe (Language, T.Text) (Dataset, Language, T.Text) IO ()
splitLangFiles p = forever $ do (l,s) <- await
                                lift (hPutStrLn stderr "splitting lang file")
                                let ss = T.lines s
                                    testLs = ceiling $ fromIntegral (length ss * p) / 100
                                    (sTest,sData) = L.splitAt testLs ss
                                yield ("test", l, T.concat sTest)
                                lift (hPutStrLn stderr "yielded 1")
                                yield ("data", l, T.concat sData)
                                lift (hPutStrLn stderr "yielded 2")

fakeSplitLangFiles _ = Pipes.map (\(l,s) -> ("data",l,s))

trigramstest :: IO ()
trigramstest = do conn <- connect "trigramsTest.sqlite3"
                  table <- getTable conn trigrams
                  sel <- mkSelector table getAllTrigs
                  let testDatas = [("eng",T.pack "Hello World he")
                                  ,("jap",T.pack "Genki desu desu!")
                                  ,("rus",T.pack "Dostaprimachatlnista")]
                  runEffect (each testDatas 
                             >-> Pipes.map mkTGRows
                             >-> insertConsumer table)
                  res1 <- select sel () 
                  sequence_ $ map print res1
                  dropTable table
                  disconnect conn

test :: IO ()
test = do conn <- connect "test.sqlite3"
          table <- getTable conn lengths
          sel <- mkSelector table getLengths
          print testData
          ret1 <- select sel "main"          

          insert table testData
          ret2 <- select sel "main"
          dropTable table
          table2 <- getTable conn lengths
          sel2 <- mkSelector table getLengths
          ret3 <- select sel "alt"
          
          disconnect conn
          putStrLn $ "ret1: " ++ show ret1
          putStrLn $ "ret2: " ++ show ret2
          putStrLn $ "ret3: " ++ show ret3

testData :: [LengthRow]
testData = [lr "main" "eng" 56
           ,lr "main" "rus" 66
           ,lr "alt"  "rus" 88
           ,lr "main" "jap" 5783]
  where lr = LengthRow
