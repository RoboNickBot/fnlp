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
import System.Directory (getDirectoryContents)
import System.FilePath
import System.IO (hPutStrLn, hFlush, stderr)
import qualified System.IO.Strict as Strict

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

instance Convertible SqlValue TriGram where
  safeConvert = convertVia (undefined::String)

instance Convertible TriGram String where
  safeConvert = Right . show

instance Convertible TriGram SqlValue where
  safeConvert = convertVia (undefined::String)


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
          d /: ch /: l /: show t /: f /: c /: []

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
getAllTrigs = SelectionSchema (const []) 
                              (map sqlsTup4) 
                              ["language","trigram","frequency","cardinality"]
                              "dataset = \"main\""

----------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------

langPipe :: Selector TriGramRow 
                     (Dataset, Language, Cardinality) 
                     (FreqList TriGram) 
         -> [Language] 
         -> Producer (Language, FreqList TriGram) IO ()
langPipe s ls = (each ls) >-> (Pipes.mapM dbg) >-> (Pipes.mapM get)
  where get l = (,) l <$> select s ("main",l,50)
        dbg l = hPutStrLn stderr ("serving lang " ++ l ++ " ...") 
                >> hFlush stderr
                >> return l

getRealDirectoryContents = 
  fmap (filter (\a -> (a /= ".") && (a /= ".."))) 
  <$> getDirectoryContents

crubadanFiles :: FilePath -> IO [(Language,FilePath)]
crubadanFiles r = fmap (\d -> (d, r </> d </> "SAMPSENTS")) 
                  <$> getRealDirectoryContents r

crubadanNames = getRealDirectoryContents

build :: Connection -> [(Language, String)] -> IO (Table TriGramRow)
build c ps = do t <- getTable c trigrams
                undefined -- for (readMany) (insertLang t)

readFilesP :: [String] -> Producer String IO ()
readFilesP (p:ps) = do lift (deb p)
                       text <- lift (Strict.readFile p)
                       yield text
                       readFilesP ps
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 
                >> hFlush stderr 
readFilesP _ = return ()

readLangFiles :: Pipe (Language, FilePath) (Language, String) IO ()
readLangFiles = Pipes.mapM (mapM (\f -> deb f >> Strict.readFile f))
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 

mkTGRows :: (Language, String) -> [TriGramRow]
mkTGRows (l,s) = let fs = (freqList . features) (mkCharSeq s)
                     row ((tg,fr),c) = TriGramRow "main" 0 l tg fr c
                 in map row (zip fs [0,1..])

insertConsumer :: Table r -> Consumer [r] IO ()
insertConsumer t = Pipes.mapM_ (insert t)

trigramstest2 :: String -> IO ()
trigramstest2 r = do conn <- connect "trigramsTest2.sqlite3"
                     table <- getEmptyTable conn trigrams
                     files <- crubadanFiles r
                     runEffect (buildDB (files) table)
                     sel <- mkSelector table getLang
                     res <- select sel ("main","en",20) 
                     disconnect conn
                     sequence_ (map print (freqList res))

buildTrigramsTable :: Connection -> String -> IO ()
buildTrigramsTable conn r = 
  do table <- getEmptyTable conn trigrams
     files <- crubadanFiles r
     runEffect (buildDB files table) 

buildDB :: [(Language,FilePath)] -> Table TriGramRow -> Effect IO ()
buildDB fs t = each fs 
               >-> readLangFiles 
               >-> Pipes.map mkTGRows 
               >-> insertConsumer t

trigramstest :: IO ()
trigramstest = do conn <- connect "trigramsTest.sqlite3"
                  table <- getTable conn trigrams
                  sel <- mkSelector table getAllTrigs
                  let testDatas = [("eng","Hello World he")
                                  ,("jap","Genki desu desu!")
                                  ,("rus","Dostaprimachatlnista")]
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
