{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.FNLP where

import qualified Data.Map as M

import Data.FNLP
import Database.FNLP.SimpleDB

type Language = String

type Dataset = String

type Frequency' = Int

type Cardinality = Int

type Length = Int


----------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------

data TriGramRow = TriGramRow Dataset
                             Language 
                             TriGram 
                             Frequency'
                             Cardinality

trigrams :: TableSchema TriGramRow
trigrams = TableSchema "trigrams" rows mkRow
  where rows = ["dataset     TEXT         NOT NULL"
               ,"language    TEXT         NOT NULL"
               ,"trigram     CHARACTER(3) NOT NULL"
               ,"frequency   INT          NOT NULL"
               ,"cardinality INT          NOT NULL"]
        mkRow (TriGramRow d l t f c) = 
          d /: l /: show t /: f /: c /: []

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
