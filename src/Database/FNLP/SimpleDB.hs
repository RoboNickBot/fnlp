{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.FNLP.SimpleDB 
  ( connect
  , disconnect
  , Connection

  , TableSchema (..)
  , Table
  , getTable
  , getEmptyTable
  , dropTable
  
  , insert
  , sqlCat
  , (/:)
  , sqlsBare
  , sqlsTup2
  , sqlsTup3
  , sqlsTup4

  , SelectionSchema (..)
  , Selector
  , andClause
  , mkSelector
  , select
  
  , fromSql
  , SqlValue
  , Convertible

  ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.List as L
import Data.Convertible (Convertible)


commaSep :: [String] -> String
commaSep = concat . L.intersperse ", "

andSep = concat . L.intersperse " AND "



----------------------------------------------------------------------
-- Database
----------------------------------------------------------------------

connect :: String -> IO Connection
connect = connectSqlite3


----------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------

data TableSchema r = TableSchema { tableName :: String
                                 , cols :: [String]
                                 , toRow :: r -> [SqlValue] }

insertionString :: TableSchema r -> String
insertionString t = let l = length (cols t)
                        n = tableName t
                        header = "INSERT INTO " ++ n ++ " VALUES ("
                        tailer = ")"
                        body = commaSep (replicate l "?")
                    in header ++ body ++ tailer

data Table r = Table { tableSchema :: TableSchema r
                     , connection :: Connection
                     , insertionSt :: Statement }

getTable :: Connection -> TableSchema r -> IO (Table r)
getTable c sc = run c (creatorSt sc) [] 
                >> commit c
                >> prepare c (insertionString sc)
                >>= (\iSt -> return (Table sc c iSt))

getEmptyTable :: Connection -> TableSchema r -> IO (Table r)
getEmptyTable c sc = dropTable' c (tableName sc) 
                     >> getTable c sc

dropTable' :: Connection -> String -> IO ()
dropTable' c n = let s = "DROP TABLE IF EXISTS " ++ n
                 in run c s [] >> commit c

creatorSt :: TableSchema r -> String
creatorSt sc = "CREATE TABLE IF NOT EXISTS " 
               ++ (tableName sc) 
               ++ " (" ++ (rowSchema sc) ++ ")"
               
rowSchema :: TableSchema r -> String
rowSchema = commaSep . cols

dropTable :: Table r -> IO ()
dropTable t = dropTable' (connection t) (tableName (tableSchema t))


----------------------------------------------------------------------
-- Insert
----------------------------------------------------------------------

insert :: Table r -> [r] -> IO ()
insert t rs = let rows = map (toRow (tableSchema t)) rs
              in executeMany (insertionSt t) rows 
                 >> commit (connection t)
                 
sqlCat :: (Convertible v SqlValue) => v -> [SqlValue] -> [SqlValue]
sqlCat v sqls = toSql v : sqls

v /: sqls = sqlCat v sqls

infixr 5 /:

sqlsBare :: (Convertible SqlValue v) => [SqlValue] -> v
sqlsBare (a:[]) = fromSql a

sqlsTup2 :: (Convertible SqlValue v, Convertible SqlValue u) 
         => [SqlValue] -> (v,u)
sqlsTup2 (a:b:[]) = (fromSql a, fromSql b)

sqlsTup3 (a:b:c:[]) = (fromSql a, fromSql b, fromSql c)

sqlsTup4 (a:b:c:d:[]) = (fromSql a, fromSql b, fromSql c, fromSql d)

----------------------------------------------------------------------
-- Select
----------------------------------------------------------------------

data SelectionSchema a g = 
  SelectionSchema { selectionArg :: a -> [SqlValue]
                  , selectionGet :: [[SqlValue]] -> g
                  , selectionFields :: [String]
                  , constraintString :: String }

data Selector r a g = 
  Selector { selectorTable :: Table r
           , selectionSchema :: SelectionSchema a g 
           , selectorSt :: Statement }

selectionString :: Table r -> SelectionSchema a g -> String
selectionString t s = let n = (tableName . tableSchema) t
                          fs = selectionFields s
                          c = constraintString s
                      in "SELECT " ++ commaSep fs 
                         ++ " FROM " ++ n
                         ++ " WHERE " ++ c

andClause :: [String] -> String
andClause = andSep . map (++ " = ?")

mkSelector :: Table r -> SelectionSchema a g -> IO (Selector r a g)
mkSelector t s = prepare (connection t) (selectionString t s) 
                 >>= (\st -> return (Selector t s st))

select :: Selector r a g -> a -> IO g
select s a = let arg = (selectionArg . selectionSchema) s
                 get = (selectionGet . selectionSchema) s
                 st = (selectorSt s)
             in execute st (arg a) 
                >> get <$> fetchAllRows' st


