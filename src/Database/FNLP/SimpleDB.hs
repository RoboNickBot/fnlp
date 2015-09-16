{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.FNLP.SimpleDB 
  ( connect
  , disconnect
  , Connection

  , TableSchema (..)
  , Table
  , getTable
  , dropTable
  
  , insert
  , sqlCat
  , (/:)
  
  , SelectionSchema (..)
  , Selector
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

creatorSt :: TableSchema r -> String
creatorSt sc = "CREATE TABLE IF NOT EXISTS " 
               ++ (tableName sc) 
               ++ " (" ++ (rowSchema sc) ++ ")"
               
rowSchema :: TableSchema r -> String
rowSchema = commaSep . cols

dropTable :: Table r -> IO ()
dropTable t = let s = "DROP TABLE IF EXISTS " 
                      ++ ((tableName . tableSchema) t)
              in run (connection t) s [] >> commit (connection t)


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

mkSelector :: Table r -> SelectionSchema a g -> IO (Selector r a g)
mkSelector t s = prepare (connection t) (selectionString t s) 
                 >>= (\st -> return (Selector t s st))

select :: Selector r a g -> a -> IO g
select s a = let arg = (selectionArg . selectionSchema) s
                 get = (selectionGet . selectionSchema) s
                 st = (selectorSt s)
             in execute st (arg a) 
                >> get <$> fetchAllRows' st


