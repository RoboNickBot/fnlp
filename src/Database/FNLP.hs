{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.FNLP where

import Database.HDBC
import Database.HDBC.Sqlite3

import System.IO (hPutStrLn, stderr)

import Data.FNLP


----------------------------------------------------------------------
-- Database
----------------------------------------------------------------------

connectDB :: String -> IO Connection
connectDB = connectSqlite3

disconnectDB :: Connection -> IO ()
disconnectDB = disconnect


----------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------

data TableSchema r = TableSchema { tableName :: String
                                 , rowSchema :: String
                                 , toRow :: r -> String }

data Table r = Table { tableSchema :: TableSchema r
                     , connection :: Connection }

getTable :: Connection -> TableSchema r -> IO (Table r)
getTable c sc = run c (creatorSt sc) [] 
                >> commit c 
                >> return (Table sc c)

creatorSt :: TableSchema r -> String
creatorSt sc = "CREATE TABLE IF NOT EXISTS " 
               ++ (tableName sc) 
               ++ " (" ++ (rowSchema sc) ++ ")"

dropTable :: Table r -> IO ()
dropTable t = let s = "DROP TABLE IF EXISTS " 
                      ++ ((tableName . tableSchema) t)
              in run (connection t) s [] >> return ()

select :: Table r -> [String] -> [(String, String)] -> IO [r]
select = undefined

newtype Cardinality = Cardinality Int
  deriving (Show, Read, Eq, Ord, Num)
