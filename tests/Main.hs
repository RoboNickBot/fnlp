module Main (main) where

import System.Environment
import System.IO

import Database.FNLP

main = getArgs >>= (\args -> case args of
                               ["build"] -> build
                               ["analize"] -> analize
                               _ -> build >> analize)

build = do hPutStrLn stderr 
                     "Performing Build Test (building \"bench.sqlite3\")"
           performBuild "bench.sqlite3" 
                        "crubadan-data-small"

analize = do hPutStrLn stderr 
                       "Performing Analysis Test (output to \"AREPORT.txt\")"
             performAnalysis "bench.sqlite3" 
                             "crubadan-data-small" 
                             "AREPORT.txt"
