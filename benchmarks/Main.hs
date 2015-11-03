module Main (main) where

import System.Environment
import System.IO

import Criterion.Main

import Database.FNLP

main = defaultMain [ bench "build" (nfIO $ build 10)
                   , bench "analize" (nfIO analize)]

build size = do hPutStrLn stderr 
                          "Performing Build Test (building \"bench.sqlite3\")"
                performBuild size "bench.sqlite3" "crubadan-data-small"

analize = do hPutStrLn stderr 
                       "Performing Analysis Test (output to \"AREPORT.txt\")"
             performAnalysis "bench.sqlite3" 
                             "crubadan-data-small" 
                             "AREPORT.txt"
