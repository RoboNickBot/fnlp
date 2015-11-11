module Main (main) where

import System.Environment
import System.IO

import Criterion.Main

import Database.FNLP

main :: IO ()
main = prep >> defaultMain [ bench "build" (nfIO $ performBuild 10 
                                                                "bench-test.sqlite3" 
                                                                "test-sampsents")
                           , bench "analize10" (nfIO $ analize "10" "bench10.sqlite3")
                           , bench "analize30" (nfIO $ analize "30" "bench30.sqlite3")
                           , bench "analize150" (nfIO $ analize "150" "bench150.sqlite3")]

prep = build 10 >> build 30 >> build 150 >> return ()

build size = do let file = "bench" ++ show size ++ ".sqlite3"
                hPutStrLn stderr 
                          ("building " ++ file ++ " for tests ...")
                performBuild size file "test-sampsents"

analize size file = 
  do hPutStrLn stderr 
               ("Performing Analysis Test (output to \"AREPORT" ++ size ++ ".txt\")")
     performAnalysis file
                     "test-sampsents" 
                     ("AREPORT" ++ size ++ ".txt")
