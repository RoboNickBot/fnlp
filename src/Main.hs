module Main (main) where

import System.Environment
import Pipes

import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams
import Data.FNLP
import FNLP.Identify

main = do (command:args) <- getArgs
          if command == "build"
             then buildTest args
             else identifyTest args

buildTest [db,src] = 
  do conn <- connect db
     buildTrigramsTable conn src
     putStrLn "All Done!"
     
identifyTest [db,src] = 
  do st <- getContents
     conn <- connect db
     ls <- crubadanNames src
     t <- getTable conn trigrams
     s <- mkSelector t getLang
     let pipe = langPipe s ls
     rep <- someResults 100 pipe (mkCharSeq st)

     putStrLn "\n-------------"
     putStrLn "Report:"
     putStrLn "-------------"
     sequence_ (map print (getScores rep))
     -- runEffect (streamResults pipe (mkCharSeq st))
