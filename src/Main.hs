module Main (main) where

import System.Environment
import Pipes
import qualified Pipes.Prelude as P

import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams
import Data.FNLP
import FNLP.Identify

main = do (command:args) <- getArgs
          if command == "build"
             then buildTest args
             else if command == "identify"
                     then identifyTest args
                     else analysisTest args

buildTest [db,src] = 
  do conn <- connect db
     buildTrigramsTable conn 10 src
     putStrLn "All Done!"
     
identifyTest [db,src] = 
  do st <- getContents
     conn <- connect db
     ls <- crubadanNames src
     t <- getTable conn trigrams
     s <- mkSelector t getLang
     let pipe = langPipe' s "data" ls
     rep <- someResults 100 
                        pipe 
                        (features $ mkCharSeq st)

     putStrLn "\n-------------"
     putStrLn "Report:"
     putStrLn "-------------"
     sequence_ (map print (getScores rep))
     -- runEffect (streamResults pipe (mkCharSeq st))

analysisTest [db,src] = 
  do conn <- connect db
     ls <- crubadanNames src
     t <- getTable conn trigrams
     sData <- mkSelector t getLang
     sTest <- mkSelector t getLang
     let dataPipe = langPipe' sData "data" ls
         testPipe = langPipe' sTest "test" ls
         results = cross testID testPipe dataPipe 
                   >-> P.map mkOutput
                   >-> P.stdoutLn
     runEffect results

cross :: Monad m 
      => (Producer a m r -> a -> m b) 
      -> Producer a m r 
      -> Producer a m r
      -> Producer b m r
cross f pt pd = pt >-> P.mapM (f pd)

mkOutput :: (Bool, Language, Maybe Language) -> String
mkOutput (True,l1,l2)  = " [ ] " ++ l1 ++ ", " ++ show l2
mkOutput (False,l1,l2) = " [x] " ++ l1 ++ ", " ++ show l2



