module Database.FNLP
  ( performBuild
  , performAnalysis
  , performIdentity
  
  ) where

import Pipes
import qualified Pipes.Prelude as P
import System.IO
import qualified Data.Text.IO as TIO
import qualified Data.List as L

import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams (buildTrigramsTable, trigrams, Language, listChunks, chunks)
import Data.FNLP

import FNLP.Identify

performBuild size db src = 
  do conn <- connect db
     buildTrigramsTable conn size 10 src
     disconnect conn
     hPutStrLn stderr "All Done!"

-- performIdentity needs to be updated to use the chunk pipe

performIdentity = undefined

-- performIdentity db src = 
--   do st <- TIO.getContents
--      conn <- connect db
--      ls <- crubadanNames src
--      t <- getTable conn trigrams
--      s <- mkSelector t getLang
--      let pipe = langPipe' s "data" ls
--      rep <- someResults 100 
--                         pipe 
--                         (features $ corpus st)

--      putStrLn "\n-------------"
--      putStrLn "Report:"
--      putStrLn "-------------"
--      sequence_ (map print (getScores rep))
     
performAnalysis db src out = 
  do conn <- connect db
     t <- getTable conn trigrams
     chunkIDs <- mkSelector t listChunks >>= \s -> select s ()
     print chunkIDs
     sData <- mkSelector t (chunks 50 "data")
     sTest <- mkSelector t (chunks 50 "test")
     let dataPipe = spoutCat sData chunkIDs
         testPipe = spoutCat sTest chunkIDs
         results = cross testID testPipe dataPipe
     outStr <- show <$> compileAReport results
     writeFile out outStr

cross :: Monad m 
      => (Producer a m r -> a -> m b) 
      -> Producer a m r 
      -> Producer a m r
      -> Producer b m r
cross f pt pd = pt >-> P.mapM (f pd)

type AScore = (Language, Maybe Language)

correct :: AScore -> Bool
correct (l1, Just l2) = l1 == l2
correct _ = False

scoreLine :: AScore -> String
scoreLine s = let c = if correct s
                         then " "
                         else "x"
              in " [" ++ c ++ "] " ++ fst s 
                 ++ " -> " ++ show (snd s)

newtype AReport = AReport [AScore]

instance Show AReport where
  show (AReport ss) = 
    let total = length ss
        corr = length (filter correct ss)
        per = show (fromIntegral corr * 100 
                    / fromIntegral total) ++ "%"
    in "--REPORT -------------\n\
       \\n\
       \Correctness: " 
       ++ per ++ " ( " 
       ++ show corr ++ " / " 
       ++ show total ++ " )\n"
       ++ (concat 
           . L.intersperse "\n" 
           . map scoreLine) ss
           
addToAReport (AReport ss) s = AReport (s:ss)

emptyAReport = AReport []

compileAReport :: Monad m => Producer AScore m () -> m AReport
compileAReport = P.fold addToAReport emptyAReport id
