module Database.FNLP
  ( performBuild
  , performAnalysis
  , performIdentity
  
  , textReadFile

  ) where

import Pipes
import qualified Pipes.Prelude as P
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L

import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams
import Data.FNLP

import FNLP.Identify

textReadFile = TIO.readFile

performBuild :: Cardinality -> FilePath -> FilePath -> IO ()
performBuild size db src = 
  do conn <- connect db
     buildTrigramsTable conn size 10 src
     disconnect conn
     hPutStrLn stderr "All Done!"

performIdentity :: FilePath -> T.Text -> IO String
performIdentity db text = 
  do conn <- connect db
     t <- getTable conn trigrams

     chunkIDs <- mkSelector t listChunks >>= \s -> select s ()
     sData <- mkSelector t (chunks 50 "data")
     let langs = spoutCat sData chunkIDs
     report <- someResults 20 langs (features $ corpus text)
          
     return (prettyr report)

prettyr report = 
  "-------------\n\
  \Report:\n\
  \-------------\n"
  ++ concat (L.intersperse "\n" (map show (getScores report)))
  ++ "\n"

performAnalysis :: FilePath -> FilePath -> FilePath -> IO ()
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
