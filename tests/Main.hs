module Main (main) where

import FNLP
import FNLP.Common

import Pipes
import qualified Pipes.Prelude as P

main :: IO ()
main = showOldfs >> performExchange >> return ()

showOldfs :: IO ()
showOldfs = do putStrLn ">>> Doing Oldfs test"
               prv <- provide (getProvider "test-data/oldfs")
               runEffect ((prv :: Producer (Classified Corpus) IO ()) 
                           >-> P.map (fmap (features :: Corpus -> FreqList TriGram))
                           >-> P.map (fmap (\f -> head $ freqList f))
                           >-> P.map show 
                           >-> P.stdoutLn)

testAccepter :: IO (Consumer (Classified (FreqList TriGram)) IO ())
testAccepter = accept (openAccepter "testdb.sqlite3" "training")

performExchange :: IO ()
performExchange = do putStrLn ">>> Doing Exchange Test"
                     prv <- provide (getProvider "test-data/oldfs")
                     acc <- testAccepter
                     runEffect ((prv 
                                 >-> P.map (fmap (features :: Corpus -> FreqList TriGram)) 
                                 >-> acc))
                     check <- provide (openProvider "testdb.sqlite3" "training")
                     runEffect (((check :: Producer (Classified (FreqList TriGram)) IO ())
                                 >-> P.map (fmap (\f -> head $ freqList f)) 
                                 >-> P.map show >-> P.stdoutLn))
