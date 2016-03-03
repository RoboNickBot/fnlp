module Main (main) where

import FNLP
import FNLP.Common
import FNLP.Classes

import Pipes
import qualified Pipes.Prelude as P
import System.Exit

main :: IO ()
main = do samps <- sampsents "test-data/oldfs"
          tdb <- openDB "test-db.sqlite3" >>= clear >>= teach samps
          runEffect (crossCheck tdb >-> P.map show >-> P.stdoutLn)
          die "whatever"

teach :: Producer (Ann Corpus) IO () -> TrigramDB -> IO TrigramDB
teach p tdb = runEffect (p >-> learner tdb) >> refreshDB tdb

