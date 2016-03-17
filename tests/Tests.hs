{-# LANGUAGE OverloadedStrings #-}

module Tests (main) where

import FNLP
import FNLP.Common
import FNLP.Classes

import Pipes
import qualified Pipes.Prelude as P
import System.Exit

main :: IO ()
main = do samps <- sampsents "test-data/oldfs"
          tdb <- trigramDB "test-db.sqlite3" >>= wipe >>= teach samps
          results <- P.toListM (crossCheck tdb)
          if results == expected
             then return ()
             else die $ "Unexpected results: " ++ show results
             
expected :: [(Ann (Maybe Tag))]
expected = [(Tag "en",    Just (Tag "en"))
           ,(Tag "ja",    Just (Tag "en"))
           ,(Tag "lorem", Just (Tag "lorem"))
           ,(Tag "ru",    Just (Tag "ru"))]

