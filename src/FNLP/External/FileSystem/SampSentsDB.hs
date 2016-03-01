{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FNLP.External.FileSystem.SampSentsDB 
  ( 
  
    sampsents

  ) where

import qualified Data.Text as T

import System.Directory (getDirectoryContents)
import System.FilePath
import System.IO (hPutStrLn, hFlush, stderr)
import qualified Data.Text.IO as TIO (readFile)

import Pipes
import qualified Pipes.Prelude as P


import Data.FNLP
import Data.FNLP.Common
import FNLP.Classes

----------------------------------------------------------------------

sampsents :: FilePath -> IO (Producer (Ann Corpus) IO ())
sampsents path = prod <$> fileList path
  where prod f = each f >-> readLangFiles >-> P.map (fmap corpus)

fileList :: FilePath -> IO [Ann FilePath]
fileList path = 
  fmap (\d -> (Tag (convert d), path </> d </> "SAMPSENTS")) 
     <$> getRealDirectoryContents path

readLangFiles :: Pipe (Ann FilePath) (Ann T.Text) IO ()
readLangFiles = P.mapM (mapM (\f -> deb f >> TIO.readFile f))
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...")

getRealDirectoryContents = 
  fmap (filter (\a -> (a /= ".") && (a /= ".."))) 
  <$> getDirectoryContents


