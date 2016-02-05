module FNLP.External.FileSystem.SampSentsDB 
  ( getProvider
  
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
import Data.FNLP.Freq
import FNLP
import FNLP.External

getProvider :: FilePath -> Provider (Classified Corpus)
getProvider = outpipe

getRealDirectoryContents = 
  fmap (filter (\a -> (a /= ".") && (a /= ".."))) 
  <$> getDirectoryContents

oldFSFiles :: FilePath -> IO [(Class,FilePath)]
oldFSFiles r = fmap (\d -> (convert d, r </> d </> "SAMPSENTS")) 
                  <$> getRealDirectoryContents r

oldFSNames = getRealDirectoryContents

readFilesP :: [String] -> Producer T.Text IO ()
readFilesP (p:ps) = do lift (deb p)
                       text <- lift (TIO.readFile p)
                       yield text
                       readFilesP ps
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 
                >> hFlush stderr 
readFilesP _ = return ()

readLangFiles :: Pipe (Class, FilePath) (Class, T.Text) IO ()
readLangFiles = P.mapM (mapM (\f -> deb f >> TIO.readFile f))
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 

outpipe :: FilePath -> Provider (Classified Corpus)
outpipe p = 
  Provider (do files <- oldFSFiles p
               return (each files >-> readLangFiles >-> P.map (fmap corpus)))
