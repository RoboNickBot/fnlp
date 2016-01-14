module FNLP.External.OldFS (getInterface) where

import qualified Data.Text as T

import System.Directory (getDirectoryContents)
import System.FilePath
import System.IO (hPutStrLn, hFlush, stderr)
import qualified Data.Text.IO as TIO (readFile)

import Pipes
import qualified Pipes.Prelude as P

import Data.FNLP
import FNLP.External

getInterface :: FilePath -> IO (ReadOnly Corpus)
getInterface = outpipe


getRealDirectoryContents = 
  fmap (filter (\a -> (a /= ".") && (a /= ".."))) 
  <$> getDirectoryContents

oldFSFiles :: FilePath -> IO [(ID,FilePath)]
oldFSFiles r = fmap (\d -> (d, r </> d </> "SAMPSENTS")) 
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

readLangFiles :: Pipe (ID, FilePath) (ID, T.Text) IO ()
readLangFiles = P.mapM (mapM (\f -> deb f >> TIO.readFile f))
  where deb p = hPutStrLn stderr ("reading file" ++ p ++ " ...") 

outpipe :: FilePath -> IO (Producer (Meta Corpus) IO ())
outpipe p = do files <- oldFSFiles p
               return (each files >-> readLangFiles >-> P.map (fmap corpus))
