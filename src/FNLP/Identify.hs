module FNLP.Identify
  ( Candidate (..)
  , Score (..)
  , Report (..)
  , IdReport (..)
  
  -- * Identifiers
  , Identifier (..)
  , identify
  , allResults
  , goodResults
  , someResults
  , streamResults ) where

import Pipes
import qualified Pipes.Prelude as P
import qualified Data.List as L
import Data.Foldable (fold)
import Data.Maybe (listToMaybe)

import Data.FNLP
import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams

----------------------------------------------------------------------

type Candidate = (Language, FreqList TriGram)

type Score l s = (l, s)

scoreSort :: Ord s => [Score l s] -> [Score l s]
scoreSort = L.sortBy (\(_,a) (_,b) -> compare b a)

testScore :: Ord s => s -> Score l s -> Bool
testScore t (l,s) = s >= t

newtype Report l s = Report { getScores :: [Score l s] }

instance Ord s => Monoid (Report l s) where
  mappend (Report as) (Report bs) = Report $ scoreSort (as ++ bs)
  mempty = Report []

addToReport :: Ord s => Report l s -> Score l s -> Report l s
addToReport (Report r) s = Report (scoreSort (s:r))

addToBoundedReport ::Ord s => Int -> Report l s -> Score l s -> Report l s
addToBoundedReport i (Report r) s = Report (take i (scoreSort (s:r)))

type IdReport = Report Language Double

type Identifier m r = Producer Candidate m () -> CharSeq -> m r

-- | produces the best, if any, Language that scores above the
--   threshold
identify :: Monad m => Double -> Identifier m (Maybe Language)
identify t prd cs = (fmap fst . listToMaybe . getScores)
                    <$> goodResults t prd cs

-- | produces all Language comparison scores
allResults :: Monad m => Identifier m IdReport
allResults prd cs = compile (prd >-> scores cs)

-- | produces all Languages that score above the threshold
goodResults :: Monad m => Double -> Identifier m IdReport
goodResults t prd cs = compile (prd >-> scores cs >-> filt)
  where filt = P.filter (testScore t)

-- | produces the best n results
someResults :: Monad m => Int -> Identifier m IdReport
someResults n prd cs = compile' n (prd >-> scores cs)

results :: Monad m 
        => Producer Candidate m () 
        -> CharSeq 
        -> Producer (Score Language Double) m ()
results prd cs = prd >-> scores cs

streamResults :: Producer Candidate IO () -> CharSeq -> Effect IO ()
streamResults prd cs = results prd cs >-> P.print

compile :: (Ord s, Monad m) => Producer (Score l s) m () -> m (Report l s)
compile = P.fold addToReport mempty id

compile' n = P.fold (addToBoundedReport n) mempty id

scores cs = P.map (getScore cs)

getScore :: CharSeq -> Candidate -> Score Language Double
getScore t = fmap (cosine (trigs t))

trigs :: CharSeq -> FreqList TriGram
trigs = features


