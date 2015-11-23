module FNLP.Identify
  ( Candidate (..)
  , Score (..)
  , ReportOld (..)
  , IdReportOld (..)
  
  -- * Identifiers
  , Identifier (..)
  , identify
  , testID
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
import qualified Data.FNLP.Reports as R
import Pipes.FNLP.Comparators
import Database.FNLP.SimpleDB
import Database.FNLP.TriGrams

----------------------------------------------------------------------

type Candidate = (Language, FreqList TriGram)

type Score l s = (l, s)

scoreSort :: Ord s => [Score l s] -> [Score l s]
scoreSort = L.sortBy (\(_,a) (_,b) -> compare b a)

testScore :: Ord s => s -> Score l s -> Bool
testScore t (l,s) = s >= t

newtype ReportOld l s = ReportOld { getScores :: [Score l s] }

instance Ord s => Monoid (ReportOld l s) where
  mappend (ReportOld as) (ReportOld bs) = ReportOld $ scoreSort (as ++ bs)
  mempty = ReportOld []

addToReportOld :: Ord s => ReportOld l s -> Score l s -> ReportOld l s
addToReportOld (ReportOld r) s = ReportOld (scoreSort (s:r))

addToBoundedReportOld ::Ord s => Int -> ReportOld l s -> Score l s -> ReportOld l s
addToBoundedReportOld i (ReportOld r) s = ReportOld (take i (scoreSort (s:r)))

type IdReportOld = ReportOld Language Double

type Identifier m r = Producer Candidate m () -> FreqList TriGram -> m r

testID :: Monad m 
       => Producer Candidate m () 
       -> Candidate 
       -> m (Language, Maybe Language)
testID p (l,fr) = (,) l <$> identify 0 p fr
  -- where ev l1 (Just l2) = if l1 == l2
  --                            then (True, l1, Just l2)
  --                            else (False, l1, Just l2)
  --       ev l1 Nothing = (False, l1, Nothing)

-- | produces the best, if any, Language that scores above the
--   threshold
identify :: Monad m => Double -> Identifier m (Maybe Language)
identify t prd fr = (fmap fst . listToMaybe . getScores)
                    <$> goodResults t prd fr

-- | produces all Language comparison scores
allResults :: Monad m => Identifier m IdReportOld
allResults prd fr = compile (prd >-> scores fr)

-- | produces all Languages that score above the threshold
goodResults :: Monad m => Double -> Identifier m IdReportOld
goodResults t prd fr = compile (prd >-> scores fr >-> filt)
  where filt = P.filter (testScore t)

-- | produces the best n results
someResults :: Monad m => Int -> Identifier m IdReportOld
someResults n prd fr = compile' n (prd >-> scores fr)

results :: Monad m 
        => Producer Candidate m () 
        -> FreqList TriGram
        -> Producer (Score Language Double) m ()
results prd fr = prd >-> scores fr

streamResults :: Producer Candidate IO () -> FreqList TriGram -> Effect IO ()
streamResults prd fr = results prd fr >-> P.print

compile :: (Ord s, Monad m) => Producer (Score l s) m () -> m (ReportOld l s)
compile = P.fold addToReportOld mempty id

compile' n = P.fold (addToBoundedReportOld n) mempty id

scores :: Monad m 
       => FreqList TriGram 
       -> Pipe Candidate (Score Language Double) m ()
scores fr = P.map (fmap (cosine fr))

trigs :: Corpus -> FreqList TriGram
trigs = features


-- type Id' m r = Producer Candidate m () -> FreqList TriGram -> m r

-- testID :: Producer Candidate m () -> Candidate -> m (Maybe Language)
-- testID p c = undefined

-- identify' :: Monad m => Double -> Id' m r
-- identify' t p c = (fmap fst . listToMaybe . getScores) <$> results'
