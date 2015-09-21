module FNLP.Identify
  ( Candidate (..)
  , Score (..)
  , Report (..)
  , IdReport (..)
  
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
allResults :: Monad m => Identifier m IdReport
allResults prd fr = compile (prd >-> scores fr)

-- | produces all Languages that score above the threshold
goodResults :: Monad m => Double -> Identifier m IdReport
goodResults t prd fr = compile (prd >-> scores fr >-> filt)
  where filt = P.filter (testScore t)

-- | produces the best n results
someResults :: Monad m => Int -> Identifier m IdReport
someResults n prd fr = compile' n (prd >-> scores fr)

results :: Monad m 
        => Producer Candidate m () 
        -> FreqList TriGram
        -> Producer (Score Language Double) m ()
results prd fr = prd >-> scores fr

streamResults :: Producer Candidate IO () -> FreqList TriGram -> Effect IO ()
streamResults prd fr = results prd fr >-> P.print

compile :: (Ord s, Monad m) => Producer (Score l s) m () -> m (Report l s)
compile = P.fold addToReport mempty id

compile' n = P.fold (addToBoundedReport n) mempty id

scores :: Monad m 
       => FreqList TriGram 
       -> Pipe Candidate (Score Language Double) m ()
scores fr = P.map (fmap (cosine fr))

trigs :: CharSeq -> FreqList TriGram
trigs = features


-- type Id' m r = Producer Candidate m () -> FreqList TriGram -> m r

-- testID :: Producer Candidate m () -> Candidate -> m (Maybe Language)
-- testID p c = undefined

-- identify' :: Monad m => Double -> Id' m r
-- identify' t p c = (fmap fst . listToMaybe . getScores) <$> results'
