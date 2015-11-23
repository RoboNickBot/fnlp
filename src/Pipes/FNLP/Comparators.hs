{-| Pipe functions useful for classification operations -}

module Pipes.FNLP.Comparators
  (   
  -- * Comparator
    Comparator
  , report
  , reportWith
  
  -- * Streamed comparisons
  , CompareStream
  , scoreStream

  -- * Cross-stream comparison
  , Comparator2D
  , cross
  
  ) where

import Data.Convertible (Convertible, convert)
import Pipes
import qualified Pipes.Prelude as P

import Data.FNLP.Reports

----------------------------------------------------------------------

-- | A function that takes every item from a 'Producer' and compares 
-- them with a particular item of the same type, compiling the results
-- of the comparisons into a 'Report'
type Comparator c m r = c -> Producer c m () -> m r

-- | Creates a 'Comparator' function from the given scoring function
report :: (Monad m, Report r s) => (c -> c -> s) -> Comparator c m r
report = reportWith add blank

-- | Creates a 'Comparator' function that compiles its report in a
-- special way
reportWith :: Monad m 
           => (r -> s -> r) -- ^ how to add scores to the report
           -> r -- ^ the empty report with which to start
           -> (c -> c -> s) -- ^ the scoring function
           -> Comparator c m r
reportWith r i c = \a p -> P.fold r i id (p >-> P.map (c a))


-- | A function that performs comparisons but does not compile them
-- into a report, instead sending them downstream to be arbitrarily
-- manipulated or collected
type CompareStream c s m = c -> Producer c m () -> Producer s m ()

-- | Creates a 'CompareStream' function from the given scoring
-- function
scoreStream :: Monad m => (c -> c -> s) -> CompareStream c s m
scoreStream c = \a p -> p >-> P.map (c a)


-- | A function that takes two producers and compares all values from
-- one with all values from the other
type Comparator2D c m r = Producer c m () -> Producer c m () -> m r

-- | Takes the given 'Comparator' and promotes it to a 'Comparator2d'
--
-- The given 'Comparator' will be applied to every value from the
-- first 'Producer' passed to the 'Comparator2D', and the second
-- 'Producer' will be passed to each run of the 'Comparator'.
--
-- Practically, this means that the second 'Producer' will be
-- completely re-run as many times as there are elements produced by
-- the first 'Producer'.
cross :: (Monad m, Report t r)
      => Comparator c m r
      -> Comparator2D c m t
cross comp = \p1 p2 -> fld (p1 >-> P.mapM (flip comp $ p2))
  where fld = P.fold add blank id
