{-# LANGUAGE FlexibleInstances #-}

{-| Functions for comparing things together -}

module Pipes.FNLP.Comparator 
  (
  
    Comparator (..)
  , comparator

  , Comparison (..)

  , Collector
  , collector
  
  , ExtraOps
  , Op (..)
  , extraOps
  , preOp
  , preFilter
  , postOp
  , postFilter

  ) where

import Pipes
import qualified Pipes.Prelude as P

----------------------------------------------------------------------

-- | A function that takes every item from a 'Producer' and compares 
-- them with a particular item of the same type, compiling the results
-- of the comparisons into some report type
type Comparator c m r = Producer c m () -> c -> m r

-- a Comparator is made up of: COMPARE method, FILTERS/OPs, COLLECT
-- method... these should all be interchangeable

-- TODO: Alternate idea: split Comparator into [COMPARE + PRE-FILTER
-- (comparator)] and [POST-FILTER + COLLECT (collector)].  These would
-- be more sensibly interchangeable I think.  Together they could be
-- called an Experiment or Classifier or something?

-- current thinking:
--
-- producer :: Producer c m ()
-- comparator :: c -> Pipe c s m ()
-- collector :: Producer s m () -> m r
--
-- wholeFun c = collector $ producer >-> comparator c
--
-- maybe there can be a convenient (Comparison -> Filter ->
-- Comparator) function for that common case, but there's no sense in
-- distancing the end result from a simple Pipe, right?
--
-- then again maybe?  I just think it would be useful to take the
-- whole, combined function as a black box and still extend it with
-- pre and post filters---those should be much more convenient to
-- tweak than, say, the central comparison function?
--
-- The pre and post filters should still be simple (c -> Pipe)s, but
-- they should be applyable easily to the whole, which means the whole
-- should be more of a lens than a function
--
-- An additional "filter" could be a transformation function applied
-- to each item (including the reference item).  This would be a
-- simple function, not a pipe

-- | build a 'Comparator' function
comparator :: Monad m 
           => Comparison c m s -- ^ The core comparison by which items
                               -- are scored
           -> ExtraOps c m s -- ^ Filters and other actions applied to
                             -- items and scores before and after the
                             -- core comparison
           -> Collector s m r -- ^ The method by which scores are
                              -- compiled into the resulting report
           -> Comparator c m r
comparator comp ext coll = \p c -> coll (p >-> mkPipe comp ext c)


type Comparison c m s = c -> c -> m s


type Collector s m r = Producer s m () -> m r

collector :: Monad m => (r -> s -> r) -> r -> Collector s m r
collector a i = P.fold a i id


data ExtraOps c m s = ExtraOps { preOps  :: Op c m c
                               , postOps :: Op c m s }

instance Monad m => Monoid (ExtraOps c m s) where
  mempty = ExtraOps idOp idOp
  mappend (ExtraOps pre1 post1) (ExtraOps pre2 post2) = 
    ExtraOps ((>->) <$> pre1  <*> pre2) 
             ((>->) <$> post2 <*> post1)

mkPipe :: Monad m => Comparison c m s -> ExtraOps c m s -> c -> Pipe c s m ()
mkPipe comp (ExtraOps pre post) c = pre c >-> P.mapM (comp c) >-> post c

type Op c m t = c -> Pipe t t m ()

extraOps :: Monad m => Op c m c -> Op c m s -> ExtraOps c m s
extraOps = ExtraOps

idOp :: Monad m => Op c m t
idOp = const cat

preOp :: Monad m => Op c m c -> ExtraOps c m s
preOp pf = ExtraOps pf idOp

preFilter :: Monad m => (c -> c -> Bool) -> ExtraOps c m s
preFilter pred = preOp (\c -> P.filter (pred c))

postOp :: Monad m => Op c m s -> ExtraOps c m s
postOp p = ExtraOps idOp p

postFilter :: Monad m => (c -> s -> Bool) -> ExtraOps c m s
postFilter pred = postOp (\c -> P.filter (pred c))

minScore :: (Monad m, Ord s) => s -> ExtraOps c m s
minScore s = postFilter (const (>= s))

