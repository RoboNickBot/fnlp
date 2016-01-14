{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module FNLP.Report
  ( 
  
  -- * The 'Report' typeclass
    Report (add)
  , blank
  , combine

  -- * Common 'Report' implementors
  , LRep (..)
  , OLRep (..)
  , TRep (..)

  ) where

import Data.List (sort)

----------------------------------------------------------------------

class (Monoid r) => Report r v where
  add :: r -> v -> r

blank :: Monoid r => r
blank = mempty

combine :: Monoid r => r -> r -> r
combine = mappend

-- | A list-type report, which collects all results into an un-ordered
-- list
newtype LRep v = LRep { lrep :: [v] } 
  deriving (Show, Eq, Ord, Monoid)

instance Report (LRep v) v where
  add (LRep r) v = LRep $ v : r

-- | An ordered list-type report, which keeps results in order as they
-- are collected
newtype (Ord v) => OLRep v = OLRep { olrep :: [v] }
  deriving (Show, Eq, Ord)

instance (Ord v) => Monoid (OLRep v) where
  mempty = OLRep []
  mappend (OLRep a) (OLRep b) = OLRep $ sort (a ++ b)

instance (Ord v) => Report (OLRep v) v where
  add (OLRep r) v = OLRep $ sort (v : r)

-- | A report which holds only the maximum score (if any) received
newtype (Ord v) => TRep v = TRep (Maybe v) 
  deriving (Show, Eq, Ord)

instance (Ord v) => Monoid (TRep v) where
  mempty = TRep Nothing
  mappend = max

instance (Ord v) => Report (TRep v) v where
  add r v = r `mappend` (TRep (Just v))

