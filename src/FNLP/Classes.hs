{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FNLP.Classes
  ( Ann
  , Tag (..)
  , External (..)
  , wipe
  , Learner (..)
  , teach

  ) where

----------------------------------------------------------------------

import Pipes
import Data.Text (Text)

import Data.Convertible
import Data.Convertible.Auto

----------------------------------------------------------------------

type Annotated a = (Tag, a)

type Ann a = Annotated a

data Tag = Tag Text deriving (Show, Read, Eq, Ord)

instance Convertible Tag Text where
  safeConvert (Tag t) = Right t
  
instance Convertible Text Tag where
  safeConvert = Right . Tag

class Monad m => External e m where
  wipeAction :: e -> m ()
  refresh :: e -> m e

wipe :: External l m => l -> m l
wipe db = wipeAction db >> refresh db

class External l m => Learner l a m where
  learner :: l -> Consumer a m ()

teach :: (Monad m, Learner d a m) => Producer a m () -> d -> m d
teach p tdb = runEffect (p >-> learner tdb) >> refresh tdb
