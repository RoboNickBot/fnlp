{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FNLP.Classes where

----------------------------------------------------------------------

import Pipes
import Data.Text (Text)



import Data.Convertible
import Data.Convertible.Auto

import Data.FNLP.Common
import Data.FNLP.Freq

----------------------------------------------------------------------

type Annotated a = (Tag, a)

type Ann a = Annotated a

data Tag = Tag Text deriving (Show, Read, Eq, Ord)

instance Convertible Tag Text where
  safeConvert (Tag t) = Right t
  
instance Convertible Text Tag where
  safeConvert = Right . Tag

class (Monad m) => Resource r q a m where
  withdraw :: r -> q -> m (Producer a m ())
  stream :: r -> m (Producer a m ())

class (Monad m) => Depot' r a m where
  deposit :: r -> Consumer a m ()

class Report s where
  best :: s -> Maybe Tag

class (Monad m, Report s) => Classifier c a m s where
  classify' :: (Monad m) => c -> a -> m s

class (Monad m) => Learner c a m where
  teach' :: (Monad m) => c -> Consumer (Ann a) m ()
  -- clear :: c -> m ()

data Report1 = Report1

instance Report Report1 where
  best = undefined

instance (Resource r () (FreqList a) m) => Classifier r (FreqList a) m Report1 where
  classify' = undefined

type Classifier' a r m = Pipe a r m ()
