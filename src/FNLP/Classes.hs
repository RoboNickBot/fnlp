{-# LANGUAGE MultiParamTypeClasses #-}

module FNLP.Classes where

----------------------------------------------------------------------

import Pipes
import Data.Text (Text)

import Data.Convertible.Auto

----------------------------------------------------------------------

type Annotated a = ([Tag], a)

type Ann a = Annotated a

data Tag = Tag Text

class (Monad m) => Resource r q a m where
  withdraw :: r -> q -> Producer a m ()

class (Monad m) => Depot r a m where
  deposit :: r -> Consumer a m ()

class Report s where
  best :: s -> Maybe Tag

class (Monad m, Report s) => Classifier c a m s where
  classify :: (Monad m, Featuring b a) => c -> b -> m s

class (Monad m) => Learner c a m where
  teach :: (Monad m, Featuring b a) => c -> Consumer (Ann b) m ()
  clear :: c -> m ()
