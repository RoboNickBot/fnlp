{-# LANGUAGE MultiParamTypeClasses #-}

-- | General interfaces to IO data resources

module FNLP.External
  ( 
  
    module Pipes.Share

  , ID (..)
  , Meta (..)
  , External (..)
  , ReadOnly (..)

  ) where

import Pipes
import Pipes.Share

type ID = String

type Meta d = (ID, d)

class Provides e d where
  provide :: e -> IO (Producer (Meta d) IO ())

class Accepts e d where
  accept :: e -> IO (Consumer (Meta d) IO ())

data External d = External { pour :: Producer (Meta d) IO ()
                           , fill :: Consumer (Meta d) IO ()
                           , dump :: IO ()
                           , toss :: IO () }

type ReadOnly d = Producer (Meta d) IO ()
