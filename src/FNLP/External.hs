{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FNLP.External
  ( 
  
    ID (..)
  , Meta (..)
  , External (..)
  , ReadOnly (..)

  ) where

import Pipes

type ID = String

type Meta d = (ID, d)

data External d = External { pour :: Producer (Meta d) IO ()
                           , fill :: Consumer (Meta d) IO ()
                           , dump :: IO ()
                           , toss :: IO () }

type ReadOnly d = Producer (Meta d) IO ()
