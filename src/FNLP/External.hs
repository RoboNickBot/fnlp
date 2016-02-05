{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | General interfaces to IO data resources

module FNLP.External
  ( 
  
    module Pipes.Share
     
  , Accepts (accept)
  , Provides (provide)
  
  , Accepter (..)
  , Provider (..)
  , Depot (..)

  ) where

----------------------------------------------------------------------

import FNLP.Types

import Pipes
import Pipes.Share

----------------------------------------------------------------------

class Accepts e d where
  accept :: e -> IO (Consumer d IO ())

class Provides e d where
  provide :: e -> IO (Producer d IO ())


data Accepter d = Accepter (IO (Consumer d IO ()))

instance Accepts (Accepter d) d where
  accept (Accepter a) = a


data Provider d = Provider (IO (Producer d IO ()))

instance Provides (Provider d) d where
  provide (Provider p) = p


data Depot d = Depot (Accepter d) (Provider d)

instance Accepts (Depot d) d where
  accept (Depot a _) = accept a
  
instance Provides (Depot d) d where
  provide (Depot _ p) = provide p
