{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | General interfaces to IO data resources

module FNLP.External
  ( 
  
    module Pipes.Share
    
  , Provides (provide)
  , Accepts (accept)

  , Provider (..)
  , Accepter (..)

  ) where

----------------------------------------------------------------------

import FNLP.Types

import Pipes
import Pipes.Share

----------------------------------------------------------------------

class Provides e d where
  provide :: e -> IO (Producer d IO ())

class Accepts e d where
  accept :: e -> IO (Consumer d IO ())

data Provider d = Provider (IO (Producer d IO ()))

instance Provides (Provider d) d where
  provide (Provider p) = p

data Accepter d = Accepter (IO (Consumer d IO ()))

instance Accepts (Accepter d) d where
  accept (Accepter a) = a
