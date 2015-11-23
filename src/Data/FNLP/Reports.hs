{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FNLP.Reports 
  ( Report (add)
  , blank
  , combine
  
  ) where

import Data.Convertible (Convertible, convert)

----------------------------------------------------------------------

class (Monoid r, Convertible s r) => Report r s where
  add :: r -> s -> r

blank :: Monoid r => r
blank = mempty

combine :: Monoid r => r -> r -> r
combine = mappend
