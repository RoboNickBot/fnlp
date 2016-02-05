{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FNLP.Types
  ( Class
  , Classified
  , mkClass

  ) where

import Data.Text (Text)
import Data.Convertible

newtype Class = Class Text 
  deriving (Show, Read, Eq, Ord)

instance Convertible Text Class where
  safeConvert = Right . Class

instance Convertible Class Text where
  safeConvert (Class t) = Right t

instance Convertible Class String where
  safeConvert = convertVia (undefined::Text)

instance Convertible String Class where
  safeConvert = convertVia (undefined::Text)

mkClass :: Text -> Class
mkClass = convert

type Classified d = (Class, d)
