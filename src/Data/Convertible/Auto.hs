{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies, TypeFamilies, ScopedTypeVariables #-}

module Data.Convertible.Auto
  (
  
    PState
  , PClosed
  , AutoLink
  , linkstep
  , features

  ) where


data POpen
data PClosed

class PState a b flag | a b -> flag

instance (flag ~ POpen) => PState a b flag

class Featuring' flag a b where
  features' :: flag -> a -> b 


class Featuring f g where
  features :: f -> g

class AutoLink f g | g -> f where
  linkstep :: f -> g


instance (PState a b flag, Featuring' flag a b) 
         => Featuring a b where
  features = features' (undefined :: flag)

-- TODO: Type sig for linkstep is probably unnecessary?
instance (AutoLink a b) => Featuring' PClosed a b where
  features' _ = (linkstep :: AutoLink a b => a -> b) 

instance (PState a b flag, AutoLink b c, Featuring' flag a b) 
         => Featuring' POpen a c where
  features' _ = (linkstep :: AutoLink b c => b -> c)
                . ((features' :: Featuring' flag a b => flag -> a -> b) 
                     (undefined :: flag))
