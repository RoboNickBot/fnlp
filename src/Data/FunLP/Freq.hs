{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.FunLP.Freq 
  ( cosine
  , dot
  , len
  , mkFreqList
  , prettyprint
  , Frequency(..)
  , FreqList(..) 
  
  ) where

import qualified Data.Map as M
import qualified Data.List as L

import Data.FunLP.Core
import Data.FunLP.Common

newtype Frequency = Frequency { frequency :: Int } 
  deriving (Show, Read, Eq, Ord)

freqBump (Frequency f) = Frequency (f + 1)

freqInit = Frequency 1

data FreqList f = FreqList { freqMap :: M.Map f Frequency}
              deriving (Show, Read, Eq, Ord)

mkFreqList :: Ord f => [f] -> FreqList f
mkFreqList fs = 
  FreqList (foldr (\t m -> if M.member t m
                              then M.adjust freqBump t m
                              else M.insert t freqInit m)
                  M.empty fs)

instance PState TriGramList (FreqList TriGram) PClosed
instance LinkedTo TriGramList (FreqList TriGram) where
  linkstep = mkFreqList . triGramList

instance PState UBlockList (FreqList UBlock) PClosed
instance LinkedTo UBlockList (FreqList UBlock) where
  linkstep = mkFreqList . uBlockList

showln (sg,f) = sg ++ " " ++ show f

cosine :: Ord f => FreqList f -> FreqList f -> Double
cosine a b = dot a b / (len a * len b)

dot :: Ord f => FreqList f -> FreqList f -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (frequency $ l k) + p) 0 
           . fmap (onSnd frequency) . M.toList) (freqMap a)
  where l k = case M.lookup k (freqMap b) of
                Just v -> v
                _ -> (Frequency 0)

onSnd f (a,x) = (a,f x)

len :: FreqList f -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap frequency . fmap snd . M.toList . freqMap

prettyprint :: (Show a) => FreqList a -> [String]
prettyprint (FreqList m) = 
  let vals = M.toList m 
  in fmap show (L.sortBy 
                  (\a b -> compare (snd b) (snd a)) 
                  vals)
