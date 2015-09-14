{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.FunLP.Freq ( cosine
                       , dot
                       , len
                       , mkFreqList
                       , prettyprint
                       , Frequency
                       , FreqList(..) ) where

import qualified Data.Map as M
import qualified Data.List as L

import Data.FunLP.General

type Frequency = Int

data F f => FreqList f = FreqList { freqMap :: M.Map f Frequency}
                     deriving (Show, Read, Eq, Ord)

instance F f => Feature (FreqList f)

mkFreqList :: (Show f, Read f, Eq f, Ord f) => [f] -> FreqList f
mkFreqList fs = 
  FreqList (foldr (\t m -> if M.member t m
                              then M.adjust (+1) t m
                              else M.insert t 1 m)
                  M.empty fs)

instance PState [TriGram] (FreqList TriGram) PClosed
instance LinkedTo [TriGram] (FreqList TriGram) where
  linkstep = mkFreqList
    
instance PState [UBlock] (FreqList UBlock) PClosed
instance LinkedTo [UBlock] (FreqList UBlock) where
  linkstep = mkFreqList

showln (sg,f) = sg ++ " " ++ show f

cosine :: F f => FreqList f -> FreqList f -> Double
cosine a b = dot a b / (len a * len b)

dot :: F f => FreqList f -> FreqList f -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (l k) + p) 0 
           . M.toList) (freqMap a)
  where l k = case M.lookup k (freqMap b) of
                Just v -> v
                _ -> 0

len :: F f => FreqList f -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap snd . M.toList . freqMap

prettyprint :: F a => FreqList a -> [String]
prettyprint (FreqList m) = 
  let vals = M.toList m 
  in fmap show (L.sortBy 
                  (\a b -> compare (snd b) (snd a)) 
                  vals)
