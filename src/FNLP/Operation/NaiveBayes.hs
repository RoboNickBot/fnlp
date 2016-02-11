{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FNLP.Operation.NaiveBayes 
  ( Bayes
  , classify
  
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Pipes
import qualified Pipes.Prelude as P

import FNLP
import Data.FNLP.Common

newtype Probability = Probability { _probability :: Double } 
  deriving (Show, Eq, Ord, Num)

data (Monad m) => Bayes f m = Bayes { _total :: m Int
                                    , _priors :: Pipe Class Probability m ()
                                    , _evidence :: Pipe f Probability m ()
                                    , _probabilities :: Pipe f (Map Class Probability) m ()}

classify :: (Ord f, Monad m) => Bayes f m -> [f] -> m [(Class, Probability)]
classify = undefined

data BayesDB f = BayesDB { numRec :: Int
                         , numClasses :: Map Class Int
                         , numFeats :: Map f Int
                         , numCombined :: Map f (Map Class Int)}
                         deriving (Show, Read, Eq, Ord)

emptyBayes :: BayesDB f
emptyBayes = BayesDB 0 M.empty M.empty M.empty

type Record f = (Class, [f])

teach :: (Ord f) => BayesDB f -> Record f -> BayesDB f
teach db r = BayesDB (upRec db r) (upClasses db r) (upFeats db r) (upCombined db r)

upRec :: BayesDB f -> Record f -> Int
upRec db _ = numRec db + 1

upClasses :: BayesDB f -> Record f -> Map Class Int
upClasses db r = inc (fst r) 1 (numClasses db)

upFeats :: (Ord f) => BayesDB f -> Record f -> Map f Int
upFeats db r = foldl (\m f -> inc f 1 m) (numFeats db) (snd r)

upCombined :: (Ord f) => BayesDB f -> Record f -> Map f (Map Class Int)
upCombined db r = let c = fst r
                  in foldl (\m f -> inc2 (f,c) 1 m) (numCombined db) (snd r)

probCombined :: (Ord f) => BayesDB f -> Map f (Map Class Double)
probCombined db = (M.fromList . fmap go . M.toList . numFeats) db
  where go (f,n) = let e = lookupM f (numCombined db)
                   in (f,fmap (\a -> fromIntegral a / fromIntegral n) e)

consult :: (Ord f) => BayesDB f -> f -> Map Class Double
consult db f = lookupM f (probCombined db)

lookup0 :: (Ord k, Num v) => Map k v -> k -> v
lookup0 m k = case M.lookup k m of
                Just v -> v
                _ -> 0

lookupM :: (Ord k, Monoid m) => k -> Map k m -> m
lookupM k m = case M.lookup k m of
                Just v -> v
                _ -> mempty

lookupE :: (Ord k, Ord l) => k -> Map k (Map l v) -> Map l v
lookupE k m = case M.lookup k m of
                Just v -> v
                _ -> mempty

inc :: (Ord k, Num v) => k -> v -> Map k v -> Map k v
inc k v m = M.insert k (lookup0 m k + v) m
              
inc2 :: (Ord k, Ord l, Num v) => (k,l) -> v -> Map k (Map l v) -> Map k (Map l v)
inc2 = undefined
