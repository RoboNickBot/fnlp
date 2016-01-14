{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FNLP.Common
 ( Corpus
 , corpus 
 
 , UBlock
 , ublock
 , UBlocks (uBlockList)

 , TriGram
 , trigram
 , TriGrams (triGramList)

 ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import Data.CharSet.Unicode.Block (Block(..), blocks)
import Data.CharSet (member)
import Data.Char (isAlpha, toLower)

import Data.FNLP

----------------------------------------------------------------------

newtype Corpus = Corpus Text 
  deriving (Show, Read, Eq, Ord)

corpus = Corpus

instance Convertible Corpus Text where
  safeConvert (Corpus text) = Right text

instance Convertible Text Corpus where
  safeConvert = Right . corpus


----------------------------------------------------------------------
-- Unicode Blocks
----------------------------------------------------------------------

blocksUsed :: Char -> [String]
blocksUsed c = 
  if isAlpha c
     then (fmap blockName 
           . filter (\b -> member c (blockCharSet b))) (blocks)
     else []

data UBlock = UBlock String deriving (Show, Read, Eq, Ord)

ublock = UBlock

newtype UBlocks = UBlocks { uBlockList :: [UBlock] }

instance PState Corpus UBlocks PClosed
instance AutoLink Corpus UBlocks where
  linkstep = UBlocks 
             . fmap UBlock 
             . foldr (\s -> (++) (blocksUsed s)) [] 
             . T.unpack 
             . convert


----------------------------------------------------------------------
-- TriGrams
----------------------------------------------------------------------

newtype TriGram = TriGram Text deriving (Show, Read, Eq, Ord)

trigram = TriGram

instance Convertible Text TriGram where
  safeConvert text = if T.length text == 3
                        then Right (TriGram text)
                        else convError "Not Three Characters" text

instance Convertible TriGram Text where
  safeConvert (TriGram text) = Right text

newtype TriGrams = TriGrams { triGramList :: [TriGram] }

instance PState Corpus TriGrams PClosed
instance AutoLink Corpus TriGrams where
  linkstep = TriGrams . concat . map trigrams . prepWords . convert

newtype PrepWord = PrepWord Text

trigrams :: PrepWord -> [TriGram]
trigrams (PrepWord w) = 
  if T.compareLength w 3 == LT
     then []
     else TriGram (T.take 3 w) : trigrams (PrepWord (T.drop 1 w))

prepWords :: Text -> [PrepWord]
prepWords = map PrepWord 
            . map ends 
            . map (T.map toLower) 
            . map (T.filter ok) 
            . T.words
  where ok :: Char -> Bool
        ok c = isAlpha c || c == '\'' || c == '-'
        
        ends :: Text -> Text
        ends w = T.cons '^' (T.snoc w '$')
