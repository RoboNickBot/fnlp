{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.FunLP.Common where

import qualified Data.Text as T
import qualified Data.Set as S

import Data.CharSet.Unicode.Block (Block(..), blocks)
import Data.CharSet (member)
import Data.Char (isSpace, isAlpha, toLower)

import Data.FunLP.Core


----------------------------------------------------------------------
-- CharSeq
----------------------------------------------------------------------

newtype CharSeq = CharSeq { charSeq :: [Char] }

class IsCharSeq x where
  mkCharSeq :: x -> CharSeq
  
instance IsCharSeq String where
  mkCharSeq = CharSeq
  
instance IsCharSeq T.Text where
  mkCharSeq = CharSeq . T.unpack


----------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------

data NGToken = WordStart | Letter Char | WordEnd
               deriving (Show, Read, Eq, Ord)

newtype OrderedTokenList = OrderedTokenList [NGToken]
                           deriving (Show, Read, Eq, Ord)

instance PState CharSeq OrderedTokenList PClosed
instance LinkedTo CharSeq OrderedTokenList where
  linkstep = OrderedTokenList . concat . smooth . charSeq

newtype TokenList = TokenList [NGToken]
                    deriving (Show, Read, Eq, Ord)

instance PState OrderedTokenList TokenList PClosed
instance LinkedTo OrderedTokenList TokenList where
  linkstep (OrderedTokenList ts) = TokenList ts

newtype TokenSet = TokenSet (S.Set NGToken)
                   deriving (Show, Read, Eq, Ord)

instance PState TokenList TokenSet PClosed
instance LinkedTo TokenList TokenSet where
  linkstep (TokenList ts) = TokenSet (S.fromList ts)

smooth :: [Char] -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c 
                         || isSpace c 
                         || c == '\''
                         || c == '-')

fromTok :: NGToken -> Char
fromTok WordStart = '<'
fromTok WordEnd = '>'
fromTok (Letter c) = c

toTok :: Char -> NGToken
toTok '<' = WordStart
toTok '>' = WordEnd
toTok c = Letter c

wordToTok :: String -> [NGToken]
wordToTok word = [WordStart] ++ (fmap Letter word) ++ [WordEnd]

----------------------------------------------------------------------
-- Unicode Blocks
----------------------------------------------------------------------

blocksUsed :: NGToken -> [String]
blocksUsed (Letter c) =
  (fmap blockName 
   . filter (\b -> member c (blockCharSet b))) (blocks)
blocksUsed _ = []

data UBlock = UBlock { ubname :: String } 
              deriving (Show, Read, Eq, Ord)

newtype UBlockList = UBlockList { uBlockList :: [UBlock] }

instance PState TokenList UBlockList PClosed
instance LinkedTo TokenList UBlockList where
  linkstep (TokenList ts) = 
    UBlockList 
    . fmap UBlock 
    . foldr (\s -> (++) (blocksUsed s)) [] $ ts


----------------------------------------------------------------------
-- TriGrams
----------------------------------------------------------------------

data TriGram = TriGram { tri1 :: NGToken
                       , tri2 :: NGToken
                       , tri3 :: NGToken }
               deriving (Show, Read, Eq, Ord)

newtype TriGramList = TriGramList { triGramList :: [TriGram] }

instance PState OrderedTokenList TriGramList PClosed
instance LinkedTo OrderedTokenList TriGramList where
  linkstep (OrderedTokenList ts) = TriGramList (r ts)
            where r (a:b:c:ts) = 
                    (TriGram a b c) 
                    : (if c == WordEnd
                           then r ts
                           else r (b:c:ts))
                  r _ = []
