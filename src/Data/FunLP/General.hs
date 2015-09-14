{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies, TypeFamilies, OverlappingInstances, ScopedTypeVariables #-}

module Data.FunLP.General ( NGToken(..)
                          , fromTok
                          , toTok
                          , wordToTok
                          , F
                          , PState(..)
                          , PClosed(..)
                          , POpen(..)
                          , Feature(..)
                          , LinkedTo(..)
                          , Featuring(..) 
                          , TriGram(..)
                          , UBlock(..) ) where

import qualified Data.Text as T
import qualified Data.Set as S

import Data.CharSet.Unicode.Block (Block(..), blocks)
import Data.CharSet (member)
import Data.Char (isSpace, isAlpha, toLower)

class (Show f, Read f, Eq f, Ord f) => F f

instance (Show f, Read f, Eq f, Ord f) => F f

class F f => Feature f

class (Show x, Read x, Eq x, Ord x) => CharSeq x where
  charSeq :: x -> [Char]

instance Feature String

instance CharSeq T.Text where
  charSeq = T.unpack

instance CharSeq [Char] where
  charSeq = id

class (Feature f, Feature g) => LinkedTo f g | g -> f where
  linkstep :: f -> g

class (Feature f, Feature g) => Featuring f g where
  features :: f -> g

instance (PState a b flag, Featuring' flag a b) 
         => Featuring a b where
  features = features' (undefined :: flag)

class (Feature a, Feature b) => Featuring' flag a b where
  features' :: flag -> a -> b 
  
instance (LinkedTo a b) => Featuring' PClosed a b where
  features' _ = (linkstep :: LinkedTo a b => a -> b)

instance (PState a b flag, LinkedTo b c, Featuring' flag a b) 
         => Featuring' POpen a c where
  features' _ = (linkstep :: LinkedTo b c => b -> c)
                . ((features' :: Featuring' flag a b => flag -> a -> b) 
                     (undefined :: flag))

class (Feature a, Feature b) => PState a b flag | a b -> flag 

data POpen
data PClosed

instance (Feature a, Feature b, flag ~ POpen) => PState a b flag
  
data NGToken = WordStart | Letter Char | WordEnd
               deriving (Show, Read, Eq, Ord)

newtype OrderedTokenList = OrderedTokenList [NGToken]
                           deriving (Show, Read, Eq, Ord)

instance Feature OrderedTokenList
        
instance PState String OrderedTokenList PClosed
instance LinkedTo String OrderedTokenList where
  linkstep = OrderedTokenList . concat . smooth

newtype TokenList = TokenList [NGToken]
                    deriving (Show, Read, Eq, Ord)

instance Feature TokenList

instance PState OrderedTokenList TokenList PClosed
instance LinkedTo OrderedTokenList TokenList where
  linkstep (OrderedTokenList ts) = TokenList ts

newtype TokenSet = TokenSet (S.Set NGToken)
                   deriving (Show, Read, Eq, Ord)

instance Feature TokenSet

instance PState TokenList TokenSet PClosed
instance LinkedTo TokenList TokenSet where
  linkstep (TokenList ts) = TokenSet (S.fromList ts)

blocksUsed :: NGToken -> [String]
blocksUsed (Letter c) =
  (fmap blockName 
   . filter (\b -> member c (blockCharSet b))) (blocks)
blocksUsed _ = []

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

data TriGram = TriGram { tri1 :: NGToken
                       , tri2 :: NGToken
                       , tri3 :: NGToken }
               deriving (Show, Read, Eq, Ord)

instance Feature [TriGram]

instance PState OrderedTokenList [TriGram] PClosed
instance LinkedTo OrderedTokenList [TriGram] where
  linkstep (OrderedTokenList ts) = r ts
            where r (a:b:c:ts) = 
                    (TriGram a b c) 
                    : (if c == WordEnd
                           then r ts
                           else r (b:c:ts))
                  r _ = []

data UBlock = UBlock { ubname :: String } 
              deriving (Show, Read, Eq, Ord)

instance Feature [UBlock]

instance PState TokenList [UBlock] PClosed
instance LinkedTo TokenList [UBlock] where
  linkstep (TokenList ts) = 
    fmap UBlock . foldr (\s -> (++) (blocksUsed s)) [] $ ts

triGrams :: [NGToken] -> [TriGram]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []

smooth :: [Char] -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c 
                         || isSpace c 
                         || c == '\''
                         || c == '-')
