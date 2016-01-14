module Split (share, mash) where

import Pipes

-- have I done it??

-- | Divide the products of a 'Producer' over two 'Consumer's
share :: Monad m 
      => (a -> (b,c)) 
      -> Producer a m ()
      -> Consumer b m () 
      -> Consumer c m () 
      -> Effect m ()
share sp proA conB conC = for proA (mash sp conB conC)

-- | Turn two 'Consumer's into a single loop body
mash :: Monad m 
     => (a -> (b, c)) 
     -> Consumer b m () 
     -> Consumer c m () 
     -> (a -> Effect m ())
mash sp conB conC = 
  let step x con = lift $ runEffect (yield x >-> con)
  in (\(b,c) -> step b conB >> step c conC) . sp
