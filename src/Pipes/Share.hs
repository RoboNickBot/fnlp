-- | Just some silly convenience functions for doing messy things with
-- a 'pipes' API

module Pipes.Share (share, mash, unPipe) where

import Pipes

----------------------------------------------------------------------

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
  let step x con = lift $ unPipe con x
  in (\(b,c) -> step b conB >> step c conC) . sp

-- | Turn a 'Consumer' back into a regular monadic function
unPipe :: Monad m => Consumer a m () -> a -> m ()
unPipe con a = runEffect (yield a >-> con)
