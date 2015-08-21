{-# LANGUAGE FlexibleContexts #-}

-- Importance of effect ordering
-- Example reproduced from http://book.realworldhaskell.org/read/monad-transformers.html
-- ghci> a
-- ghci> b
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

-- Different attempts at describing the same computational effects
type A = WriterT String Maybe
type B = MaybeT (Writer String)

-- The problem
problem :: MonadWriter String m => m ()
problem = do
  tell "this is where I fail"
  fail "oops"

-- Instantiations of 'problem'
a :: A ()
a = problem

b :: B ()
b = problem
