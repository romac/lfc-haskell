{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Functor.Foldable.Ext
  ( cataM
  ) where

import Data.Functor.Foldable (Base, Recursive, Corecursive, project, embed, ana)
import Control.Monad (liftM, join, (<=<))
import Control.Monad.Free (Free(..))

cataM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t a -> m a) -> t -> m a
cataM alg t = alg =<< traverse (cataM alg) (project t)

anaM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t a)) -> a -> m t
anaM coalg = go
  where go a = fmap embed (coalg a >>= mapM go)

futuM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t (Free (Base t) a))) -> a -> m t
futuM coalg = anaM go . Pure
  where
    go (Pure a)  = coalg a
    go (Free fa) = return fa

