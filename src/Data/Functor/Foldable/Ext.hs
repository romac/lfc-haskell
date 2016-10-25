{-# LANGUAGE FlexibleContexts #-}

module Data.Functor.Foldable.Ext
  ( cataM
  ) where

import Data.Functor.Foldable (Base, Recursive, project)

cataM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t a -> m a) -> t -> m a
cataM f x = f =<< traverse (cataM f) (project x)

