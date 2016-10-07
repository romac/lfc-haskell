
module Data.Functor.Foldable.Ext
  ( cataM
  ) where

import Data.Functor.Foldable (Fix, unfix)

cataM :: (Applicative m, Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM f x = f =<< traverse (cataM f) (unfix x)

