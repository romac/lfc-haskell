
module Language.Lambda.Annotate
  ( annotate
  , annotateM
  , annotateA
  ) where

import Data.Functor.Foldable (Fix, unfix, cata)
import Control.Comonad.Cofree (Cofree(..))

annotate :: Functor f => (Fix f -> a) -> Fix f -> Cofree f a
annotate f x = f x :< fmap (annotate f) (unfix x)

annotateM :: (Monad m, Functor f, Traversable f) => (Fix f -> m a) -> Fix f -> m (Cofree f a)
annotateM f x = do
  ann  <- f x
  sub  <- traverse (annotateM f) (unfix x)
  return (ann :< sub)

annotateA :: (Traversable f, Monad m) => (f (m a) -> m a) -> Fix f-> m (Cofree f a)
annotateA f x = sequence (annotate (cata f) x)

