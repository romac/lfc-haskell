{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Annotate
  ( annotate
  , annotateM
  ) where

import Data.Functor.Foldable (Base, Recursive, project, cata)
import Control.Comonad.Cofree (Cofree(..))

annotate :: (Recursive t, Functor (Base t)) => (t -> a) -> t -> Cofree (Base t) a
annotate f x = f x :< fmap (annotate f) (project x)

-- annotateM :: (Recursive t, Monad m, Functor (Base t), Traversable (Base t)) => (t -> m a) -> t -> m (Cofree (Base t) a)
-- annotateM f x = do
--   ann  <- f x
--   sub  <- traverse (annotateM f) (project x)
--   return (ann :< sub)

annotateM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t (m a) -> m a) -> t -> m (Cofree (Base t) a)
annotateM f x = sequence (annotate (cata f) x)

