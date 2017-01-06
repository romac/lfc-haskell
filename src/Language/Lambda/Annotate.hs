{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Annotate
  ( annotate
  , annotateM
  ) where

import Data.Functor.Foldable (Base, Recursive, project, cata)
import Control.Comonad.Cofree (Cofree(..))

annotate :: Recursive t => (t -> a) -> t -> Cofree (Base t) a
annotate f x = f x :< fmap (annotate f) (project x)

-- FIXME: Rewrite annotateM without delegating to annotate
--        as the current version walks the whole structure twice per node
annotateM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t (m a) -> m a) -> t -> m (Cofree (Base t) a)
annotateM f x = sequence (annotate (cata f) x)

-- annotateM :: (Recursive t, Monad m, Traversable (Base t)) => (t -> m a) -> t -> m (Cofree (Base t) a)
-- annotateM f x = do
--   ann  <- f x
--   sub  <- traverse (annotateM f) (project x)
--   return (ann :< sub)

