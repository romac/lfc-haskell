{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Annotate
  ( annotate
  , annotateM
  , annotateM'
  ) where

import Protolude

import Data.Functor.Foldable (Base, Recursive, project, cata)
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad (extend)

annotate :: Recursive t
         => (t -> a)
         -> t
         -> Cofree (Base t) a
annotate alg t = alg t :< fmap (annotate alg) (project t)

annotateM :: (Recursive t, Monad m, Traversable (Base t))
          => (Base t (m a) -> m a)
          -> t
          -> m (Cofree (Base t) a)
annotateM f x = sequence (annotate (cata f) x)

annotateM' :: (Recursive t, Traversable (Base t), Monad m)
           => (Cofree (Base t) () -> m a)
           -> t
           -> m (Cofree (Base t) a)
annotateM' f x = sequence (extend f (annotate (const ()) x))

