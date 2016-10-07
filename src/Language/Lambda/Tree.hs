
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Lambda.Tree
  ( TreeF(..)
  ) where

import Language.Lambda.Name

data TreeF a
  = Var Name
  | Abs a a
  | App a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

