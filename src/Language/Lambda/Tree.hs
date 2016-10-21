
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Tree
  ( TreeF(..)
  ) where

import Language.Lambda.Name
import Language.Lambda.Ty

data TreeF a
  = Var Name
  | Zero
  | Succ a
  | Pred a
  | IsZero a
  | Tru
  | Fals
  | If a a a
  | Abs Name Ty a
  | App a a
  deriving (Eq, Functor, Foldable, Traversable)

-- deriving instance Show a => Show (TreeF a)
