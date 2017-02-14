
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Lambda.Tree
  ( TreeF(..)
  ) where

import           Protolude

import           Data.Deriving

import           Control.Comonad.Cofree (Cofree(..))

import           Language.Lambda.Name
import           Language.Lambda.Ty

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
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

$(deriveEq1   ''TreeF)
$(deriveShow1 ''TreeF)
$(deriveRead1 ''TreeF)

