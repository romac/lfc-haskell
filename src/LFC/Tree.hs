
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module LFC.Tree
  ( TreeF(..)
  ) where

import           Protolude

import           Data.Deriving

import           Control.Comonad.Cofree (Cofree(..))

import           LFC.Name
import           LFC.Ty

data TreeF a
  = Var !Name
  | Zero
  | Succ a
  | Pred a
  | IsZero a
  | Tru
  | Fals
  | If a a a
  | Abs !Name !Ty a
  | App a a
  | RecEmpty
  | RecExtend a (Name, a)
  | RecSelect a !Name
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Typeable)

$(deriveEq1     ''TreeF)
$(deriveOrd1    ''TreeF)
$(deriveShow1   ''TreeF)
$(deriveRead1   ''TreeF)
