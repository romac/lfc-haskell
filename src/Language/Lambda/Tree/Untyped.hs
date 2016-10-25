
module Language.Lambda.Tree.Untyped
  ( UntypedTree
  , untypedVar
  , untypedAbs
  , untypedApp
  ) where

import Data.Functor.Foldable (Mu(..), embed)

import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Ty

type UntypedTree = Mu TreeF

untypedVar :: Name -> UntypedTree
untypedVar = embed . Var

untypedAbs :: Name -> Ty -> UntypedTree -> UntypedTree
untypedAbs x t e = embed (Abs x t e)

untypedApp :: UntypedTree -> UntypedTree -> UntypedTree
untypedApp f x = embed (App f x)

