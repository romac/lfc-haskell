
module Language.Lambda.Tree.Untyped
  ( UntypedTree
  , untypedVar
  , untypedAbs
  , untypedApp
  ) where

import Data.Functor.Foldable (Fix(..))

import Language.Lambda.Name
import Language.Lambda.Tree

type UntypedTree = Fix TreeF

untypedVar :: Name -> UntypedTree
untypedVar = Fix . Var

untypedAbs :: UntypedTree -> UntypedTree -> UntypedTree
untypedAbs x e = Fix (Abs x e)

untypedApp :: UntypedTree -> UntypedTree -> UntypedTree
untypedApp f x = Fix (App f x)

