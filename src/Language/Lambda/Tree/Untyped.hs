
module Language.Lambda.Tree.Untyped
  ( UntypedTree
  , untypedVar
  , untypedAbs
  , untypedApp
  ) where

import Data.Functor.Foldable (Fix(..))

import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Ty

type UntypedTree = Fix TreeF

untypedVar :: Name -> UntypedTree
untypedVar = Fix . Var

untypedAbs :: Name -> Ty -> UntypedTree -> UntypedTree
untypedAbs x t e = Fix (Abs x t e)

untypedApp :: UntypedTree -> UntypedTree -> UntypedTree
untypedApp f x = Fix (App f x)

