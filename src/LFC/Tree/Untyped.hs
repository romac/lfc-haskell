
module LFC.Tree.Untyped
  ( UntypedTree
  , mkVar
  , mkAbs
  , mkApp
  , mkTrue
  , mkFalse
  , mkSucc
  , mkPred
  , mkZero
  , mkIsZero
  , mkIf
  ) where

import Protolude

import Data.Functor.Foldable (Mu(..), embed)

import LFC.Name
import LFC.Tree
import LFC.Ty

type UntypedTree = Mu TreeF

mkVar :: Name -> UntypedTree
mkVar = embed . Var

mkZero :: UntypedTree
mkZero = embed Zero

mkTrue :: UntypedTree
mkTrue = embed Tru

mkFalse :: UntypedTree
mkFalse = embed Fals

mkSucc :: UntypedTree -> UntypedTree
mkSucc = embed . Succ

mkPred :: UntypedTree -> UntypedTree
mkPred = embed . Pred

mkIsZero :: UntypedTree -> UntypedTree
mkIsZero = embed . IsZero

mkAbs :: Name -> Ty -> UntypedTree -> UntypedTree
mkAbs x t e = embed (Abs x t e)

mkApp :: UntypedTree -> UntypedTree -> UntypedTree
mkApp f x = embed (App f x)

mkIf :: UntypedTree -> UntypedTree -> UntypedTree -> UntypedTree
mkIf c t e = embed (If c t e)

