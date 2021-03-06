
module LFC.Tree.Typed
  ( TypedTree
  , typedVar
  , typedAbs
  , typedApp
  , getType
  ) where

import Control.Comonad.Cofree (Cofree(..))

import LFC.Name
import LFC.Tree
import LFC.Ty

type TypedTree = Cofree TreeF Ty

typedVar :: Ty -> Name -> TypedTree
typedVar ty name = ty :< Var name

typedAbs :: Ty -> Name -> Ty -> TypedTree -> TypedTree
typedAbs ty x t e = ty :< Abs x t e

typedApp :: Ty -> TypedTree -> TypedTree -> TypedTree
typedApp ty f x = ty :< App f x

getType :: TypedTree -> Ty
getType (ty :< _) = ty

