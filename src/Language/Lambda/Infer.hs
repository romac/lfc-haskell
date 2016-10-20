
module Language.Lambda.Infer
  ( typeTree
  ) where

import Data.Functor.Foldable     (Fix(..), cata, unfix)
import Data.Functor.Foldable.Ext (cataM)

import Data.Functor.Identity

import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.State.Lazy

import Language.Lambda.Annotate
import Language.Lambda.Name
import Language.Lambda.Tree
-- import Language.Lambda.Tree.Typed
import Language.Lambda.Tree.Untyped
import Language.Lambda.Ty

type Context = [(Name, Ty)]

type Infer = State Context

type TypedTree = Cofree TreeF (Maybe Ty)

typeTree' :: TreeF (Maybe Ty) -> Infer (Maybe Ty)
typeTree' (Var n)           = gets (lookup n)
typeTree' Zero              = return (Just tyNat)
typeTree' (Succ (Just _))   = return (Just tyNat)
typeTree' (Pred (Just _))   = return (Just tyNat)
typeTree' (IsZero (Just _)) = return (Just tyBool)
typeTree' Tru               = return (Just tyBool)
typeTree' Fals              = return (Just tyBool)

typeTree' (If (Just _) (Just tyThen) (Just tyEls)) | tyThen == tyEls =
  return (Just tyThen)

typeTree :: UntypedTree -> Infer TypedTree
typeTree = annotateM (cataM typeTree')

