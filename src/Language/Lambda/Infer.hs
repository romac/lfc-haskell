
module Language.Lambda.Infer
  ( typeTree
  ) where

import Data.Functor.Foldable (Fix(..), unfix, cata)

import Language.Lambda.Annotate
import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Tree.Typed
import Language.Lambda.Tree.Untyped
import Language.Lambda.Ty

-- This is of course completely wrong, to be replaced by proper HM inference.
dumbTypeInf :: UntypedTree -> Ty
dumbTypeInf = cata dumbTypeInf'
  where
    dumbTypeInf' :: TreeF Ty -> Ty
    dumbTypeInf' (Var x)                   = tyVar (Name "a")
    dumbTypeInf' (Abs x bdy)               = tyFun x bdy
    dumbTypeInf' (App (Fix (TyFun a b)) x) = if a == x then b else error "a != x"

typeTree :: UntypedTree -> TypedTree
typeTree = annotate dumbTypeInf

