
module Language.Lambda.Infer
  ( typeTree
  ) where

import Data.Functor.Foldable     (Fix(..), cata, unfix)
import Data.Functor.Foldable.Ext (cataM)

import Data.Functor.Identity

import Language.Lambda.Annotate
import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Tree.Typed
import Language.Lambda.Tree.Untyped
import Language.Lambda.Ty

-- This is of course completely wrong, to be replaced by proper HM inference.
dumbTypeInf :: UntypedTree -> Identity Ty
dumbTypeInf = cataM dumbTypeInf'
  where
    dumbTypeInf' :: TreeF Ty -> Identity Ty
    dumbTypeInf' (Var _)                   = return $ tyVar (Name "a")
    dumbTypeInf' (Abs x bdy)               = return $ tyFun x bdy
    dumbTypeInf' (App (Fix (TyFun a b)) x) = return $ if a == x then b else error "a != x"
    dumbTypeInf' (App (Fix (TyVar _)) _)   = error "cannot apply non-function"

typeTree :: UntypedTree -> Identity TypedTree
typeTree = annotateM dumbTypeInf

