
module Language.Lambda.Infer
  ( typeTree
  ) where

import Data.Maybe (isJust, fromJust)

import Data.Functor.Foldable     (cata, project)
import Data.Functor.Foldable.Ext (cataM)

import Data.Functor.Identity

import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Reader

import Language.Lambda.Annotate
import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Tree.Typed
import Language.Lambda.Tree.Untyped
import Language.Lambda.Ty

type Context = [(Name, Ty)]

type Infer = ReaderT Context Maybe

typeTree' :: TreeF (Infer Ty) -> Infer Ty
typeTree' (Var n) = do
  x <- asks (lookup n)
  guard (isJust x)
  return (fromJust x)

typeTree' Zero =
  return tyNat

typeTree' (Succ x) = do
  ty <- x
  guard (ty == tyNat)
  return tyNat

typeTree' (Pred x) = do
  ty <- x
  guard (ty == tyNat)
  return tyNat

typeTree' (IsZero x) =
  return tyBool

typeTree' Tru =
  return tyBool

typeTree' Fals =
  return tyBool

typeTree' (If tyCond tyThen tyEls) = do
  a <- tyThen
  b <- tyThen
  guard (a == b)
  tyThen

typeTree' (App tyFun tyArg) = do
  TyFun a b <- project <$> tyFun
  c <- tyArg
  guard (a == c)
  return b

typeTree' (Abs x a tyBody) = do
  b <- local (\ctx -> (x, a) : ctx) tyBody
  return (tyFun a b)

typeTree :: UntypedTree -> Maybe TypedTree
typeTree tree = runReaderT (annotateM typeTree' tree) []

