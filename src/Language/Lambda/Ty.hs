
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Ty
  ( Ty
  , TyF(..)
  , tyVar
  , tyFun
  ) where

import Data.Functor.Classes  (Eq1(..))
import Data.Functor.Foldable (Fix(..))

import Language.Lambda.Name

data TyF a
  = TyVar Name
  | TyFun a a
  deriving (Eq, Show, Functor)

instance Eq1 TyF where
  liftEq _  (TyVar x)   (TyVar y)     = x == y
  liftEq eq (TyFun x y) (TyFun x' y') = eq x x' && eq y y'
  liftEq _  _           _             = False

type Ty = Fix TyF

tyVar :: Name -> Ty
tyVar = Fix . TyVar

tyFun :: Ty -> Ty -> Ty
tyFun a b = Fix (TyFun a b)

