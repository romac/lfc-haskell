
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Ty
  ( Ty
  , TyF(..)
  , tyBool
  , tyNat
  , tyFun
  ) where

import Data.Functor.Classes  (Eq1(..))
import Data.Functor.Foldable (Fix(..))

import Language.Lambda.Name

data TyF a
  = TyBool
  | TyNat
  | TyFun a a
  deriving (Eq, Show, Functor)

instance Eq1 TyF where
  liftEq _  TyBool TyBool             = True
  liftEq _  TyNat  TyNat              = True
  liftEq eq (TyFun x y) (TyFun x' y') = eq x x' && eq y y'
  liftEq _  _           _             = False

type Ty = Fix TyF

tyBool :: Ty
tyBool = Fix TyBool

tyNat :: Ty
tyNat = Fix TyNat

tyFun :: Ty -> Ty -> Ty
tyFun a b = Fix (TyFun a b)

