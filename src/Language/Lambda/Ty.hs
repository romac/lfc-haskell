
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
import Data.Functor.Foldable (Mu(..), embed)

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

type Ty = Mu TyF

tyBool :: Ty
tyBool = embed TyBool

tyNat :: Ty
tyNat = embed TyNat

tyFun :: Ty -> Ty -> Ty
tyFun a b = embed (TyFun a b)

