
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Lambda.Ty
  ( Ty
  , TyF(..)
  , tyBool
  , tyNat
  , tyFun
  , tyVar
  , tyFtv
  , tyOccurs
  ) where

import           Protolude

import           Data.Deriving

import qualified Data.Set as Set

import           Data.Functor.Foldable (Mu(..), embed, cata)

import           Language.Lambda.Name

data TyF a
  = TyBool
  | TyNat
  | TyFun a a
  | TyVar !Name
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Typeable)

$(deriveEq1     ''TyF)
$(deriveOrd1    ''TyF)
$(deriveShow1   ''TyF)
$(deriveRead1   ''TyF)

type Ty = Mu TyF

tyVar :: Name -> Ty
tyVar = embed . TyVar

tyBool :: Ty
tyBool = embed TyBool

tyNat :: Ty
tyNat = embed TyNat

tyFun :: Ty -> Ty -> Ty
tyFun a b = embed (TyFun a b)

tyFtv :: Ty -> Set Name
tyFtv = cata alg
  where
    alg (TyVar n)   = Set.singleton n
    alg (TyFun a b) = a <> b
    alg _           = Set.empty

tyOccurs :: Ty -> Name -> Bool
tyOccurs ty n = Set.member n (tyFtv ty)

