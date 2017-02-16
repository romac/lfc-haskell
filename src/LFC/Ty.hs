
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module LFC.Ty
  ( Ty
  , TyF(..)
  , tyBool
  , tyNat
  , tyFun
  , tyVar
  , tyRecord
  , tyRowEmpty
  , tyRowExt
  , tyFtv
  , tyOccurs
  ) where

import           Protolude

import           Data.Deriving

import qualified Data.Set as Set

import           Data.Functor.Foldable (Fix, embed, cata)

import           LFC.Name

data TyF a
  = TyBool
  | TyNat
  | TyFun a a
  | TyVar !Name
  | TyRecord a
  | TyRowEmpty
  | TyRowExt a (Name, a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Typeable)

$(deriveEq1     ''TyF)
$(deriveOrd1    ''TyF)
$(deriveShow1   ''TyF)
$(deriveRead1   ''TyF)

type Ty = Fix TyF

tyVar :: Name -> Ty
tyVar = embed . TyVar

tyBool :: Ty
tyBool = embed TyBool

tyNat :: Ty
tyNat = embed TyNat

tyFun :: Ty -> Ty -> Ty
tyFun a b = embed (TyFun a b)

tyRecord :: Ty -> Ty
tyRecord = embed . TyRecord

tyRowEmpty :: Ty
tyRowEmpty = embed TyRowEmpty

tyRowExt :: Ty -> (Name, Ty) -> Ty
tyRowExt r e = embed (TyRowExt r e)

tyFtv :: Ty -> Set Name
tyFtv = cata alg
  where
    alg (TyVar n)           = Set.singleton n
    alg (TyFun a b)         = a <> b
    alg (TyRecord r)        = r
    alg (TyRowExt r (_, v)) = v <> r
    alg _                   = Set.empty

tyOccurs :: Ty -> Name -> Bool
tyOccurs ty n = Set.member n (tyFtv ty)

