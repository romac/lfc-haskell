{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LFC.Subst where

import           Protolude hiding (empty)

import qualified Data.Map.Strict as Map

import           Data.Functor.Foldable (cata, embed)

import           LFC.Name
import           LFC.Ty

newtype Subst = Subst (Map Name Ty)

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst (Map.map (apply (Subst s1)) s2 <> s1)

instance Monoid Subst where
  mempty  = empty
  mappend = (<>)

apply :: Subst -> Ty -> Ty
apply (Subst s) = cata apply'
  where
    apply' ty@(TyVar n) = Map.findWithDefault (embed ty) n s
    apply' ty           = embed ty

empty :: Subst
empty = Subst Map.empty

singleton :: Name -> Ty -> Subst
singleton k v = Subst (Map.singleton k v)

onPair :: Subst -> (Ty, Ty) -> (Ty, Ty)
onPair s = bimap (apply s) (apply s)

