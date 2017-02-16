{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LFC.Subst where

import           LFC.Prelude hiding (show, empty)

import qualified Data.Map.Strict as Map

import           LFC.Name
import           LFC.Ty
import           LFC.PrettyPrint

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)
import Text.Show

newtype Subst = Subst (Map Name Ty)
  deriving (Eq, Ord)

instance Show Subst where
  show (Subst map) = show (tupled (go <$> (Map.toList map)))
    where go ((n, ty)) = ppName n <+> text "\\" <+> ppTy ty

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

