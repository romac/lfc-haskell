
{-# LANGUAGE OverloadedStrings #-}

module LFC.PrettyPrint
  ( ppUntypedTree
  , ppTypedTree
  , ppTy
  , ppName
  ) where

import LFC.Prelude

import Control.Comonad.Trans.Cofree

import Text.PrettyPrint.ANSI.Leijen

import LFC.Name
import LFC.Tree
import LFC.Tree.Untyped
import LFC.Tree.Typed
import LFC.Ty

ppName :: Name -> Doc
ppName (Name n) = text n

ppTy :: Ty -> Doc
ppTy = cata ppTy'

ppTy' :: TyF Doc -> Doc
ppTy' (TyVar n)   = ppName n
ppTy' TyBool      = "Bool"
ppTy' TyNat       = "Nat"
ppTy' (TyFun a b) = a <+> "->" <+> b

ppUntypedTree :: UntypedTree -> Doc
ppUntypedTree = cata ppUntypedTree'

ppUntypedTree' :: TreeF Doc -> Doc
ppUntypedTree' Zero                   = text "0"
ppUntypedTree' (Succ t)               = text "succ" <+> t
ppUntypedTree' (Pred t)               = text "pred" <+> t
ppUntypedTree' (IsZero t)             = text "iszero" <+> t
ppUntypedTree' Tru                    = text "True"
ppUntypedTree' Fals                   = text "False"
ppUntypedTree' (If c t e)             = text "if" <+> c <+> "then" <+> t <+> "else" <+> e
ppUntypedTree' (Var (Name x))         = text x
ppUntypedTree' (Abs (Name x) ty body) = parens $ "λ" <> text x <> ":" <+> ppTy ty <> "." <+> body
ppUntypedTree' (App f x)              = f <+> x

ppTypedTree :: TypedTree -> Doc
ppTypedTree = cata ppTypedTree'

ppTypedTree' :: CofreeF TreeF Ty Doc -> Doc
ppTypedTree' (ty :< tree) = parens (ppUntypedTree' tree) <> ":" <+> ppTy ty

