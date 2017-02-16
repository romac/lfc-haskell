
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LFC.PrettyPrint
  ( ppTypeError
  , ppUntypedTree
  , ppTypedTree
  , ppTy
  , ppName
  ) where

import LFC.Prelude hiding ((<>))

import Control.Comonad.Trans.Cofree

import Text.PrettyPrint.ANSI.Leijen

import LFC.Error
import LFC.Name
import LFC.Tree
import LFC.Tree.Typed
import LFC.Tree.Untyped
import LFC.Ty

ppTypeError :: TypeError -> Doc
ppTypeError (ValueNotFound v)     = "value not found:" <+> ppName v
ppTypeError (InfiniteType a b)    = "cannot unify infinite type:" <+> ppTy a <+> "with" <+> ppTy b
ppTypeError (CannotUnify a b)     = "cannot unify:" <+> ppTy a <+> "with" <+> ppTy b
ppTypeError (CannotInsertLabel l) = "cannot insert label:" <+> ppName l

ppName :: Name -> Doc
ppName (Name n) = text n

ppTy :: Ty -> Doc
ppTy = cata ppTy'

ppTy' :: TyF Doc -> Doc
ppTy' (TyVar n)           = ppName n
ppTy' TyBool              = "Bool"
ppTy' TyNat               = "Nat"
ppTy' (TyFun a b)         = a <+> "->" <+> b
ppTy' (TyRecord row)      = braces' row
ppTy' TyRowEmpty          = mempty
ppTy' (TyRowExt r (x, t)) = ppName x <+> ":" <+> t <+> "|" <+> r

braces' :: Doc -> Doc
braces' d | null (show d :: [Char]) = "{}"
braces' d                           = "{" <+> d <+> "}"

ppUntypedTree :: UntypedTree -> Doc
ppUntypedTree = cata ppUntypedTree'

ppUntypedTree' :: TreeF Doc -> Doc
ppUntypedTree' Zero                 = "0"
ppUntypedTree' (Succ t)             = "succ" <+> t
ppUntypedTree' (Pred t)             = "pred" <+> t
ppUntypedTree' (IsZero t)           = "iszero" <+> t
ppUntypedTree' Tru                  = "True"
ppUntypedTree' Fals                 = "False"
ppUntypedTree' (If c t e)           = "if" <+> c <+> "then" <+> t <+> "else" <+> e
ppUntypedTree' (Var x)              = ppName x
ppUntypedTree' (Abs x ty body)      = parens $ "λ" <> ppName x <> ":" <+> ppTy ty <> "." <+> body
ppUntypedTree' (App f x)            = f <+> x
ppUntypedTree' RecEmpty             = "{}"
ppUntypedTree' (RecExtend r (l, t)) = braces' $ ppName l <+> "=" <+> t <+> "|" <+> r
ppUntypedTree' (RecSelect r l)      = r <> "." <> ppName l

ppTypedTree :: TypedTree -> Doc
ppTypedTree = cata ppTypedTree'

ppTypedTree' :: CofreeF TreeF Ty Doc -> Doc
ppTypedTree' (ty :< tree) = brackets (ppTy ty) <> ppUntypedTree' tree

