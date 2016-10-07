
-- Worst pretty printer ever
module Language.Lambda.PrettyPrint
  ( pprintUntypedTree
  , pprintTypedTree
  , pprintTy
  ) where

import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (cata)

import Language.Lambda.Name
import Language.Lambda.Tree
import Language.Lambda.Tree.Untyped
import Language.Lambda.Tree.Typed
import Language.Lambda.Ty

pprintTy :: Ty -> String
pprintTy = cata pprintTy'

pprintTy' :: TyF String -> String
pprintTy' (TyVar (Name a)) = a
pprintTy' (TyFun a b)      = a ++ " -> " ++ b

pprintUntypedTree :: UntypedTree -> String
pprintUntypedTree = cata pprintUntypedTree'

pprintUntypedTree' :: TreeF String -> String
pprintUntypedTree' (Var (Name x)) = x
pprintUntypedTree' (Abs x body)   = "λ" ++ x ++ ". " ++ body
pprintUntypedTree' (App f x)      = f ++ " " ++ x

-- There must be a cleaner way to do that
pprintTypedTree :: TypedTree -> String
pprintTypedTree (ty :< tree) = "(" ++ pprintUntypedTree' (pprintTypedTree <$> tree) ++ ": " ++ pprintTy ty ++ ")"

