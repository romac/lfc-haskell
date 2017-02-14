
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.Infer
  ( inferType
  ) where

import           Protolude hiding (Constraint, TypeError)

import           Data.Maybe (isJust, fromJust)
import           Data.List (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Data.Functor.Foldable     (cata, project)
import           Data.Functor.Identity

import           Control.Comonad.Cofree (Cofree(..))
import           Control.Monad.RWS.Strict

import           Language.Lambda.Annotate
import           Language.Lambda.Error
import           Language.Lambda.Fresh
import           Language.Lambda.Memoize
import           Language.Lambda.Name
import           Language.Lambda.PrettyPrint
import           Language.Lambda.Tree
import           Language.Lambda.Tree.Typed
import           Language.Lambda.Tree.Untyped
import           Language.Lambda.Ty
import           Language.Lambda.Unify

import qualified Language.Lambda.Subst as Subst

#define DEBUG 0
debugM :: Monad m => Text -> m ()

#if DEBUG
debugM = traceM
#else
debugM = const (pure ())
#endif

type Env = Map Name Ty

type Constraint = (Ty, Ty)

type Memo = Map (Cofree TreeF ()) Ty

newtype Infer a
  = Infer
    { runInfer :: RWST
                    Env
                    (Set Constraint)
                    Memo
                    (ExceptT TypeError Fresh)
                    a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadWriter (Set Constraint)
    , MonadState Memo
    , MonadError TypeError
    , MonadFresh
    )

addConstraint :: Ty -> Ty -> Infer ()
addConstraint a b = tell (Set.singleton (a, b))

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (Name, Ty) -> Infer Ty
withBinding a (x, ty) = local (Map.insert x ty) a

freshTy :: Infer Ty
freshTy = tyVar <$> fresh

inferType :: UntypedTree -> Either TypeError TypedTree
inferType tree = do
  (tyTree, cs) <- annotateTree tree
  subst <- solve cs
  pure $ Subst.apply subst <$> tyTree

annotateTree :: UntypedTree -> Either TypeError (TypedTree, (Set Constraint))
annotateTree tree =
  let infer' = annotateM' (memoizeM infer) tree
      except = evalRWST (runInfer infer') Map.empty Map.empty
      fresh  = runExceptT except
   in runFresh fresh

infer :: Cofree TreeF () -> Infer Ty
infer = memoizeM infer'
  where
    infer' (() :< tree) = do
      env <- ask
      case tree of
        Tru  -> pure tyBool
        Fals -> pure tyBool
        Zero -> pure tyNat

        Succ t -> do
          ty <- infer t
          ty <-> tyNat
          pure tyNat

        Pred t -> do
          ty <- infer t
          ty <-> tyNat
          pure tyNat

        IsZero t -> do
          ty <- infer t
          ty <-> tyNat
          pure tyBool

        Var name -> do
          env <- ask
          case Map.lookup name env of
            Nothing -> throwError (ValueNotFound name)
            Just ty -> pure ty

        If cnd thn els -> do
          tc <- infer cnd
          tt <- infer thn
          te <- infer els

          tc <-> tyBool
          tt <-> te

          pure tt

        Abs x ty body -> do
          env <- ask
          tp  <- freshTy
          tp' <- infer body `withBinding` (x, tp)

          pure (tyFun tp tp')

        App f x -> do
          tf <- infer f
          tx <- infer x
          ty <- freshTy

          tf <-> tyFun tx ty

          pure ty

