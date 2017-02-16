
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LFC.Infer
  ( inferType
  ) where

import           LFC.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Control.Comonad.Cofree (Cofree(..))

import           LFC.Annotate
import           LFC.Error
import           LFC.FreshName
import           LFC.Memoize
import           LFC.Name
import           LFC.PrettyPrint
import           LFC.Tree
import           LFC.Tree.Typed
import           LFC.Tree.Untyped
import           LFC.Ty
import           LFC.Unify

import qualified LFC.Subst as Subst

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

type InferEff
  = '[ Reader Env
     , Writer (Set Constraint)
     , State Memo
     , Fresh
     , Exc TypeError
     ]

type Infer = Eff InferEff

type InferResult = Either TypeError

runInfer :: Infer a -> InferResult (a, Set Constraint)
runInfer a = fst <$> run res
  where
    writer = runReader a Map.empty
    state  = runWriter writer
    fresh  = runState state Map.empty
    exc    = runFresh' fresh 0
    res    = runError exc

addConstraint :: Ty -> Ty -> Infer ()
addConstraint a b = tell (Set.singleton (a, b))

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (Name, Ty) -> Infer Ty
withBinding a (x, ty) = local (Map.insert x ty) a

freshTy :: Infer Ty
freshTy = tyVar <$> freshName

inferType :: UntypedTree -> InferResult TypedTree
inferType tree = do
  (tyTree, cs) <- annotateTree tree
  subst <- solve cs
  pure $ Subst.apply subst <$> tyTree

annotateTree :: UntypedTree -> InferResult (TypedTree, (Set Constraint))
annotateTree tree = runInfer (annotateM' infer tree)

getEnv :: Member (Reader Env) r => Eff r Env
getEnv = ask

infer :: Cofree TreeF () -> Infer Ty
infer = memoizeM infer'
  where
    infer' (() :< tree) = do
      env <- getEnv
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

        Abs x a body -> do
          env <- getEnv
          b <- infer body `withBinding` (x, a)

          pure (tyFun a b)

        App f x -> do
          tf <- infer f
          tx <- infer x
          ty <- freshTy

          tf <-> tyFun tx ty

          pure ty

        RecEmpty ->
          pure (tyRecord tyRowEmpty)

        RecExtend r (l, v) -> do
          tyRec <- infer r
          tyVal <- infer v

          tyRow <- freshTy
          tyRec <-> tyRecord tyRow

          pure (tyRecord (tyRowExt tyRec (l, tyVal)))

        RecSelect r l -> do
          tyRec <- infer r
          tyRow <- freshTy
          a     <- freshTy
          r     <- freshTy

          tyRec <-> tyRecord tyRow
          tyRow <-> tyRowExt r (l, a)

          pure a
