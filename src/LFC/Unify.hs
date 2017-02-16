
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LFC.Unify
  ( solve
  , UnifyResult
  ) where

#define DEBUG_UNIFY 0

import           LFC.Prelude

import qualified Data.Set as Set

import           LFC.Subst (Subst)
import qualified LFC.Subst as Subst
import           LFC.Error
import           LFC.FreshName
import           LFC.Name
import           LFC.Ty

#if DEBUG_UNIFY
import           LFC.PrettyPrint
#endif

debugM :: Monad m => Text -> m ()

#if DEBUG_UNIFY
debugM = traceM
#else
debugM = const (pure ())
#endif

type Constraint = (Ty, Ty)

type UnifyState = (Subst, [Constraint])

type UnifyResult = Either TypeError

type UnifyEff
  = '[ State UnifyState
     , Fresh
     , Exc TypeError
     ]

type Unify = Eff UnifyEff

freshTy :: Unify Ty
freshTy = tyVar <$> freshName

runUnify :: UnifyState -> Unify a -> UnifyResult a
runUnify st a = run $ runError (runFresh (evalState a st))

solve :: Set Constraint -> UnifyResult Subst
solve cs = runUnify (Subst.empty, Set.toList cs) unify

unify :: Unify Subst
unify = do
  (sub, css) <- get
  case css of
    []            -> pure sub
    ((s, t) : cs) -> do


#if DEBUG_UNIFY
      let x' = show (ppTy s)
      let y' = show (ppTy t)
      debugM $ " * Unifying " <> x' <> " with " <> y'
#endif

      (sub', cs') <- unify' (s, t)
      put (sub' <> sub, cs' <> (Subst.onPair sub' <$> cs))
      unify

noSubst :: UnifyState
noSubst = (mempty, mempty)

unify' :: Constraint -> Unify UnifyState
unify' c@(s', t') = case bimap project project c of
  (s, t) | s == t ->
    pure noSubst

  (s@(TyVar n), t) | tyOccurs (embed t) n ->
    throwError (InfiniteType (embed s) (embed t))

  (s, t@(TyVar n)) | tyOccurs (embed s) n ->
    throwError (InfiniteType (embed t) (embed s))

  (s, TyVar n) ->
    pure (Subst.singleton n (embed s), mempty)

  (TyVar n, t) ->
    pure (Subst.singleton n (embed t), mempty)

  (TyFun a b, TyFun a' b') ->
    unifyPairs (a, a') (b, b')

  (TyRecord a, TyRecord b) ->
    unify' (a, b)

  (TyRowEmpty, TyRowEmpty) ->
    pure noSubst

  (TyRowExt r1 (l1, t1), row2@(TyRowExt _ _)) -> do
    (t2, r2, sub1) <- insertLabel (embed row2) l1
    (sub2, cs2)    <- unify' (Subst.apply sub1 t1, Subst.apply sub1 t2)

    let sub3 = sub1 <> sub2
    (sub4, cs3) <- unify' (Subst.apply sub3 r1, Subst.apply sub3 r2)

    pure (sub3 <> sub1, cs2 <> cs3)

  (s, t) ->
    throwError (CannotUnify (embed s) (embed t))

  where
    unifyPairs (a, a') (b, b') = do
      (s1, c1) <- unify' (a, a')
      (s2, c2) <- unify' (Subst.onPair s1 (b, b'))
      pure (s1 <> s2, c1 <> c2)


insertLabel :: Ty -> Name -> Unify (Ty, Ty, Subst)
insertLabel ty label = do
  (a, b, c) <- ins (project ty) label

#if DEBUG_UNIFY
  debugM ("insertLabel " <> show (ppName label) <> " in " <> show (ppTy ty))
  debugM ("headTy: "  <> show (ppTy a))
  debugM ("tailTy: "  <> show (ppTy b))
  debugM ("subst: "   <> show c)
  debugM ("headTy': " <> show (ppTy (Subst.apply c a)))
  debugM ("tailTy': " <> show (ppTy (Subst.apply c b)))
#endif

  pure (a,b, c)
    where
      ins :: TyF Ty -> Name -> Unify (Ty, Ty, Subst)
      ins TyRowEmpty l =
        throwError (CannotInsertLabel l)

      ins (TyRowExt tail (l, ty)) l' | l == l' =
        pure (ty, tail, Subst.empty)

      ins (TyRowExt tail (l, ty)) l' | TyVar r <- project tail = do
        freshRow <- freshTy
        freshVar <- freshTy

        pure ( freshVar
             , tyRowExt freshRow (l, ty)
             , Subst.singleton r (tyRecord (tyRowExt freshRow (l', freshVar)))
             )

      ins (TyRowExt tail row) l' = do
        (ty', tail', sub) <- insertLabel tail l'
        pure (ty', tyRowExt tail' row, sub)

      ins ty _ =
        throwError (CannotUnify (embed ty) (tyRecord (tyVar (Name "r"))))
