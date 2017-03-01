
{-# LANGUAGE FlexibleContexts #-}

module LFC.Memoize where

import           LFC.Prelude
import qualified Data.Map as Map

memoizeM :: (Ord a, Member (State (Map a b)) r)
         => (a -> Eff r b)
         -> a
         -> Eff r b
memoizeM = memoizeM' identity const

memoizeM' :: (Ord a, Member (State s) r)
          => (s -> Map a b)
          -> (Map a b -> s -> s)
          -> (a -> Eff r b)
          -> a
          -> Eff r b
memoizeM' getMemo setMemo f a = do
  s <- gets getMemo
  case Map.lookup a s of
    Just b  -> pure b
    Nothing -> do
      b <- f a
      modify (\s -> setMemo (Map.insert a b (getMemo s)) s)
      pure b

