
{-# LANGUAGE FlexibleContexts #-}

module LFC.Memoize where

import           Protolude
import qualified Data.Map as Map

memoizeM :: (Ord a, MonadState (Map a b) m)
          => (a -> m b)
          -> a
          -> m b
memoizeM = memoizeM' identity const

memoizeM' :: (Ord a, MonadState s m)
          => (s -> Map a b)
          -> (Map a b -> s -> s)
          -> (a -> m b)
          -> a
          -> m b
memoizeM' getMemo setMemo f a = do
  cached <- Map.lookup a <$> gets getMemo
  case cached of
    Just b  -> pure b
    Nothing -> do
      b <- f a
      modify (\s -> setMemo (Map.insert a b (getMemo s)) s)
      pure b

