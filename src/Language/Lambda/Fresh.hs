
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lambda.Fresh
  ( FreshT(..)
  , Fresh
  , runFreshT
  , runFresh
  , MonadFresh
  , fresh
  ) where

import           Protolude

import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict hiding ((<>))
import           Control.Monad.RWS.Strict hiding ((<>))

import           Language.Lambda.Name

newtype FreshT m a = FreshT (StateT [Name] m a)
  deriving (Functor, Applicative, Monad, MonadState [Name], MonadTrans)

type Fresh = FreshT Identity

alphabet :: [Name]
alphabet = (\c -> Name (c:[])) <$> ['a'..'z']

supply :: [Name] -> [Name]
supply syms = syms <> go syms 1
  where
    go :: [Name] -> Int -> [Name]
    go s n = ((<> Name (show n)) <$> s) <> go s (n + 1)

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (FreshT st) = evalStateT st (supply alphabet)

runFresh :: Fresh a -> a
runFresh = runIdentity . runFreshT

class Monad m => MonadFresh m where
  fresh :: m Name

instance Monad m => MonadFresh (FreshT m) where
  fresh = do
    n : ns <- get
    put ns
    pure n

instance MonadFresh m => MonadFresh (ExceptT e m) where
  fresh = lift fresh

instance MonadFresh m => MonadFresh (ReaderT e m) where
  fresh = lift fresh

instance (Monoid w, MonadFresh m) => MonadFresh (WriterT w m) where
  fresh = lift fresh

instance (Monoid w, MonadFresh m) => MonadFresh (RWST r w s m) where
  fresh = lift fresh

