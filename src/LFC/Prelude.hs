
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module LFC.Prelude
  ( module X

  -- State
  , evalState
  , gets
  , puts

  -- Writer
  , listen

  -- Fresh
  , runFresh
  ) where

import Protolude as X hiding
  ( Constraint
  , TypeError

  -- State
  , State
  , runState
  , evalState
  , get
  , gets
  , put
  , puts
  , modify

  -- Reader
  , Reader
  , runReader
  , ask
  , asks
  , local

  -- Writer
  , Writer
  , tell
  , listen
  , pass

  -- Error
  , throwError
  , catchError
  )

import Data.List.NonEmpty            as X (NonEmpty(..))

import Control.Monad.Freer           as X
import Control.Monad.Freer.Fresh     as X
import Control.Monad.Freer.Reader    as X
import Control.Monad.Freer.State     as X
import Control.Monad.Freer.Writer    as X
import Control.Monad.Freer.Exception as X

import Data.Functor.Foldable         as X hiding (fold)

-- * State

evalState :: Eff (State s ': r) w -> s -> Eff r w
evalState e s = fst <$> runState e s

gets :: Member (State s) r => (s -> a) -> Eff r a
gets f = f <$> get

puts :: Member (State s) r => (a -> s) -> a -> Eff r ()
puts f = put . f

-- * Writer

listen :: Monoid o => Eff (Writer o ': r) a -> Eff r ((a, o), o)
listen e = do
  (a, w) <- runWriter e
  pure ((a, w), w)

-- * Fresh

runFresh :: Eff (Fresh ': r) w -> Eff r w
runFresh = flip runFresh' 0

