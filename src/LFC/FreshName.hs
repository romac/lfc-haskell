
{-# LANGUAGE FlexibleContexts #-}

module LFC.FreshName
  ( freshName
  ) where

import           LFC.Prelude
import           LFC.Name

import qualified Data.List.NonEmpty as NE

alphabet :: NonEmpty Name
alphabet = (\c -> Name (c:[])) <$> NE.fromList ['a'..'z']

-- FIXME: Rewrite this as a tail-recursive loop
supply :: (Functor f, Semigroup (f Name)) => f Name -> f Name
supply syms = syms <> go syms 1
  where
    go s n =
      let ps = fmap (\x -> x <> Name (show n)) s
       in ps <> go s (n + 1)

names :: NonEmpty Name
names = supply alphabet

freshName :: Member Fresh r => Eff r Name
freshName = do
  n <- fresh
  pure (names NE.!! n)

