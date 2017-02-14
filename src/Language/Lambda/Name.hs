
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lambda.Name
  ( Name(..)
  ) where

import           Protolude

newtype Name = Name [Char]
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

