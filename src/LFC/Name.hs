
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LFC.Name
  ( Name(..)
  ) where

import           Protolude

newtype Name = Name [Char]
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

