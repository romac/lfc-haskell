
module Language.Lambda.Name
  ( Name(..)
  ) where

newtype Name = Name String
  deriving (Eq, Ord, Show)

