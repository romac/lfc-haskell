
module Language.Lambda.Error where

import Protolude

import Language.Lambda.Name
import Language.Lambda.Ty

data TypeError
  = ValueNotFound Name
  | InfiniteType Ty Ty
  | CannotUnify Ty Ty
  deriving (Eq, Ord, Show, Read)

