
module LFC.Error where

import Protolude

import LFC.Name
import LFC.Ty

data TypeError
  = ValueNotFound Name
  | InfiniteType Ty Ty
  | CannotUnify Ty Ty
  | CannotInsertLabel Name
  deriving (Eq, Ord, Show, Read)

