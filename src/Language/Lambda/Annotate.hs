
module Language.Lambda.Annotate
  ( annotate
  , annotate'
  ) where

import Data.Functor.Foldable (Fix, unfix, cata)
import Control.Comonad.Cofree (Cofree(..))

annotate :: Functor f => (Fix f -> a) -> Fix f -> Cofree f a
annotate f x = f x :< fmap (annotate f) (unfix x)

annotate' :: Functor f => (f a -> a) -> Fix f -> Cofree f a
annotate' f = annotate (cata f)

