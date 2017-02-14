
module Language.Lambda.Example where

import Protolude hiding (TypeError)

import Language.Lambda.Error
import Language.Lambda.Infer
import Language.Lambda.Name
import Language.Lambda.PrettyPrint
import Language.Lambda.Tree.Typed
import Language.Lambda.Tree.Untyped
import Language.Lambda.Ty

example1 :: UntypedTree
example1 = let x = Name "x" in
  mkApp
    (mkAbs x tyNat
      (mkIf (mkIsZero (mkVar x))
           mkTrue
           mkFalse))
   (mkSucc mkZero)

example1' :: IO ()
example1' = case inferType example1 of
              Left err -> print err
              Right tree -> print (ppTypedTree tree)

