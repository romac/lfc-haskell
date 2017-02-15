
module LFC.Example where

import Protolude hiding (TypeError)

import LFC.Error
import LFC.Infer
import LFC.Name
import LFC.PrettyPrint
import LFC.Tree.Typed
import LFC.Tree.Untyped
import LFC.Ty

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

