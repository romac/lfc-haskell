
module LFC.Example where

import Protolude hiding (TypeError)

import LFC.Error
import LFC.Infer
import LFC.Name
import LFC.PrettyPrint
import LFC.Tree.Typed
import LFC.Tree.Untyped
import LFC.Ty

printTreeType :: UntypedTree -> IO ()
printTreeType untyped =
  case inferType untyped of
    Left err    -> print (ppTypeError err)
    Right typed -> print (ppTypedTree typed)

example1 :: UntypedTree
example1 = let x = Name "x" in
  mkApp
    (mkAbs x tyNat
      (mkIf (mkIsZero (mkVar x))
           mkTrue
           mkFalse))
   (mkSucc mkZero)

example1' :: IO ()
example1' = printTreeType example1

example2 :: UntypedTree
example2 =
  let x  = Name "x"
      y  = Name "y"
      r  = Name "r"
      ty = tyRecord (tyRowExt (tyRecord (tyRowExt (tyRecord tyRowEmpty) (y, tyBool))) (x, tyNat))
    in
  mkApp
    (mkAbs r ty
      (mkRecSelect (mkVar r) x))
    (mkRecExtend
      (mkRecExtend mkRecEmpty
        (y, mkTrue))
      (x, mkZero))

example2' :: IO ()
example2' = printTreeType example2

example3 :: UntypedTree
example3 =
  let x  = Name "x"
      y  = Name "y"
      r  = Name "r"
      ty = tyRecord (tyRowExt (tyRecord (tyRowExt (tyRecord tyRowEmpty) (y, tyBool))) (x, tyNat))
    in
  mkApp
    (mkAbs r ty
      (mkRecSelect (mkVar r) y))
    (mkRecExtend
      (mkRecExtend mkRecEmpty
        (y, mkTrue))
      (x, mkZero))

example3' :: IO ()
example3' = printTreeType example3

