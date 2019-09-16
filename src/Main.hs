module Main where

import LLL
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

main :: IO ()
main = do
  putStrLn
    . Pretty.render
    . pPrint
    $ example1
  print =<< execute example1

example1 :: Program
example1 = Program $ pure $ Define
  (Bind (Name "main") (Just (Implies Unit Unit)))
  (ImpliesIntro (Bind (Name "args") (Just Unit))
    (AndElim
      (Bind (Name "a") Nothing)
      (Bind (Name "b") Nothing)
      (As (AndIntro (NumberIntro 1) (NumberIntro 2)) (And Number Number))
    $ Let (Bind (Name "c") (Just Number))
      (WithElim FirstTag
        (As
          (WithIntro
            (As (NumberIntro 3) Number)
            (As (BoolIntro False) Bool))
          (With Number Bool)))
    $ ParElim
      (OrElim (OrIntro LeftTag (NumberIntro 4))
        (Bind (Name "d") (Just Number))
        (Group BeginEnd
          (Sequence
            (Trace
              (BinaryElim Multiplication
                (Var (Name "c"))
                (Var (Name "d"))))
            (ParIntro
              (BoolIntro True)
              (NumberIntro 5))))
        (Bind (Name "e") Nothing)
        Abort)
      (Bind (Name "f") Nothing)
      (Trace (NumberIntro 6))
      (Bind (Name "g") Nothing)
      BottomIntro))
