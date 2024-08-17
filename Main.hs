
module Main where

import Underload.Instruction
import Underload.Fried
import Underload.Code
import Underload.Number(pushNumber)
import Underload.Builtins
import Underload.Combinators

main :: IO ()
main = do
  let code =
          -- Naive addition
          pushNumber 3 <> pushNumber 8 <> add <> numToUnary 'x' <> output <>
          -- Fried addition
          pushNumber 3 <> pushNumber 8 <> fry (pushStr "y" <> dup <> qref 1 <> swap <> qref 0 <> append) <> output
  print code
