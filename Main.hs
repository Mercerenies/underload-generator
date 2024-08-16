
module Main where

import Underload.Instruction
import Underload.Util()
import Underload.Code()
import Underload.Number(pushNumber)
import Underload.Builtins

main :: IO ()
main = do
  let code = pushNumber 3 <> pushNumber 8 <> add <> numToUnary 'x' <> output
  print code
