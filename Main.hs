
module Main where

import Underload.Instruction
import Underload.Util()
import Underload.Code
import Underload.Number(pushNumber)
import Underload.Builtins
import Underload.Combinators(dip)

main :: IO ()
main = do
  let code = pushStr "abc" <> dip (pushNumber 3 <> pushNumber 8 <> add <> numToUnary 'x' <> output) <> output
  print code
