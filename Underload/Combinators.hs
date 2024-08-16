
module Underload.Combinators(dip, keep) where

import Underload.Code(Code, pushLit)
import Underload.Instruction(eval, prepend, enclose, dup)

-- Assuming argument code has stack effect ( ..a -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
dip :: Code -> Code
dip inner = enclose <> pushLit inner <> prepend <> eval

-- Assuming argument code has stack effect ( ..a x -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
keep :: Code -> Code
keep inner = dup <> dip inner
