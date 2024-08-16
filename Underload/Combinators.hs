
module Underload.Combinators(dip, keep) where

import Underload.Code(Reifiable, pushLit)
import Underload.Instruction(EmbedInstr, eval, prepend, enclose, dup)

-- Assuming argument code has stack effect ( ..a -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
dip :: (EmbedInstr a, Reifiable a, Monoid a) => a -> a
dip inner = enclose <> pushLit inner <> prepend <> eval

-- Assuming argument code has stack effect ( ..a x -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
keep :: (EmbedInstr a, Reifiable a, Monoid a) => a -> a
keep inner = dup <> dip inner
