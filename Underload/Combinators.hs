
module Underload.Combinators(dip, keep, dupToTop) where

import Underload.Code(Reifiable, pushLit)
import Underload.Instruction(EmbedInstr, eval, prepend, enclose, dup, swap)

-- Assuming argument code has stack effect ( ..a -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
dip :: (EmbedInstr a, Reifiable a, Monoid a) => a -> a
dip inner = enclose <> pushLit inner <> prepend <> eval

-- Assuming argument code has stack effect ( ..a x -- ..b ), the result
-- has stack effect ( ..a x -- ..b x ).
keep :: (EmbedInstr a, Reifiable a, Monoid a) => a -> a
keep inner = dup <> dip inner

-- Stack index 0 is the top of the stack. `dupToTop 0` is equivalent
-- to `dup`.
dupToTop :: (EmbedInstr a, Reifiable a, Monoid a) => Int -> a
dupToTop n | n < 0 = error "dupToTop: n < 0"
dupToTop n = go n
    where go 0 = dup
          go n = dip (go $ n - 1) <> swap
