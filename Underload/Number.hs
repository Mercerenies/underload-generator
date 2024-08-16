
module Underload.Number(pushNumber) where

import Underload.Instruction(EmbedInstr, append, dup, discard)
import Underload.Code(Reifiable, nop, pushLit, pushEmptyLit)
import Underload.Util(mpow)

-- Only supports a handful of small nonnegative integers right now.
--
-- Source: https://esolangs.org/wiki/Underload/Numbers
pushNumber :: (EmbedInstr a, Reifiable a, Monoid a) => Int -> a
pushNumber n = pushLit (go n)
    where go 0 = discard <> pushEmptyLit
          go 1 = nop
          go 2 = dup <> append
          go 3 = dup <> dup <> append <> append
          go 4 = (dup <> append) `mpow` 2
          go 5 = dup <> dup <> append <> dup <> append <> append
          go 6 = dup <> dup <> append <> append <> dup <> append
          go 7 = dup `mpow` 3 <> append `mpow` 2 <> dup <> append `mpow` 2
          go 8 = (dup <> append) `mpow` 3
          go 9 = dup <> dup <> append <> dup <> append <> dup <> append <> append
          go n = error $ "pushNumber: not a small nonnegative integer: " ++ show n
