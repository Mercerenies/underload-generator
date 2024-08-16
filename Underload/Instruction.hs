
module Underload.Instruction(Instruction(..), EmbedInstr(..),
                             swap, dup, discard, append, enclose, eval,
                             output, prepend) where

import Underload.Util(foldShows)

data Instruction = Swap
                 | Dup
                 | Discard
                 | Append
                 | Enclose
                 | Eval
                 | Output
                 | PushLit [Instruction]
                 | PushStr String
                 deriving (Eq)

instance Show Instruction where
    showsPrec _ instr =
        case instr of
          Swap -> ("~" ++)
          Dup -> (":" ++)
          Discard -> ("!" ++)
          Append -> ("*" ++)
          Enclose -> ("a" ++)
          Eval -> ("^" ++)
          Output -> ("S" ++)
          PushLit instrs -> ("(" ++) . foldShows (map shows instrs) . (")" ++)
          PushStr s -> ("(" ++) . (s ++) . (")" ++)

-- Typeclass for things which can embed instructions, such as code blocks.
class EmbedInstr a where
    embedInstr :: Instruction -> a

instance EmbedInstr Instruction where
    embedInstr = id

swap :: EmbedInstr a => a
swap = embedInstr Swap

dup :: EmbedInstr a => a
dup = embedInstr Dup

discard :: EmbedInstr a => a
discard = embedInstr Discard

append :: EmbedInstr a => a
append = embedInstr Append

enclose :: EmbedInstr a => a
enclose = embedInstr Enclose

eval :: EmbedInstr a => a
eval = embedInstr Eval

output :: EmbedInstr a => a
output = embedInstr Output

prepend :: (Semigroup a, EmbedInstr a) => a
prepend = swap <> append
