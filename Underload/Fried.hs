
module Underload.Fried() where

-- Fried quotations, similar to Factor.

import Underload.Instruction(Instruction(..), EmbedInstr(..))
import Underload.Util(foldShows)

newtype QCode = QCode [QInstruction]
    deriving (Eq, Semigroup, Monoid)

data QInstruction = QGround Instruction
                  | QPushLit [QInstruction]
                  | QRef StackIndex
                    deriving (Eq)

-- Index 0 is the top of the stack, positive indices count downward
-- from there. Negative indices are not permitted.
type StackIndex = Int

instance Show QInstruction where
    showsPrec n qinstr =
        case qinstr of
          QGround instr -> showsPrec n instr
          QPushLit qinstrs -> ("(" ++) . foldShows (map shows qinstrs) . (")" ++)
          QRef index -> ("<" ++) . shows index . (">" ++)

instance EmbedInstr QInstruction where
    embedInstr = QGround

instance EmbedInstr QCode where
    embedInstr instr = QCode [embedInstr instr]
