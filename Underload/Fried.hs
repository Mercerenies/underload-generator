
module Underload.Fried(QCode(), QInstruction(), StackIndex, fry, fryLambda, qref) where

-- Fried quotations, similar to Factor.

import Underload.Instruction(Instruction(..), EmbedInstr(..), enclose, append, eval)
import Underload.Code(Code(..), pushLit, pushEmptyLit)
import Underload.Util(foldShows)
import Underload.Combinators(dupToTop)

newtype QCode = QCode [QInstruction]
    deriving (Eq)

data QInstruction = QGround [Instruction]
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
    embedInstr x = QGround [x]

instance EmbedInstr QCode where
    embedInstr instr = QCode [embedInstr instr]

instance Semigroup QCode where
    QCode xs <> QCode ys = QCode $ go xs ys
        where go [] ys = ys
              -- Do some special checks on the last element of x to
              -- combine it with QGrounds on the right.
              go [x] [] = [x]
              go [x] (y:ys) =
                  case (x, y) of
                    (QGround left, QGround right) -> QGround (left ++ right) : ys
                    (x, y) -> x : y : ys
              go (x:xs) ys = x : go xs ys

instance Monoid QCode where
    mempty = QCode []

qref :: StackIndex -> QCode
qref n = QCode [QRef n]

fry :: QCode -> Code
fry qcode = fryLambda qcode <> eval

fryLambda :: QCode -> Code
fryLambda (QCode instrs) = pushEmptyLit <> generalFry 1 instrs

-- General-purpose frying algorithm. Less efficient but works in all
-- cases.
generalFry :: Int -> [QInstruction] -> Code
generalFry extraStackValues = foldMap go
    where go (QGround instrs) = pushLit (Code instrs) <> append
          go (QPushLit qinstrs) = pushEmptyLit <> generalFry (extraStackValues + 1) qinstrs <> enclose <> append
          go (QRef index) = dupToTop (index + extraStackValues) <> append
