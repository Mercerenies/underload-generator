
module Underload.Code(Code(..), pushLit, pushEmptyLit, pushStr,
                      nop, liftCode) where

import Underload.Instruction(Instruction(..), EmbedInstr(..), discard)
import Underload.Util(foldShows)

newtype Code = Code [Instruction]
    deriving (Eq, Semigroup, Monoid)

instance EmbedInstr Code where
    embedInstr instr = Code [instr]

instance Show Code where
    showsPrec _ (Code code) = foldShows (map shows code)

pushLit :: Code -> Code
pushLit (Code instrs) = embedInstr $ PushLit instrs

pushEmptyLit :: Code
pushEmptyLit = pushLit mempty

pushStr :: String -> Code
pushStr text = embedInstr $ PushStr text

nop :: Code
nop = pushEmptyLit <> discard

liftCode :: (Monoid a, EmbedInstr a) => Code -> a
liftCode (Code xs) = foldMap embedInstr xs
