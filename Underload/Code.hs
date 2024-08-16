
module Underload.Code(Code(..), Reifiable(..), pushEmptyLit, pushStr,
                      nop, liftCode) where

import Underload.Instruction(Instruction(..), EmbedInstr(..), discard)
import Underload.Util(foldShows)

newtype Code = Code [Instruction]
    deriving (Eq, Semigroup, Monoid)

instance EmbedInstr Code where
    embedInstr instr = Code [instr]

instance Show Code where
    showsPrec _ (Code code) = foldShows (map shows code)

-- Class for code-like objects which can be reified as instructions
-- which push themselves.
class Reifiable a where
    pushLit :: a -> a

instance Reifiable Code where
    pushLit (Code instrs) = embedInstr $ PushLit instrs

pushEmptyLit :: (Reifiable a, Monoid a) => a
pushEmptyLit = pushLit mempty

pushStr :: String -> Code
pushStr text = embedInstr $ PushStr text

nop :: (Reifiable a, Monoid a, EmbedInstr a) => a
nop = pushEmptyLit <> discard

liftCode :: (Monoid a, EmbedInstr a) => Code -> a
liftCode (Code xs) = foldMap embedInstr xs
