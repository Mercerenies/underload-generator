
module Underload.Builtins(branch, numToUnary,
                          mul, pow, add) where

import Underload.Util(mpow)
import Underload.Code(Code, pushLit, pushStr)
import Underload.Instruction(dup, discard, eval, append, prepend, swap)

-- Stack effect: ( ..a n -- ..b ) assuming each branch has effect ( ..a -- ..b )
--
-- Branch instruction: chooses which branch to take by popping the top
-- stack element. If the list of branches has length N, then the top
-- stack element must be an integer from 0 (representing the first
-- branch in the list) up to N-1 (the last branch in the list), in
-- Church encoding representation.
branch :: [Code] -> Code
branch codes = lookupTable <> swap <>
               pushLit (eval <> pushLit discard) <> prepend <>
               pushLit (eval <> eval) <> append <>
               eval
    where lookupTable :: Code
          lookupTable = pushLit $ foldMap tableEntry . zip [0..] $ reverse codes
          tableEntry :: (Int, Code) -> Code
          tableEntry (n, c) = pushLit $ discard `mpow` n <> c

-- Stack effect: ( x -- x )
numToUnary :: Char -> Code
numToUnary ch = pushStr [ch] <> swap <> eval

-- Stack effect: ( x y -- z )
mul :: Code
mul = append

-- Stack effect: ( x y -- z )
pow :: Code
pow = eval

-- Stack effect: ( x y -- z )
add :: Code
add = pushLit swap <> append <>
      pushLit dup <> prepend <>
      swap <> pushLit append <> append <> append
