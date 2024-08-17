
module Main where

import Underload.Instruction
import Underload.Fried
import Underload.Code
import Underload.Number(pushNumber)
import Underload.Builtins
import Underload.Util(mpow)
import Underload.Combinators

main :: IO ()
main = do
  putStr (show projectEuler181)
  putStr printNumeral
  putStr printNumeral
  putStr printNumeral
  putStrLn printNumeral

tmp :: Code
tmp = pushNumber 1 <> pushNumber 2 <> pushNumber 3 <> pushNumber 4 <> pushNumber 5 <> generateAndAccum0

add3 :: Code
add3 = fry code
    where code = pushLit (dup `mpow` 2 <> qref 2 <> swap <> qref 1 <> append <> swap <> qref 0 <> append)

generateAndAccum0 :: Code
generateAndAccum0 = add3 <> dipN 3 (mul <> pushNumber 2 <> mul)

-- Stack effect: ( x -- )
--
-- Source: https://esolangs.org/wiki/Underload#Print_a_numeral_as_decimal
printNumeral :: String
printNumeral = "((:(1)*(:(2)*(:(3)*(:(4)*(:(5)*(:(6)*(:(7)*(:(8)*(:(9)*(!~:^)))))))))):(~^~(~a~*~a~*)~a*^:(0)*)~a*~:(a(:^)*())~*a(:^)*~()~(0)~(~!^))~*^^!S!!!"

projectEuler181 :: Code
projectEuler181 =
    -- Step 1: Push a[0], a[1], a[2].
    pushNumber 1 <> pushNumber 2 <> pushNumber 4 <>
    -- Step 2: Generate a[3] through a[14] using fried addition.
    finiteLoop 5 add3 <> -- should be 12
    -- Step 3: Generate a[15] through a[17] on the stack. Same trick.
    finiteLoop 3 add3 <>
    -- Step 4: Generate a[18] and start our accumulator at
    -- 2*a[15]*a[14].
    generateAndAccum0
