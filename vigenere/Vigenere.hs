module Vigenere 
( encrypt
, decrypt
, shiftString
, unshiftString
) where

import Utils

-- shifts a single char by another char
shift :: Char -> Char -> Char
shift k char = toChar . shift' . toCode $ char
    where shift' x = (x + toCode k) `mod` 26

unshift :: Char -> Char -> Char
unshift k char = toChar . unshift' . toCode $ char
    where unshift' x = (x - toCode k) `mod` 26


-- shifts a string
shiftString :: Char -> String -> String
shiftString char = map (shift char)

unshiftString :: Char -> String -> String
unshiftString char = map (unshift char)


-- implementation of Vigenere cipher
encrypt :: String -> String -> String
encrypt key string = map (uncurry shift) . zip (cycle key) $ string

decrypt :: String -> String -> String
decrypt key string = map (uncurry unshift) . zip (cycle key) $ string
