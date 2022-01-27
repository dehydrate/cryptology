module StringLib
( toCode
, toChar
, toString
, toVector
, isValid
, prep
) where

import qualified Data.Char as C

-- -1 indicates failure
indexOf :: (Eq a) => [a] -> a -> Integer
indexOf [] _ = -1
indexOf (x:xs) y
    | x == y    = 0
    | otherwise = 1 + (indexOf xs y)

-- fails on the empty list
nth :: (Eq a) => [a] -> Integer -> a
nth (x:xs) n
    | n == 0    = x
    | otherwise = nth xs (n-1)


-- conversion tools

toCode :: [Char] -> Char -> Integer
toCode alph = indexOf alph

toChar :: [Char] -> Integer -> Char
toChar alph = nth alph

toString :: [Char] -> [Integer] -> String
toString alph = map (toChar alph)

toVector :: [Char] -> String -> [Integer]
toVector alph = map (toCode alph)


-- input validation/preprocessing

isValid :: String -> Bool
isValid = all validChar
    where validChar c = (C.isLetter c) || (c == ' ')

prep :: String -> String
prep = filter (/= ' ') . map C.toLower
