module StringLib
( indexOf
, nth
, toCode
, toChar
, toString
, toVector
, isValid
, prep
) where

import qualified Data.Char as Char

-- -1 indicates failure
indexOf :: (Eq a) => [a] -> a -> Int
indexOf [] _ = -1
indexOf (x:xs) y
    | x == y    = 0
    | otherwise = 1 + (indexOf xs y)

-- no way to indicate failure without using a Maybe
nth :: (Eq a) => [a] -> Int -> a
nth (x:xs) n
    | n == 0    = x
    | otherwise = nth xs (n-1)


-- conversion tools

toCode :: Char -> Int
toCode = indexOf ['a'..'z']

toChar :: Int -> Char
toChar = nth ['a'..'z']

toString :: [Int] -> String
toString nums = map toChar nums

toVector :: String -> [Int]
toVector string = map toCode string


-- input validation/preprocessing

isValid :: String -> Bool
isValid string = all validChar string
    where validChar c = (Char.isLetter c) || (c == ' ')

prep :: String -> String
prep string = filter (/= ' ') . map Char.toLower $ string
