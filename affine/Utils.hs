module Utils
( indexOf
, nth
, toVector
, toString
, toCode
, toChar
) where

-- -1 indicates failure
indexOf :: (Eq a) => [a] -> a -> Int
indexOf [] _ = -1
indexOf (x:xs) y
    | x == y    = 0
    | otherwise = 1 + (indexOf xs y)

-- no way to indicate failure without using Maybes etc.
nth :: (Eq a) => [a] -> Int -> a
nth (x:xs) n
    | n == 0    = x
    | otherwise = nth xs (n-1)

toVector :: String -> [Int]
toVector string = map toCode string

toString :: [Int] -> String
toString nums = map toChar nums

toCode :: Char -> Int
toCode = indexOf ['a'..'z']

toChar :: Int -> Char
toChar = nth ['a'..'z']
