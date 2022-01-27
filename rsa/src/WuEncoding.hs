module WuEncoding 
( encodeA
, encodeB
, decodeA
, decodeB
, messageSizeA
, messageSizeB
, Alphabet
) where

import StringLib

type Alphabet = [Char]
abc, abc1 :: Alphabet
abc     = ['a'..'z']
abc1    = '1' : ['a'..'z']


encodeWithAlphabet :: Alphabet -> String -> Integer
encodeWithAlphabet alph = 
      foldl (\ acc x -> (integerLen alph)*acc + x) 0
    . toVector alph

-- setting A to zero makes it impossible to detect leading A's
-- the extended alphabet sort of remedies this
encodeA, encodeB  :: String -> Integer

encodeA = encodeWithAlphabet abc1
encodeB = encodeWithAlphabet abc


decodeWithAlphabet :: Alphabet -> Integer -> String
decodeWithAlphabet alph message
    | message == 0  = []
    | otherwise     = 
        toChar alph (message `mod` (integerLen alph)) :
        decodeWithAlphabet alph (message `quot` (integerLen alph))

decodeA, decodeB :: Integer -> String

decodeA = reverse . decodeWithAlphabet abc1
decodeB = reverse . decodeWithAlphabet abc

messageSizeWithAlphabet :: Alphabet -> Integer -> Int
messageSizeWithAlphabet alph n = truncate $ logBase
    (fromIntegral $ integerLen alph)
    (fromIntegral $ n + 1)

messageSizeA, messageSizeB :: Integer -> Int

messageSizeA = messageSizeWithAlphabet abc1
messageSizeB = messageSizeWithAlphabet abc

integerLen = fromIntegral . length
