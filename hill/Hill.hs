module Hill
( encrypt
, decrypt
, alignedDecrypt
, validKey
, prettyKey
, matrixInverse
, leftMultiplyMatrix
, Alignment (Even, Odd)
, AlignedKey
, Key
, Vector
) where

import Math
import StringLib
import qualified Data.List as List
import qualified Data.Maybe as Maybe



-- keys are 2x2 matrices, represented as 4-length lists: [a, b, c, d] means
--  (a b)
--  (c d)
type Key = [Int]

-- this allows us to tell the decryption algorithm that the first block is incomplete
data Alignment  = Even | Odd deriving (Show)
type AlignedKey = (Key, Alignment)


-- better would be to pad the matrix elements
prettyKey :: Key -> String
prettyKey [a, b, c, d] = 
    "(" ++ show a ++ " " ++ show b ++ ")" ++ "\n" ++ 
    "(" ++ show c ++ " " ++ show d ++ ")"

validKey :: Key -> Bool
validKey = Maybe.isJust . matrixInverse


-- Nothing if the matrix has no inverse
matrixInverse :: Key -> Maybe Key
matrixInverse [a, b, c, d] =
    let det     = a*d - b*c
        detInv  = invMod 26 det
        inv'    = [d, -b, -c, a]
        inv     = map (\x -> (x*detInv) `mod` 26) inv' 
    in if detInv == 0 
        then Nothing
        else Just inv
matrixInverse _ = Nothing


-- vectors are 2x1 matrices, represented as 2-length lists: [a, b] means
--  (a)
--  (b)
type Vector = [Int] 

textToVectors :: String -> [Vector]
textToVectors []            = []
textToVectors [x]           = textToVectors [x, 'x']      -- pad with an odd number of letters
textToVectors (a:b:tail)    = (toVector [a,b]) : textToVectors tail

vectorsToText :: [Vector] -> String
vectorsToText = toString . List.concat


leftMultiplyBlock :: Key -> Vector -> Vector
leftMultiplyBlock [a, b, c, d] [e, f]
    = map (`mod` 26) [a*e + b*f, c*e + d*f]

leftMultiplyMatrix :: Key -> [Vector] -> [Vector]
leftMultiplyMatrix key = map (leftMultiplyBlock key)



encrypt, decrypt :: Key -> String -> Maybe String

encrypt k string
    | validKey k    = Just . vectorsToText . leftMultiplyMatrix k . textToVectors $ string
    | otherwise     = Nothing

-- naive decrypt assumes the first two characters of the string are a full block
decrypt k string
    | odd (length string)   = d (tail string)
    | otherwise             = d string
     where
        d string = leftMultiplyMatrix <$> matrixInverse k <*> pure (textToVectors string) >>= return . vectorsToText

alignedDecrypt :: Alignment -> Key -> String -> Maybe String
-- alignedDecrypt with an even alignment is just naive decrypt
alignedDecrypt Even k string    = decrypt k string
-- alignedDecrypt with an odd alignment means you can't decrypt the first character, maybe also the last
alignedDecrypt Odd k string
    | odd (length string)   = decrypt k (tail string)
    | otherwise             = decrypt k . init . tail $ string
