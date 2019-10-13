module Keys 
( keySolutions
, bestKey
, probableKeyLen -- not very effective
, tryVariousKeylengths
) where 

import Stats
import Utils
import qualified Vigenere as Vigenere
import qualified Data.List as List


--- blocking the input ---

-- gets all the alphabets for each key letter
alphabets :: Int -> String -> [String]
alphabets keylen string = columns . block keylen $ string

-- divides string into n-sized blocks
block n string 
    | n >= length string    = [string]
    | otherwise             = first:rest
    where
        (first, tail) = splitOff n string
        rest = block n tail

-- helper for block
splitOff :: Int -> [a] -> ([a], [a])
splitOff 0 list = ([], list)
splitOff n (h:t) = (h:front, back) where
    (front, back) = splitOff (n-1) t

-- transposes a list of lists by grouping all the firsts, seconds, etc.
columns [] = []
columns listlist = 
    let (col, tail) = removeColumn listlist in
    if (col == []) 
        then columns tail
        else (col : columns tail)

-- helper for columns
removeColumn :: [[a]] -> ([a], [[a]])
removeColumn [] = ([], [])
removeColumn ([]:rest) = removeColumn rest
removeColumn ((h:t):rest) = (h:heads, t:tails) where
    (heads, tails) = removeColumn rest


--- guessing keys ---

-- sorts letters a..z from most to least probable shift key for an observed shifted alphabet
probableShift :: String -> String
probableShift string = List.sortBy criteria ['a'..'z'] where
    criteria c1 c2 = compare 
        (badness . Vigenere.unshiftString c1 $ string) 
        (badness . Vigenere.unshiftString c2 $ string)

-- does probableShift for each key index, given key length and ciphertext
keySolutions :: Int -> String -> [String] 
keySolutions size string = map probableShift . alphabets size $ string

-- it would be better to sort the 26^n possible keys by total badness, but for now this will do
bestKey :: [String] -> String
bestKey options = 
    let (col, _) = removeColumn options 
    in col

-- using index of coincidence method
-- this is ineffective
probableKeyLen string = 0.0265 * n / (0.065 - i + n*(i - (1/26)))
    where
        n = fromIntegral . length $ string
        i = indexOfCoincidence string

-- hacky alternative way to try keys of varying length
tryVariousKeylengths :: Int -> String -> String
tryVariousKeylengths max string = 
      head 
    . List.sortBy criteria
    . map bestKey 
    . map (\l -> keySolutions l string) $ [1..max]
    where
        criteria k1 k2 = compare  
            (badness . Vigenere.decrypt k1 $ string) 
            (badness . Vigenere.decrypt k2 $ string)
