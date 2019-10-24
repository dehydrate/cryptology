module Stats 
( bucket
, badness
, indexOfCoincidence
) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

-- simple "minifying" function; converts all letters l in a string to (n, l) where n is the number of times
-- in a row that l appears in the string
-- e.g. "aaron" becomes [(2, a), (1, r), (1, o), (1, n)]
condense :: (Eq a) => [a] -> [(Int, a)]
condense [x] = [(1, x)]
condense (x:xs)
    | y == x    = (n+1, x):rest
    | otherwise = (1, x):(n, y):rest
    where
        (n, y):rest = condense xs

-- tabulates absolute frequency of each char in the string
bucket :: (Ord a) => [a] -> [(Int, a)]
bucket string = condense . List.sort $ string

-- converts absolute frequencies of bucket to proportions
normalize :: (Ord a) => [a] -> [(a, Float)]
normalize string = map (\(count, char) -> (char, toFrequency count)) . bucket $ string
    where
        total = fromIntegral (length string)
        toFrequency n = (fromIntegral n) / total

-- score for how similar observed letter frequencies are to typical English
badness :: String -> Float 
badness string = 
    foldl 
        (\acc (key, freq) -> acc + abs (freq - defaultLookup key normalized)) 
        0 
        english
    where
        normalized = Map.fromList . normalize $ string
        unwrap (Just x) = x
        defaultLookup key table
            | Map.lookup key table == Nothing   = 0
            | otherwise                         = unwrap $ Map.lookup key table

-- English letter frequencies
english =
    [ ('e', 0.1202)
    , ('t', 0.0910)
    , ('a', 0.0812)
    , ('o', 0.0768)
    , ('i', 0.0731)
    , ('n', 0.0695)
    , ('s', 0.0628)
    , ('r', 0.0602)
    , ('h', 0.0592)
    , ('d', 0.0432)
    , ('l', 0.0398)
    , ('u', 0.0288)
    , ('c', 0.0271)
    , ('m', 0.0261)
    , ('f', 0.0230)
    , ('y', 0.0211)
    , ('w', 0.0209)
    , ('g', 0.0203)
    , ('p', 0.0182)
    , ('b', 0.0149)
    , ('v', 0.0111)
    , ('k', 0.0069)
    , ('x', 0.0017)
    , ('q', 0.0011)
    , ('j', 0.0010)
    , ('z', 0.0007) ]


-- the formula seems right, but the results are a letdown
indexOfCoincidence string =
    let n = fromIntegral . length $ string
        freqs = bucket string
        sum = fromIntegral . foldl (\acc (x, _) -> acc + x*(x-1)) 0 $ freqs
    in sum / (n*(n-1))
