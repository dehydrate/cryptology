module Stats
( bucket
, badness
)
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

condense :: (Eq a) => [a] -> [(Int, a)]
condense [x] = [(1, x)]
condense (x:xs)
    | y == x    = (n+1, x):rest
    | otherwise = (1, x):(n, y):rest
    where
        (n, y):rest = condense xs

bucket :: (Ord a) => [a] -> [(Int, a)]
bucket string = condense . List.sort $ string

normalize :: (Ord a) => [a] -> [(a, Float)]
normalize string = map (\(count, char) -> (char, toFrequency count)) . bucket $ string
    where
        total = fromIntegral (length string)
        toFrequency n = (fromIntegral n) / total

-- badness sums the difference between the frequency of a letter in the 
-- input string and its general English frequency for each letter. Any 
-- letter that does not appear in the input string has frequency 0.
-- see accompanying excel file for some interesting data plots
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


-- root sum of squares
-- no apparent advantage over badness as described above
badness2 :: String -> Float
badness2 string = sqrt 
    . foldl
        (\ acc (key, freq) -> acc + (freq - defaultLookup key normalized)**2)
        0
        $ english
    where
        normalized = Map.fromList . normalize $ string
        unwrap (Just x) = x
        defaultLookup key table
            | Map.lookup key table == Nothing   = 0
            | otherwise                         = unwrap $ Map.lookup key table


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
