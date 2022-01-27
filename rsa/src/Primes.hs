module Primes where

import Math
import System.Random

-- for some pairs of numbers this still takes a long time
jacobiSymbol :: Integral a => a -> a -> a
jacobiSymbol a b
    | a == 1    = 1
    | even a    = exp1 * (jacobiSymbol (a `quot` 2) b)
    | otherwise = exp2 * (jacobiSymbol (b `mod` a) a)
    where
        exp1 =  if not $ 8 `divides` (b^2 - 1) then 0
                else (-1) ^ ((b^2 - 1) `quot` 8)
        exp2 =  if not $ 4 `divides` ((a-1)*(b-1)) then 0
                else (-1) ^ ((a-1) * (b-1) `quot` 4)

-- tests b with case a
solovayStrassenCriterion :: Integral a => a -> a -> Bool
solovayStrassenCriterion a b = and $ 
    [ odd b
    , gcd a b == 1
    , (jacobiSymbol a b) `mod` b  == 
        modularExponent a ((b-1) `quot` 2) b
    ]

-- selects 100 numbers to test b with
solovayStrassenTest :: 
    (Random a, Integral a, RandomGen g) => g -> a -> Bool
solovayStrassenTest g b = and testCases
    where
    randomTests = take 100 $ randomRs (1, b-1) g
    testCases   = map 
        (\ a -> solovayStrassenCriterion a b) randomTests
