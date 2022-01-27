module PrimeTests where

import Test.HUnit
import Primes
import System.Random

jacobiSymbol1 = TestCase $ assertEqual 
    "computes Jacobi 1 2"
    1
    (jacobiSymbol 1 2)

jacobiSymbol2 = TestCase $ assertEqual
    "computes Jacobi 3 5"
    (-1)
    (jacobiSymbol 3 5)

jacobiSymbol3 = TestCase $ assertEqual
    "computes Jacobi 2 4"
    0
    (jacobiSymbol 2 4)

jacobiSymbol4 = TestCase $ assertEqual
    "computes Jacobi 4 9"
    1
    (jacobiSymbol 4 9)

ssCritIsValid1 = TestCase $ assertBool
    "Solovay and Strassen for 13 and 7"
    (solovayStrassenCriterion 7 13)

ssCritIsValid2 = TestCase $ assertBool
    "Solovay and Strassen for 17 and 4"
    (solovayStrassenCriterion 4 17)

ssIdentifiesPrime = TestCase $ assertBool
    "Solovay and Strassen finds 397 prime"
    (solovayStrassenTest (mkStdGen 42) (397 :: Integer))

ssIdentifiesComposite = TestCase $ assertBool
    "Solovay and Strassen finds 400 composite"
    (not $ solovayStrassenTest (mkStdGen 42) (400 :: Integer))

tests = 
    [ TestLabel "jacobiSymbol1" jacobiSymbol1
    , TestLabel "jacobiSymbol2" jacobiSymbol2
    , TestLabel "jacobiSymbol3" jacobiSymbol3
    , TestLabel "jacobiSymbol4" jacobiSymbol4
    , TestLabel "ssCritIsValid1" ssCritIsValid1
    , TestLabel "ssCritIsValid2" ssCritIsValid2
    , TestLabel "ssIdentifiesPrime" ssIdentifiesPrime
    , TestLabel "ssIdentifiesComposite" ssIdentifiesComposite
    ]
