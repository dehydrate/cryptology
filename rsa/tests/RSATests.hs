module RSATests where

import Math
import Test.HUnit
import System.Random
import WuRSA
import Primes
import WuEncoding

testRange :: Range
testRange = (10^10, 10^20) -- in practice, bigger numbers

generatesPrimes = TestCase $ assertBool
    "attempt to generate a prime number"
    (solovayStrassenTest (mkStdGen 42) 
        (fst $ generatePrime testRange (mkStdGen 42)))

generatesTwoPrimes = TestCase $ assertBool
    "generates two different primes"
    (
        let (p, q) = fst $ generatePrimes testRange (mkStdGen 42)
        in  p /= q
    )

generatesDistinctPairs = TestCase $ assertBool
    "pairs of primes p, q are not repeated"
    (
        let (pair1, gen) = generatePrimes testRange (mkStdGen 42)
            (pair2, _)   = generatePrimes testRange gen
        in  pair1 /= pair2
    )

generatesKey = TestCase $ assertBool
    "full key is correct"
    (
        let (fullkey, _)    = generateKey testRange (mkStdGen 42)
            (priv, pub)     = fullkey
            ((p, q), n, d)  = priv
            (m, e)          = pub
        in and $ 
            [ solovayStrassenTest (mkStdGen 42) p
            , solovayStrassenTest (mkStdGen 42) q
            , m == p*q
            , n == (p-1)*(q-1)
            , relativelyPrime e n
            , (d*e) `mod` n == 1
            , e < n
            , d < n 
            ]
    )

generatesDistinctKeys = TestCase $ assertBool
    "generates different keys from different sources of entropy"
    (
        let (key1, gen) = generateKey testRange (mkStdGen 42)
            (key2, _)   = generateKey testRange gen
        in key1 /= key2
    )

decryptionUndoesEncryptionA = TestCase $ assertEqual
    "'code' encrypted, then decrypted, is 'code'"
    (Just "code")
    (cipher >>= decryptA (priv, pub))
    where
        ((priv, pub), _) = generateKey testRange (mkStdGen 42)
        cipher = encryptA pub "code"

encryptionUndoesDecryptionA = TestCase $ assertEqual
    "'code' decrypted, then encrypted, is 'code'"
    (Just "code")
    (plain >>= encryptA pub)
    where
        ((priv, pub), _) = generateKey testRange (mkStdGen 13)
        plain = decryptA (priv, pub) "code"

decryptionUndoesEncryptionB = TestCase $ assertEqual
    "'code' encrypted, then decrypted, is 'code'"
    (Just "code")
    (cipher >>= decryptB (priv, pub))
    where
        ((priv, pub), _) = generateKey testRange (mkStdGen 42)
        cipher = encryptB pub "code"

encryptionUndoesDecryptionB = TestCase $ assertEqual
    "'code' decrypted, then encrypted, is 'code'"
    (Just "code")
    (plain >>= encryptB pub)
    where
        ((priv, pub), _) = generateKey testRange (mkStdGen 13)
        plain = decryptB (priv, pub) "code"

encryptionChangesA = TestCase $ assertBool
    "for 'code'"
    (encryptA pub "code" /= Just "code")
    where
        ((_, pub), _) = generateKey testRange (mkStdGen 27)

encryptionChangesB = TestCase $ assertBool
    "for 'code'"
    (encryptB pub "code" /= Just "code")
    where
        ((_, pub), _) = generateKey testRange (mkStdGen 7)

decryptionChangesA = TestCase $ assertBool
    "for 'code'"
    (decryptA full "code" /= Just "code")
    where
        (full, _) = generateKey testRange (mkStdGen 1001)

decryptionChangesB = TestCase $ assertBool
    "for 'code'"
    (decryptB full "code" /= Just "code")
    where
        (full, _) = generateKey testRange (mkStdGen 987654321)

detectsTooSmallKey1 = TestCase $ assertEqual
    "for small m, cannot encrypt 'code'"
    Nothing
    (encryptB (m, e) "code")
    where
        (p, q) = (17, 23)
        (m, n) = (p*q, (p-1)*(q-1))
        (e, _) = randomRelPrime (mkStdGen 100) n

detectsTooSmallKey2 = TestCase $ assertEqual
    "for small m, cannot encrypt 'code'"
    Nothing
    (decryptA (priv, pub) "code")
    where
        (p, q)  = (17, 23)
        (m, n)  = (p*q, (p-1)*(q-1))
        (e, _)  = randomRelPrime (mkStdGen 100) n
        d       = posInvMod n e
        priv    = ((p, q), n, d)
        pub     = (m, e)

tests = 
    [ TestLabel "generatesPrime" generatesPrimes
    , TestLabel "generatesTwoPrimes" generatesTwoPrimes
    , TestLabel "generatesDistinctPairs" generatesDistinctPairs
    , TestLabel "generatesKey" generatesKey
    , TestLabel "genreatesDistinctKeys" generatesDistinctKeys
    , TestLabel "decryptEncryptA" decryptionUndoesEncryptionA
    , TestLabel "decryptEncryptB" decryptionUndoesEncryptionB
    , TestLabel "encryptDecryptA" encryptionUndoesDecryptionA
    , TestLabel "encryptDecryptB" encryptionUndoesDecryptionB
    , TestLabel "encryptionChangesA" encryptionChangesA
    , TestLabel "encryptionChangesB" encryptionChangesB
    , TestLabel "decryptionChangesA" decryptionChangesA
    , TestLabel "decryptionChangesB" decryptionChangesB
    , TestLabel "detectsTooSmallKey1" detectsTooSmallKey1
    , TestLabel "detectsTooSmallKey2" detectsTooSmallKey2
    ]
