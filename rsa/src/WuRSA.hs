module WuRSA where

import Math
import Primes
import System.Random
import WuEncoding
import qualified Data.List.Split as LS

type PQ = (Integer, Integer)
type Range = (Integer, Integer)

-- these should probably be structured data
type PrivateKey = (PQ, Integer, Integer) -- ((p, q), n, d)
type PublicKey = (Integer, Integer) -- (m, e)
type FullKey = (PrivateKey, PublicKey)

generatePrime :: Range -> StdGen -> (Integer, StdGen)
generatePrime range g
    | solovayStrassenTest g n   = (n, gen)
    | otherwise                 = generatePrime range gen
    where
        (n, gen) = randomR range g

generatePrimes :: Range -> StdGen -> (PQ, StdGen)
generatePrimes range g =
    let (p, g')     = generatePrime range g
        (q, g'')    = generatePrime range g'
    in ((p, q), g'')

generateKey :: Range -> StdGen -> (FullKey, StdGen)
generateKey range g = ((priv, pub), g'')
    where
        -- randomly select primes
        ((p, q), g')    = generatePrimes range g
        -- m and n are deterministic
        m               = p*q
        n               = (p-1) * (q-1)
        -- randomly select e
        (e, g'')        = randomRelPrime g' n
        -- d is deterministic
        d               = posInvMod n e
        priv            = ((p, q), n, d)
        pub             = (m, e)

randomRelPrime :: StdGen -> Integer -> (Integer, StdGen)
randomRelPrime g n
    | relativelyPrime e n   = (e, g')
    | otherwise             = randomRelPrime g' n
    where
        (e, g') = randomR (2, n-1) g

-- A sets 'a' to 1
-- B sets 'a' to 0
-- if message is too big for key, return nothing
-- otherwise proceed
decryptA, decryptB :: FullKey -> String -> Maybe String
encryptA, encryptB :: PublicKey -> String -> Maybe String

decryptA (priv, pub) ciphertext
    | ciphernum > m = Nothing
    | otherwise     = Just plaintext
    where
        (m, _)      = pub
        (_, _, d)   = priv
        ciphernum   = encodeA ciphertext
        plainnum    = modularExponent ciphernum d m
        plaintext   = decodeA plainnum


decryptB (priv, pub) ciphertext
    | ciphernum > m = Nothing
    | otherwise     = Just plaintext
    where
        (m, _)      = pub
        (_, _, d)   = priv
        ciphernum   = encodeB ciphertext
        plainnum    = modularExponent ciphernum d m
        plaintext   = decodeB plainnum

encryptA (m, e) plaintext
    | plainnum > m  = Nothing
    | otherwise     = Just ciphertext
    where
        plainnum    = encodeA plaintext
        ciphernum   = modularExponent plainnum e m
        ciphertext  = decodeA ciphernum
    
encryptB (m, e) plaintext
    | plainnum > m  = Nothing
    | otherwise     = Just ciphertext
    where
        plainnum    = encodeB plaintext
        ciphernum   = modularExponent plainnum e m
        ciphertext  = decodeB ciphernum
