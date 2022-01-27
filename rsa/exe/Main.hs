module Main where

import qualified Data.Either     as E
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as M
import System.Environment
import System.Random
import WuRSA
import Parse
import Math
import Primes
import StringLib

main :: IO ()
main = do
    argv <- getArgs
    rand <- getStdGen
    let argm = parse argv in
        putStrLn $ process rand argm

process :: StdGen -> Either String Argmap -> String
process _ (Left s) = "Error: " ++ s
process g (Right m)
    | M.isNothing mode              = "Error: no mode given"
    | mode == Just (Mode Encrypt)   = encryption m
    | mode == Just (Mode Decrypt)   = decryption m
    | mode == Just (Mode Generate)  = generate g m
    | mode == Just (Mode Validate)  = validate g m
    where mode = Map.lookup M m

encryption :: Argmap -> String
encryption map
    | M.isNothing text          = "Error: no plaintext given"
    | M.isNothing key           = "Error: no key given"
    | not $ isValid t           = "Error: invalid characters in plaintext"
    | encoding == (Encoding 1)  = encipher $ encryptA (m, e)
    | otherwise                 = encipher $ encryptB (m, e)
    where
        encoding                = Map.findWithDefault (Encoding 0) E map
        text                    = Map.lookup T map
        Just (Text t)           = text
        key                     = Map.lookup Pub map
        Just (PublicKey m e)    = key
        encipher f              = M.fromMaybe "Error: key too small" $ f (prep t)

decryption :: Argmap -> String
decryption map
    | M.isNothing text          = "Error: no plaintext given"
    | M.isNothing key           = "Error: no key given"
    | invalid                   = "Error: invalid key"
    | not $ isValid t           = "Error: invalid characters in plaintext"
    | encoding == (Encoding 1)  = decipher $ decryptA k
    | otherwise                 = decipher $ decryptB k
    where
        encoding                = Map.findWithDefault (Encoding 0) E map
        text                    = Map.lookup T map
        Just (Text t)           = text
        key                     = Map.lookup Priv map
        Just (PrivateKey p q e) = key
        decipher f              = M.fromMaybe "Error: key too small" $ f (prep t)
        m = p * q
        n = (p-1) * (q-1)
        d = posInvMod n e
        k = (((p, q), n, d), (m, e))
        invalid = not $ d `relativelyPrime` n

generate :: StdGen -> Argmap -> String
generate g map = presentPretty . fst $ generateKey range g
    where
        presentPretty (((p, q), n, d), (m, e)) =
            "PRIVATE:" ++ 
            "\np: " ++ show p ++
            "\nq: " ++ show q ++
            "\nn: " ++ show n ++
            "\nd: " ++ show d ++
            "\n\nPUBLIC:" ++
            "\nm: " ++ show m ++
            "\ne: " ++ show e
        size = Map.lookup S map
        Size s = M.fromMaybe (Size 50) size
        range = (10^s, 10^(s+10))

validate :: StdGen -> Argmap -> String
validate g map
    | M.isNothing priv  = "Error: no private key given"
    | fails p = "Not prime: " ++ show p
    | fails q = "Not prime: " ++ show q
    | not $ e `relativelyPrime` n
        = "(p-1)*(q-1) is not relatively prime with " ++ show e
    | otherwise = "Key is valid!"
    where
        priv                    = Map.lookup Priv map
        Just (PrivateKey p q e) = priv
        fails                   = not . solovayStrassenTest g
        n                       = (p-1) * (q-1)
