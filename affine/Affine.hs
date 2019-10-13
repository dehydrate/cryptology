module Affine
( decrypt
, encrypt
) where

import Math
import Utils

encrypt :: Int -> Int -> String -> String
encrypt a b string = toString . map encrypt' . toVector $ string
    where encrypt' p = (a * p + b) `mod` 26

decrypt :: Int -> Int -> String -> String
decrypt a b string = toString . map decrypt' . toVector $ string
    where 
        ainv = invMod 26 a
        decrypt' c = (ainv * (c - b)) `mod` 26
