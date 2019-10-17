module Math 
( invMod
, relativelyPrime
) where

-- 0 if there is no inverse
invMod :: Int -> Int -> Int
invMod modulus b 
    | b < 0     = invMod modulus (b+modulus)
    | b == 0    = 0
    | b == 1    = 1
    | otherwise = invMod' modulus b 1 0

invMod' :: Int -> Int -> Int -> Int -> Int
invMod' a b x1 x0
    | r == 1    = x
    | r == 0    = 0
    | otherwise = invMod' b r x x1
    where 
        (q, r) = a `quotRem` b
        x = -q*x1 + x0 

relativelyPrime a b = gcd a b == 1
