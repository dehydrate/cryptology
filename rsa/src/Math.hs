module Math 
( invMod
, posInvMod
, divides
, relativelyPrime
, modularExponent
) where

posInvMod :: Integer -> Integer -> Integer
posInvMod modulus b
    | inv < 0   = inv + modulus
    | otherwise = inv
    where
        inv = invMod modulus b

-- 0 if there is no inverse
invMod :: Integer -> Integer -> Integer
invMod modulus b 
    | b < 0     = invMod modulus (b+modulus)
    | b == 0    = 0
    | b == 1    = 1
    | otherwise = invMod' modulus b 1 0

invMod' :: Integer -> Integer -> Integer -> Integer -> Integer
invMod' a b x1 x0
    | r == 1    = x
    | r == 0    = 0
    | otherwise = invMod' b r x x1
    where 
        (q, r) = a `quotRem` b
        x = -q*x1 + x0 

relativelyPrime a b = gcd a b == 1

divides :: Integral a => a -> a -> Bool
divides b a = a `mod` b == 0

modularExponent :: (Integral a) => a -> a -> a -> a
modularExponent base power modulus =
    foldl f 1 (reverse $ bitstring power)
    where
        f c 0 = c^2 `mod` modulus
        f c 1 = ((c^2 `mod` modulus) * base) `mod` modulus

-- reverses digit order: most significant bit is on the right
bitstring :: (Integral a) => a -> [a]
bitstring 0 = []
bitstring x = 
    let (q, r) = x `quotRem` 2
    in  r : bitstring q
