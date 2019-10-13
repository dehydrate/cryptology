module Math 
( invMod
, relativelyPrime
) where

-- 0 if there is no inverse
invMod :: Int -> Int -> Int
invMod modulus b = 
    if b == 1 then 1
    else if b == 0 then 0
    else invMod' modulus b 1 0

invMod' :: Int -> Int -> Int -> Int -> Int
invMod' a b x1 x0 =
    let q = a `quot` b
        r = a `mod` b
        x = -q*x1 + x0 
    in
        if r == 1 then x
        else if r == 0 then 0
        else invMod' b r x x1 

relativelyPrime a b = gcd a b == 1
