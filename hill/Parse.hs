module Parse (extractInfix) where

splitAfter :: (Eq a) => a -> Int -> [a] -> ([a], [a], [a])
splitAfter c n [] = ([], [], [])
splitAfter c n (x:xs)
    | x == c    = let (front, back) = split n xs in ([], x:front, back)
    | otherwise = let (front, mid, back) = splitAfter c n (xs) in (x:front, mid, back)

split :: (Eq a) => Int -> [a] -> ([a], [a])
split 0 list = ([], list)
split _ [] = ([], [])
split n (x:xs) = (x:front, back)
    where (front, back) = split (n-1) xs

-- removes an element and the next n elements after it from a list
-- returns the infix plus the list without the infix
-- e.g. extractInfix 'c' 3 "abcdefghij" = ("cdef", "abghij")
extractInfix :: (Eq a) => a -> Int -> [a] -> ([a], [a])
extractInfix c n list = (mid, front ++ back)
    where (front, mid, back) = splitAfter c n list
