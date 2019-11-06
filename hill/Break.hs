module Break (bestKeys) where

import Hill
import StringLib
import Stats
import qualified Data.Maybe as M
import qualified Data.List as L

type CipherFragment = (String, Alignment)

-- known plaintext/known ciphertext attack to get the encryption key
getKey :: CipherFragment -> String -> Maybe AlignedKey
getKey ct pt
    | not singleton = Nothing
    | fails         = Nothing
    | otherwise     = Just (head keys, align)
    where
        (ct', align)    = ct
        pairs           = zip (block $ toVector pt) (block $ toVector ct')
        pairpairs       = [ (p1, p2) | p1 <- pairs, p2 <- pairs ]
        keys'           = M.catMaybes $ map (uncurry key) pairpairs
        keys            = L.nub keys'
        -- these both catch a few cases where the text wasn't a proper match
        fails           = (not . validKey) (head keys)
        singleton       = length keys == 1

toColumns :: Key -> [Vector]
toColumns [a, b, c, d] = [[a, c], [b, d]]

uncolumns :: [Vector] -> Key
uncolumns [[a, c], [b, d]] = [a, b, c, d]

key :: (Vector, Vector) -> (Vector, Vector) -> Maybe Key
key (p1, c1) (p2, c2) =
    let plain   = uncolumns [p1, p2]
        cipher  = uncolumns [c1, c2]
        pinv    = matrixInverse plain
    in fmap (uncolumns . leftMultiplyMatrix cipher . toColumns) pinv


block :: [a] -> [[a]]
block [] = []
block [_] = []
block (a:b:rest) = [a,b] : block rest

-- in the event that keyOptions finds multiple possible keys,
-- sort them in order of likelihood using good old badness
bestKeys :: String -> String -> [AlignedKey]
bestKeys ciphertext plainfrag =
    L.sortBy (criteria) $ keyOptions ciphertext plainfrag
    where
        decrypt' (k, a) = M.fromJust $ alignedDecrypt a k ciphertext
        criteria k1 k2
            | badness (decrypt' k1) < badness (decrypt' k2) = LT
            | badness (decrypt' k2) < badness (decrypt' k1) = GT
            | otherwise                                     = EQ

-- A key is plausible if, when you use it to decrypt the ciphertext, you get basically the plaintext
-- back. There are two places to check for this: here and in getKey.
--
-- It's a little faster computation-wise to do it in getKey, because you just use the head of keys' to
-- decrypt the CipherFragment, then compare it to the plaintext fragment: if they match, add it to the
-- list of options. This is guaranteed to catch the right key at some point. But it also lets a few 
-- more answers through, because you have to give the last letter of the key a pass if the plaintext 
-- fragment is of odd length. If instead you check for this property in keyOptions, you eliminate more 
-- keys that don't have the plaintext fragment as an infix of the ciphertext after decryption.
keyOptions :: String -> String -> [AlignedKey]
keyOptions ct pf =
    let evenKeys        = M.catMaybes $ zipWith getKey (candidates ct pf) (repeat pf) -- pf is even aligned
        oddKeys         = M.catMaybes $ zipWith getKey (candidates ct $ tail pf) (repeat $ tail pf) -- pf is odd aligned
        decrypt' (k, a) = M.fromJust $ alignedDecrypt a k ct
    in L.nub $
       [ k | k <- evenKeys, pf `L.isInfixOf` (decrypt' k) ] ++
       [ k | k <- oddKeys, (tail pf) `L.isInfixOf` (decrypt' k) ] -- we need the two different filters because an
                                                                  -- odd aligned fragment from the front of the 
                                                                  -- plaintext will have a fundamentally 
                                                                  -- unrecoverable first letter. If we don't give
                                                                  -- the first letter of an odd-aligned pf a free
                                                                  -- pass, we miss this special case

-- finds every substring in the ciphertext that could be matched to the plaintext, while remembering its alignment
candidates :: String -> String -> [CipherFragment]
candidates ciphertext plainfrag = 
    let l           = length plainfrag 
        substrings  = M.catMaybes $ walk (maybetake l) ciphertext
    in zip substrings (cycle [Even, Odd])

-- friend to scan: walk through a list while applying f to what's left at each step
walk :: ([a] -> b) -> [a] -> [b]
walk f []   = []
walk f list = f list : walk f (tail list)

-- like take, but returns Nothing if you ask for more elements than a list contains
maybetake :: Int -> [a] -> Maybe [a]
maybetake 0 _       = Just []
maybetake _ []      = Nothing
maybetake n (x:xs)  = (x:) <$> maybetake (n-1) xs
