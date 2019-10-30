module Break (bestKeys) where

import Hill
import StringLib
import Stats
import qualified Data.Maybe as Maybe
import qualified Data.List as List

type CipherFragment = (String, Alignment)

-- If the list [a, b, c, d] represents the matrix
--      (a b)
--      (c d)
-- then the columns representation of this matrix is [[a, c], [b, d]], 
-- which makes the code below somewhat confusing.

-- known plaintext/known ciphertext attack to get the encryption key
getKey :: CipherFragment -> String -> Maybe AlignedKey
getKey ct pt
    | fails             = Nothing
    | singleton keys    = Just (head keys, align)
    | otherwise         = Nothing
    where
        (ct', align)    = ct
        pairs           = zip (block $ toVector pt) (block $ toVector ct')
        pairpairs       = [ (p1, p2) | p1 <- pairs, p2 <- pairs ]
        keys'           = Maybe.catMaybes $ map (uncurry key) pairpairs
        keys            = List.nub keys'
        -- these both catch a few cases where the alignment was wrong
        fails           = any (not . validKey) keys
        singleton l     = length l == 1

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

-- in the unlikely event that keyOptions finds multiple possible keys,
-- bestKeys sorts them in order of likelihood using good old badness
bestKeys :: String -> String -> [AlignedKey]
bestKeys ciphertext plainfrag =
    List.sortBy (criteria) $ keyOptions ciphertext plainfrag
    where
        decrypt' (k, a) = Maybe.fromJust $ alignedDecrypt a k ciphertext
        criteria k1 k2
            | badness (decrypt' k1) < badness (decrypt' k2) = LT
            | badness (decrypt' k2) < badness (decrypt' k1) = GT
            | otherwise                                     = EQ

keyOptions :: String -> String -> [AlignedKey]
keyOptions ciphertext plainfrag =
    let keys            = Maybe.catMaybes $ zipWith getKey (candidates ciphertext plainfrag) (repeat plainfrag)
        decrypt' (k, a) = Maybe.fromJust $ alignedDecrypt a k ciphertext
    in filter (\ k -> List.isInfixOf plainfrag (decrypt' k)) keys

-- finds every substring in the ciphertext that could be matched to the plaintext
candidates :: String -> String -> [CipherFragment]
candidates ciphertext plainfrag = 
    let l           = length plainfrag 
        substrings  = Maybe.catMaybes . walk (maybetake l) $ ciphertext
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
