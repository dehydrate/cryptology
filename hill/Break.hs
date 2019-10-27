module Break (bestKeys) where

import Hill
import StringLib
import Stats
import qualified Data.Maybe as Maybe
import qualified Data.List as List

-- If the list [a, b, c, d] represents the matrix
--      (a b)
--      (c d)
-- then the columns representation of this matrix is [[a, c], [b, d]], 
-- which makes the code below somewhat confusing.

-- known plaintext/known ciphertext attack to get the encryption key
getKey :: String -> String -> Maybe Key
getKey ct pt
    | unanimous keys    = Just . uncolumns . head $ keys
    | otherwise         = Nothing
    where
        pairs = zip (block $ toVector pt) (block $ toVector ct)
        pairpairs = [ (p1, p2) | p1 <- pairs, p2 <- pairs ]
        keys = Maybe.catMaybes $ map (uncurry key) pairpairs
        unanimous l = length (List.nub l) == 1

toColumns :: Key -> [Vector]
toColumns [a, b, c, d] = [[a, c], [b, d]]

uncolumns :: [Vector] -> Key
uncolumns [[a, c], [b, d]] = [a, b, c, d]

key :: (Vector, Vector) -> (Vector, Vector) -> Maybe [Vector]
key (p1, c1) (p2, c2) =
    let plain   = uncolumns [p1, p2]
        cipher  = uncolumns [c1, c2]
        pinv    = matrixInverse plain
    in pinv >>= return . toColumns >>= return . leftMultiplyMatrix cipher


block :: [a] -> [[a]]
block [] = []
block [_] = []
block (a:b:rest) = [a,b] : block rest

bestKeys :: String -> String -> [Key]
bestKeys ciphertext plainfrag =
    List.sortBy (criteria) $ keyOptions ciphertext plainfrag
    where
        decrypt' k = Maybe.fromJust $ decrypt k ciphertext
        criteria k1 k2
            | badness (decrypt' k1) < badness (decrypt' k2) = LT
            | badness (decrypt' k2) < badness (decrypt' k1) = GT
            | otherwise                                     = EQ
    
keyOptions :: String -> String -> [Key]
keyOptions ciphertext plainfrag =
    Maybe.catMaybes $ zipWith getKey (candidates ciphertext plainfrag) (repeat plainfrag)

candidates :: String -> String -> [String]
candidates whole fragment = 
    let l = length fragment 
    in Maybe.catMaybes . walk (maybetake l) $ whole

walk :: ([a] -> b) -> [a] -> [b]
walk f []   = []
walk f list = f list : walk f (tail list)

maybetake :: Int -> [a] -> Maybe [a]
maybetake 0 _       = Just []
maybetake _ []      = Nothing
maybetake n (x:xs)  = (x:) <$> maybetake (n-1) xs
