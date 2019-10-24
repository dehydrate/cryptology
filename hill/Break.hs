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
getKey ct pt = 
    let pairs = zip (block $ toVector ct) (block $ toVector pt)

        matrixPairs =
            [ ([c1, c3, c2, c4], matrixInverse [p1, p3, p2, p4]) | 
                  ([c1, c2], [p1, p2]) <- pairs 
                , ([c3, c4], [p3, p4]) <- pairs ]  
               
        toColumns [a, b, c, d] = [[a, c], [b, d]]
        uncolumns [[a, c], [b, d]] = [a, b, c, d]

        key (ct, pt) = leftMultiplyMatrix <$> pure ct <*> fmap toColumns pt

        results' = map (fmap uncolumns . key) matrixPairs
        results = filter validKey . Maybe.catMaybes $ results'

    in if not (null results)
        then Just (head results)    -- all the Just values in results should be identical
        else Nothing

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
