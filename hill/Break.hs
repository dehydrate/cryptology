module Break (getKey) where

import Hill
import StringLib
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
        results = Maybe.catMaybes results'

    in if not (null results)
        then Just (head results)    -- all the Just values in results should be identical
        else Nothing

block :: [a] -> [[a]]
block [] = []
block [_] = []
block (a:b:rest) = [a,b] : block rest
