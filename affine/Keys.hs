module Keys 
( sortedSolutions
, bestSolution
) where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Affine as Affine
import Utils
import Math
import Stats


-- constants: matches can be adjusted to try more or less options

matches = 26
testers = take matches "etaoinsrhdlucmfywgpbvkxqjz"


-- getting plaintext/ciphertext letter pairs

mostFrequent :: String -> String
mostFrequent string = map (\(count, char) -> char) . sortByCount . bucket $ string
    where
        criteria (c1, _) (c2, _)
            | c1 > c2   = LT
            | c1 == c2  = EQ
            | otherwise = GT
        sortByCount = List.sortBy criteria

pairs :: String -> [(Char, Char)]
pairs string = [(plain, cipher) | plain <- testers, cipher <- take matches . mostFrequent $ string]

pairpairs :: String -> [((Char, Char), (Char, Char))]
pairpairs string = filter valid . choose2 . pairs $ string
    where
        valid ((p1, c1), (p2, c2)) = 
            let dinv = invMod 26 ((toCode p1) - (toCode p2))
                a = (dinv * ((toCode c1) - (toCode c2))) `mod` 26
            in  if dinv == 0 
                    then False
                else if relativelyPrime 26 a
                    then True
                else 
                    False
        choose2 [] = []
        choose2 (x:xs) = map (\y -> (x, y)) xs ++ choose2 xs


-- getting and ranking solutions

impliedKeys :: (Char, Char) -> (Char, Char) -> (Int, Int)
impliedKeys (p1, c1) (p2, c2) = 
    let [p1', c1', p2', c2'] = map toCode [p1, c1, p2, c2]
        dinv = invMod 26 (p1' - p2')
        a = (dinv * (c1' -  c2')) `mod` 26
        b = (dinv * (p1'*c2' - p2'*c1')) `mod` 26
    in (a, b)

keyOptions :: String -> [(Int, Int)]
keyOptions string = Set.toList . Set.fromList . map (uncurry impliedKeys) . pairpairs $ string

solutions :: String -> [((Int, Int), String)]
solutions string = map appendDecryption . keyOptions $ string
    where
        appendDecryption (a, b) = ((a, b), Affine.decrypt a b string)

sortedSolutions :: String -> [((Int, Int), String)]
sortedSolutions string = List.sortBy lessBad . solutions $ string
    where
        lessBad (_, text1) (_, text2) = compare (badness text1) (badness text2)

bestSolution :: String -> ((Int, Int), String)
bestSolution string = head . sortedSolutions $ string
