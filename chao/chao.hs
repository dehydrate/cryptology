-- demonstration
main = do
    putStrLn "Encrypting \"welldoneisbetterthanwellsaid\":"
    putStrLn $ encrypt "welldoneisbetterthanwellsaid" testpt testct
    putStrLn "Decrypting \"oahqhcnynxtszjrrhjbyhqksoujy\":"
    putStrLn $ decrypt "oahqhcnynxtszjrrhjbyhqksoujy" testpt testct


-- constants
nadir = 14
testct = "hxuczvamdslkpefjrigtwobnyq"
testpt = "ptlnbqdeoysfavzkgjrihwxumc"

data LP a = LP [a] [a] deriving (Show, Eq)

-- I don't actually need this typeclass
class ListPair f where
    addFront :: a -> f a -> f a
    addBack :: a -> f a -> f a

instance ListPair LP where
    addFront elem (LP front back) = LP (elem:front) back
    addBack elem (LP front back) = LP front (elem:back)


cycleTo :: (Eq a) => a -> [a] -> Maybe (LP a)
cycleTo _ [] = Nothing
cycleTo elem (h:tail)
    | h == elem     = Just $ LP [] (h:tail)
    | otherwise     = fmap (addFront h) (cycleTo elem tail)

cyclePast :: (Eq a) => a -> [a] -> Maybe (LP a)
cyclePast _ [] = Nothing
cyclePast elem (h:tail)
    | h == elem     = Just $ LP [h] (tail)
    | otherwise     = fmap (addFront h) (cyclePast elem tail)

nSizedPrefix :: Int -> [a] -> LP a
nSizedPrefix 0 list = LP [] list
nSizedPrefix _ [] = LP [] []
nSizedPrefix i (h:tail) =
    let LP front back = nSizedPrefix (i-1) tail
    in LP (h:front) back

-- I'm not really using the Maybeness of cycleTo and cyclePast for the permutations
permuteCt :: (Eq a) => a -> [a] -> [a]
permuteCt key alphabet =
    let Just (LP back front) = cycleTo key alphabet
        cycledAlph = front ++ back
        LP (head:hole:tail) suffix = nSizedPrefix nadir cycledAlph
    in (head:tail) ++ (hole:suffix)

permutePt :: (Eq a) => a -> [a] -> [a]
permutePt key alphabet =
    let Just (LP back front) = cyclePast key alphabet
        cycledAlph =  front ++ back
        LP (head:next:hole:tail) suffix = nSizedPrefix nadir cycledAlph
    in (head:next:tail) ++ (hole:suffix)

lookupMatch :: (Eq a) => a -> [a] -> [a] -> Maybe a
lookupMatch _ [] [] = Nothing
lookupMatch elem (l:ltail) (r:rtail)
    | elem == l     = Just r
    | otherwise     = lookupMatch elem ltail rtail

encrypt :: (Eq a) => [a] -> [a] -> [a] -> [a]
encrypt [] _ _ = []
encrypt (p:plaintext) plainAlph cipherAlph =
    let Just c = lookupMatch p plainAlph cipherAlph
        encrypted = encrypt plaintext (permutePt p plainAlph) (permuteCt c cipherAlph)
    in c:encrypted

decrypt :: (Eq a) => [a] -> [a] -> [a] -> [a]
decrypt [] _ _ = []
decrypt(c:ciphertext) plainAlph cipherAlph =
    let Just p = lookupMatch c cipherAlph plainAlph
        decrypted = decrypt ciphertext (permutePt p plainAlph) (permuteCt c cipherAlph)
    in p:decrypted
