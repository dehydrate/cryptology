import Utils
import Keys 
import qualified Vigenere as Vigenere



main = do
    ciphertext <- getLine
    if isValid ciphertext
        then 
            let (len, key, plaintext) = crack 9 ciphertext -- try keys up to length 9
            in putStrLn (
                    "Ciphertext: " ++ ciphertext ++ 
                    "\nKey: " ++ key ++ 
                    "\nPlaintext: " ++ plaintext ++ 
                    "\n"
                )
        else
            putStrLn ("Invalid character in plaintext\n")



--- known keylength attacks

-- takes the key length and ciphertext to return the most likely key
getKey :: Int -> String -> String
getKey n string = bestKey . keySolutions n . prep $ string

-- gets key and decrypts, given key size
crackKnownKeylen :: Int -> String -> (String, String)
crackKnownKeylen n string = (key, plaintext) where
    key = getKey n string
    plaintext = Vigenere.decrypt key . prep $ string


--- unknown keylength attack

-- takes a max key length to try and ciphertext; returns best tuple of
-- (keylength, key, plaintext)
crack :: Int -> String -> (Int, String, String)
crack max string =
    let prepped = prep string
        key = tryVariousKeylengths max prepped
        len = length key
        plaintext = Vigenere.decrypt key prepped
    in (len, key, plaintext)
