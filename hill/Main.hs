import Break
import Parse
import Hill
import System.Environment
import StringLib
import qualified Text.Read as Text
import qualified Data.Maybe as Maybe


-- not thrilled with this way of reading and validating input, but it'll do for now
main = do
    args <- getArgs
    let (m, rest)           = extractMode args 
        (k, rest')          = extractKey rest
        (t, input)          = extractTexts rest'
        mode                = readMode m
        key                 = readKey k
        texts               = readTexts t
        (argsValid, reason) = validInput mode key texts input
    if argsValid && validText (input ++ texts)
        then case (Maybe.fromJust mode) of
            "encrypt"   -> putStrLn $ cautiousCipher encrypt (Maybe.fromJust key) (prep $ head input) 
            "decrypt"   -> putStrLn $ cautiousCipher decrypt (Maybe.fromJust key) (prep $ head input)
            "crack"     -> attackInteract (prep $ head texts) (prep $ last texts)
    else do
        if not $ validText (input ++ texts)
            then putStrLn "Invalid characters in input text"
        else
            putStrLn reason
        return ()


-- not the most succinct approach, but it seems to take care of all the cases
attackInteract :: String -> String -> IO ()
attackInteract ciphertext plainfrag = inputCycle ciphertext (bestKeys ciphertext plainfrag)

inputCycle :: String -> [AlignedKey] -> IO ()
inputCycle ciphertext keys
    | null keys             = putStrLn "Not enough information to determine key"
    | length keys == 1      = printSolution (head keys)
    | otherwise             = printSolution (head keys) >> putStr "Continue y/n: " >> getLine >>= continue
    where
        continue choice
            | choice == "y"     = inputCycle ciphertext (tail keys)
            | otherwise         = return ()
        printSolution (key, alignment) = do
            putStrLn "Key:"
            putStrLn $ prettyKey key
            putStrLn "Plaintext:"
            putStrLn . Maybe.fromJust $ alignedDecrypt alignment key ciphertext

cautiousCipher :: (Key -> String -> Maybe String) -> Key -> String -> String
cautiousCipher f key text
    | Maybe.isNothing result    = "Invalid key"
    | otherwise                 = Maybe.fromJust result
    where result = f key text


-- input validation

type JustifiedBool = (Bool, String)

validInput :: Maybe String -> Maybe Key -> [String] -> [String] -> JustifiedBool
validInput Nothing _ _ _                        = (False, "Mode input error")
validInput (Just "crack") _ texts input 
    | length texts == 2 && length input == 0    = (True, "Peter Piper picked")
    | otherwise                                 = (False, "Input text error")
validInput _ Nothing _ _                        = (False, "Key input error")
validInput _ _ texts input
    | length texts == 0 && length input == 1    = (True, "a peck of pickled")
    | otherwise                                 = (False, "Input text error")

validText :: [String] -> Bool
validText = all isValid


-- input parsing

extractKey :: [String] -> ([String], [String])
extractKey = extractInfix "-k" 4

readKey :: [String] -> Maybe Key
readKey strings
    | null strings      = Nothing
    | length justs /= 4 = Nothing
    | otherwise         = Just justs
    where
        maybes  = map Text.readMaybe (tail strings) :: [Maybe Int]
        justs   = Maybe.catMaybes maybes

extractMode :: [String] -> ([String], [String])
extractMode = extractInfix "-m" 1

readMode :: [String] -> Maybe String
readMode [_, mode]
    | mode `elem` ["encrypt", "decrypt", "crack"]   = Just mode
    | otherwise                                     = Nothing
readMode _ = Nothing

extractTexts :: [String] -> ([String], [String])
extractTexts = extractInfix "-t" 2

readTexts :: [String] -> [String]
readTexts []    = []
readTexts list  = tail list
