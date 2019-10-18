import Break
import Parse
import Hill
import System.Environment
import StringLib
import qualified Text.Read as Text
import qualified Data.Maybe as Maybe


main = do
    args <- getArgs
    let (m, rest)           = extractMode args 
        (k, rest')          = extractKey rest
        (t, input)          = extractTexts rest'
        mode                = readMode m
        key                 = readKey k
        texts               = readTexts t
        (argsValid, reason) = validInput mode key texts input
    if not $ validText (input ++ texts)
        then putStrLn "Invalid characters in input texts"
    else if argsValid
        then case (Maybe.fromJust mode) of
            "encrypt"   -> putStrLn $ cautiousCipher encrypt (Maybe.fromJust key) (prep $ head input) 
            "decrypt"   -> putStrLn $ cautiousCipher decrypt (Maybe.fromJust key) (prep $ head input)
            "crack"     -> putStrLn $ cautiousAttack (map prep texts)
    else
        putStrLn reason



cautiousAttack :: [String] -> String
cautiousAttack [cipher, plain]
    | Maybe.isNothing result    = "Not enough text to determine key"
    | otherwise                 = "Key:\n" ++ (prettyKey $ Maybe.fromJust result)
    where result = getKey cipher plain

cautiousCipher :: (Key -> String -> Maybe String) -> Key -> String -> String
cautiousCipher f key text
    | Maybe.isNothing result    = "Invalid key"
    | otherwise                 = Maybe.fromJust result
    where result = f key text



type JustifiedBool = (Bool, String)

validInput :: Maybe String -> Maybe Key -> [String] -> [String] -> JustifiedBool
validInput Nothing _ _ _                        = (False, "Mode input error")
validInput (Just "crack") _ texts input 
    | length texts == 2 && length input == 0    = (True, "")
    | otherwise                                 = (False, "Input text error")
validInput _ Nothing _ _                        = (False, "Key input error")
validInput _ _ texts input
    | length texts == 2 && length input == 0    = (True, "")
    | length texts == 0 && length input == 1    = (True, "")
    | otherwise                                 = (False, "Input text error")

validText :: [String] -> Bool
validText = all isValid



extractKey :: [String] -> ([String], [String])
extractKey = extractInfix "-k" 4

readKey :: [String] -> Maybe Key
readKey strings
    | length justs /= 4  = Nothing
    | otherwise             = Just justs
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
