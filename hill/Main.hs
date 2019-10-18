import Break
import Parse
import Hill
import System.Environment
import qualified Text.Read as Text
import qualified Data.Maybe as Maybe

main = do
    args <- getArgs
    let (m, rest) = extractMode args 
        (k, rest') = extractKey rest
        (t, input) = extractTexts rest'
        mode = readMode m
        key = readKey k
        texts = readTexts t
        (valid, reason) = validInput mode key texts input
    if valid
        then case (Maybe.fromJust mode) of
            -- need a more sophisticated way to report results
            "encrypt"   -> putStrLn $ cautiousCipher encrypt (Maybe.fromJust key) (head input) 
            "decrypt"   -> putStrLn $ cautiousCipher decrypt (Maybe.fromJust key) (head input)
            "crack"     -> putStrLn $ cautiousSolve texts
    else
        putStrLn reason

cautiousSolve :: [String] -> String
cautiousSolve [cipher, plain]
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
validInput Nothing _ _ _ = (False, "Mode input error")
validInput (Just "crack") _ texts input 
    | length texts == 2 && length input == 0    = (True, "")
    | otherwise                                 = (False, "Input text error")
validInput _ Nothing _ _ = (False, "Key input error")
validInput _ _ texts input
    | length texts == 2 && length input == 0    = (True, "")
    | length texts == 0 && length input == 1    = (True, "")
    | otherwise                                 = (False, "Input text error")

extractKey :: [String] -> ([String], [String])
extractKey = extractInfix "-k" 4

-- takes a list such as ["-k", "1", "2", "3", "5"] and reads it as a Maybe key
readKey :: [String] -> Maybe Key
readKey [] = Nothing
readKey strings
    | length valids /= 4    = Nothing
    | otherwise             = Just valids
    where
    maybeints = map Text.readMaybe (tail strings) :: [Maybe Int]
    valids = Maybe.catMaybes maybeints

extractMode :: [String] -> ([String], [String])
extractMode = extractInfix "-m" 1

readMode :: [String] -> Maybe String
readMode [_, mode]
    | mode `elem` ["encrypt", "decrypt", "crack"]   = Just mode
    | otherwise                                     = Nothing

extractTexts :: [String] -> ([String], [String])
extractTexts = extractInfix "-t" 2

readTexts :: [String] -> [String]
readTexts [] = []
readTexts list = tail list
