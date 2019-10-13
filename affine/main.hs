import qualified Data.Char as Char
import Keys

main = interact breakSolveReassemble
    where
    breakSolveReassemble string = unlines . map handle . lines $ string

isValid :: String -> Bool
isValid string = foldl (&&) True . map (validChar) $ string
    where
        validChar c = (Char.isLetter c) || (c == ' ')

handle :: String -> String
handle string = 
    if isValid string
        then formatResults . bestSolution . prep $ string
    else
        "Error: Invalid character in input string. All input must be alphabetic or space."
    where
        prep string = filter (`elem` ['a'..'z']) . map Char.toLower $ string
        formatResults ((a, b), solution) =
            "Ciphertext: " ++ string ++ 
            "\nKeys: " ++ (show a) ++ ", " ++ (show b) ++
            "\nPlaintext: " ++ solution ++ "\n"
