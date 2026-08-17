import Data.List (isInfixOf)

main :: IO ()

hasThreeVowels :: String -> Bool
hasThreeVowels s = length (filter (`elem` "aeiou") s) >= 3

pairs :: String -> [(Char, Char)]
pairs s = zip s (drop 1 s)

containsConsecutiveChar :: String -> Bool
containsConsecutiveChar s = any (\x -> fst x == snd x) (pairs s)

containsSpecificStrings :: String -> Bool
containsSpecificStrings s
    | "ab" `isInfixOf` s = True
    | "cd" `isInfixOf` s = True
    | "pq" `isInfixOf` s = True
    | "xy" `isInfixOf` s = True
    | otherwise = False

main = do
    content <- readFile "input5.txt"
    let words = lines content
        wordsWithVowels = filter (hasThreeVowels) words
        wordsWithConsecutiveChars = filter (containsConsecutiveChar) wordsWithVowels
        wordsWithSpecficStrings = filter (not . containsSpecificStrings) wordsWithConsecutiveChars
    print (length wordsWithSpecficStrings)
