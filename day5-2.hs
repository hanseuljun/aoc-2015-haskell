import Data.List (isInfixOf, sort)
import qualified Data.Map as Map

main :: IO ()

-- list to list of pairs starting with (first element, second element)
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

positions :: Ord a => [a] -> Map.Map a [Int]
positions xs = Map.fromListWith (++) [(x, [i]) | (x, i) <- zip xs [0..]]

containsPairPositions :: [[Int]] -> Bool
containsPairPositions positions = any (\x -> (maximum x - minimum x) > 1) positions

containsPairs :: String -> Bool
containsPairs word =
  let wordPairs = pairs word
      wordPairPositions = positions wordPairs
  in containsPairPositions (Map.elems wordPairPositions)

containsCharPosition :: [Int] -> Bool
containsCharPosition positions = any (\x -> (x + 2) `elem` positions) positions

containsChars :: String -> Bool
containsChars word =
  let charPositionsMap = positions word :: Map.Map Char [Int]
      charPositions = Map.elems charPositionsMap
  in any containsCharPosition charPositions


main = do
  content <- readFile "input5.txt"
  let words = lines content
      wordsWithPairs = filter (containsPairs) words
      wordsWithChars = filter (containsChars) wordsWithPairs
  print (length wordsWithChars)
