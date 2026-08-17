import Data.List (isInfixOf, sort)
import qualified Data.Map as Map

main :: IO ()

pairs :: [a] -> [[a]]
pairs xs =
  let tuples = zip xs (drop 1 xs)
  in map (\x -> [fst x, snd x]) tuples

positions :: Ord a => [a] -> Map.Map a [Int]
positions xs = Map.fromListWith (++) [(x, [i]) | (x, i) <- zip xs [0..]]

containsPairPositions :: [[Int]] -> Bool
containsPairPositions positions = any (\x -> (maximum x - minimum x) > 1) positions

containsPairs :: String -> Bool
containsPairs word =
  let wordPairs = pairs word
      wordPairPositions = positions wordPairs
  in (containsPairPositions (Map.elems wordPairPositions))

main = do
  -- content <- readFile "input5.txt"
  -- let words = lines content
  let word = "qjhvhtzxzqqjkmpb"
      charPositions = positions word
      sortedCharPositions = map sort (Map.elems charPositions)
      sortedCharPositionPairs = map pairs sortedCharPositions
  print (sortedCharPositions)
  print (sortedCharPositionPairs)
