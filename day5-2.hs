import Data.List (isInfixOf)
import qualified Data.Map as Map

main :: IO ()

pairs :: String -> [String]
pairs s =
  let tuples = zip s (drop 1 s) :: [(Char, Char)]
  in map (\x -> [fst x, snd x]) tuples

positions :: Ord a => [a] -> Map.Map a [Int]
positions xs = Map.fromListWith (++) [(x, [i]) | (x, i) <- zip xs [0..]]

main = do
  content <- readFile "input5.txt"
  let words = lines content
      word = words !! 0
      wordPairs = pairs word
      wordPairPositions = positions wordPairs
      wordPairPositionValues = Map.elems wordPairPositions
  print wordPairPositionValues
