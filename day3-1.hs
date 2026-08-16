import qualified Data.Map as Map

charToDirection :: Char -> (Int, Int)
charToDirection '^' = (0, 1)
charToDirection 'v' = (0, -1)
charToDirection '>' = (1, 0)
charToDirection '<' = (-1, 0)
charToDirection _ = (0, 0)

count :: Ord a => [a] -> Map.Map a Int
count xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
    contents <- readFile "input3.txt"
    let directions = map (charToDirection) contents
        positions = scanl (\x y -> (fst x + fst y, snd x + snd y)) (0, 0) directions
        counts = count positions
    print (length counts)
