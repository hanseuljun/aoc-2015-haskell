main :: IO ()

charToDirection :: Char -> (Int, Int)
charToDirection '^' = (0, 1)
charToDirection 'v' = (0, -1)
charToDirection '>' = (1, 0)
charToDirection '<' = (-1, 0)
charToDirection _ = (0, 0)

main = do
    contents <- readFile "input3.txt"
    let directions = map (charToDirection) contents
        positions = scanl (\x y -> (fst x + fst y, snd x + snd y)) (0, 0) directions
    print positions
