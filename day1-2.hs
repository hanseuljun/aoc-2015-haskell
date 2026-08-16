main :: IO ()
main = do
    contents <- readFile "input1.txt"
    let deltas = map (\x -> if x == '(' then 1 else -1) contents
    let floors = scanl (+) 0 deltas
    let overgroundFloors = takeWhile (>= 0) floors
    print (length overgroundFloors)
