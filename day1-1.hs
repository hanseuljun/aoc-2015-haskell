main :: IO ()
main = do
    contents <- readFile "input1.txt"
    let nums = map (\x -> if x == '(' then 1 else -1) contents
    let sum = foldl (+) 0 nums
    print sum
