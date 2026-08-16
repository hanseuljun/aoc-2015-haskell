import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "input2.txt"
    let ls = lines contents
    -- let dimensions = map (\x -> splitOn "x" x) ls
    let dimensions = map (\x -> map read (splitOn "x" x)) ls :: [[Int]]
    let areas = map (\x -> [x !! 0 * x !! 1, x !! 1 * x !! 2, x !! 2 * x !! 0]) dimensions
    let areaSums = map (\x -> 2 * (sum x) + (minimum x)) areas
    print (sum areaSums)
