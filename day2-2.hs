import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "input2.txt"
    let ls = lines contents
        dimensions = map (map read . splitOn "x") ls :: [[Int]]
    let ribbonLengths = map (\x -> 2 * ((sum x) - (maximum x)) + (product x)) dimensions
    print (sum ribbonLengths)
