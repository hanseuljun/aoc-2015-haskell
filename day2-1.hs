import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "input2.txt"
    let ls = lines contents
    let dimensions = map (\x -> splitOn "x" x) ls
    print dimensions
