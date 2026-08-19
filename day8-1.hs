main :: IO ()
main = do
  content <- readFile "example8.txt"
  -- content <- readFile "input8.txt"
  let contentLines = lines content
      readLines = map (\x -> read x :: String) contentLines
      stringLengths = map length contentLines
      memoryLengths = map length readLines
  print contentLines
  print readLines
  print stringLengths
  print memoryLengths
  print ((sum stringLengths) - (sum memoryLengths))
