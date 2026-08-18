import Data.Bits
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace (trace)
import Data.Word (Word16)

example :: String
-- example = "123 -> x\
--           \456 -> y\
--           \x AND y -> d\
--           \x OR y -> e\
--           \x LSHIFT 2 -> f\
--           \y RSHIFT 2 -> g\
--           \NOT x -> h\
--           \NOT y -> i"

example = "123 -> x\n\
          \456 -> y\n\
          \x AND y -> d"

lookupOrRead :: (Map String Word16) -> String -> Word16
lookupOrRead map word
  | Map.member word map = map Map.! word
  | otherwise = read word :: Word16

traceWords :: [String] -> [String]
traceWords xs = trace ("words: " ++ (show xs)) xs

evaluate :: (Map String Word16) -> String -> Word16
evaluate map expression =
  let expressionWords = traceWords (words expression)
  -- in read expression :: Word16
  -- in read (expressionWords !! 0) :: Word16
  in case expressionWords of
    [word] -> read word :: Word16
    [word1, "AND", word2] -> (lookupOrRead map word1) .&. (lookupOrRead map word2)

executeLine :: (Map String Word16) -> String -> Map String Word16
executeLine map line =
  let expressions = splitOn "->" line
      leftExpression = expressions !! 0
      rightExpression = expressions !! 1
      num = evaluate map leftExpression
      var = trim rightExpression
  in Map.insert var num map

main :: IO ()
main = do
  let a = 0xFFFF :: Word16
  print a
  print (a + 1)
  let exampleLines = lines example
  print exampleLines
  let map = foldl (executeLine) Map.empty exampleLines
  print map
