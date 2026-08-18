import Data.Bits
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
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
          \456 -> y"

executeLine :: (Map String Word16) -> String -> Map String Word16
executeLine map line =
  let expressions = splitOn "->" line
      leftExpression = expressions !! 0
      rightExpression = expressions !! 1
      num = read leftExpression :: Word16
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
