import Data.Bits
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace (trace)
import Data.Word (Word16)

example :: String
example = "123 -> x\n\
          \456 -> y\n\
          \x AND y -> d\n\
          \x OR y -> e\n\
          \x LSHIFT 2 -> f\n\
          \y RSHIFT 2 -> g\n\
          \NOT x -> h\n\
          \NOT y -> i"

traceWords :: [String] -> [String]
traceWords xs = trace ("words: " ++ (show xs)) xs

data Expression
  = ExpressionValue Word16
  | ExpressionString String
  | ExpressionAnd String String
  | ExpressionOr String String
  | ExpressionLShift String Int
  | ExpressionRShift String Int
  | ExpressionNot String

evaluate :: (Map String Word16) -> Expression -> Word16
evaluate map (ExpressionValue num) = num
evaluate map (ExpressionString word) = read word :: Word16
evaluate map (ExpressionAnd word1 word2) = (map Map.! word1) .&. (map Map.! word2)
evaluate map (ExpressionOr word1 word2) = (map Map.! word1) .|. (map Map.! word2)
evaluate map (ExpressionLShift word num) = (map Map.! word) `shiftL` num
evaluate map (ExpressionRShift word num) = (map Map.! word) `shiftR` num
evaluate map (ExpressionNot word) = complement (map Map.! word)

parseExpressionString :: String -> Expression
parseExpressionString expression =
  let expressionWords = words expression
  in case expressionWords of
    [word] -> ExpressionString word
    [word1, "AND", word2] -> ExpressionAnd word1 word2
    [word1, "OR", word2] -> ExpressionOr word1 word2
    [word1, "LSHIFT", word2] -> ExpressionLShift word1 (read word2 :: Int)
    [word1, "RSHIFT", word2] -> ExpressionRShift word1 (read word2 :: Int)
    ["NOT", word] -> ExpressionNot word

evaluateString :: (Map String Word16) -> String -> Word16
evaluateString map expressionString =
  let expression = parseExpressionString expressionString
  in evaluate map expression

executeLine :: (Map String Word16) -> String -> Map String Word16
executeLine map line =
  let expressionStrings = splitOn "->" line
      leftExpressionString = expressionStrings !! 0
      rightExpressionString = expressionStrings !! 1
      num = evaluateString map leftExpressionString
      var = trim rightExpressionString
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
