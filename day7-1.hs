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
  deriving (Show)

evaluate :: Map String Word16 -> Expression -> Word16
evaluate expressionMap (ExpressionValue num) = num
evaluate expressionMap (ExpressionString word) = read word :: Word16
evaluate expressionMap (ExpressionAnd word1 word2) = (expressionMap Map.! word1) .&. (expressionMap Map.! word2)
evaluate expressionMap (ExpressionOr word1 word2) = (expressionMap Map.! word1) .|. (expressionMap Map.! word2)
evaluate expressionMap (ExpressionLShift word num) = (expressionMap Map.! word) `shiftL` num
evaluate expressionMap (ExpressionRShift word num) = (expressionMap Map.! word) `shiftR` num
evaluate expressionMap (ExpressionNot word) = complement (expressionMap Map.! word)

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

executeLine :: Map String Word16 -> String -> Map String Word16
executeLine expressionMap line =
  let expressionStrings = splitOn "->" line
      leftExpressionString = expressionStrings !! 0
      rightExpressionString = expressionStrings !! 1
      num = evaluate expressionMap (parseExpressionString leftExpressionString)
      var = trim rightExpressionString
  in Map.insert var num expressionMap

parseLine :: String -> (String, Expression)
parseLine line =
  let expressionStrings = splitOn "->" line
      leftExpressionString = expressionStrings !! 0
      rightExpressionString = expressionStrings !! 1
      leftExpression = parseExpressionString leftExpressionString
      var = trim rightExpressionString
  in (var, leftExpression)

main :: IO ()
main = do
  let content = example
  let contentLines = lines content
  print contentLines
  let expressionMap = Map.fromList (map parseLine contentLines)
  print expressionMap
  let evaluatedExpressionMap = foldl (executeLine) Map.empty contentLines
  print evaluatedExpressionMap
