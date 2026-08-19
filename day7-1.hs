import Data.Bits
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word (Word16)
import Debug.Trace (trace)
import Text.Read (readMaybe)

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

parseLine :: String -> (String, Expression)
parseLine line =
  let expressionStrings = splitOn "->" line
      leftExpressionString = expressionStrings !! 0
      rightExpressionString = expressionStrings !! 1
      leftExpression = parseExpressionString leftExpressionString
      var = trim rightExpressionString
  in (var, leftExpression)

traceExpression :: Expression -> Expression
traceExpression e = trace (show e) e

getValue :: Map String Expression -> String -> Word16
getValue expressionMap var =
  -- let mapValue = expressionMap Map.! var :: Expression
  let mapValue = traceExpression (expressionMap Map.! var :: Expression)
  in case mapValue of
    ExpressionValue num -> num
    ExpressionString word -> maybe (getValue expressionMap word) id (readMaybe word :: Maybe Word16)
    ExpressionAnd word1 word2 -> (getValue expressionMap word1) .&. (getValue expressionMap word2)
    ExpressionOr word1 word2 -> (getValue expressionMap word1) .|. (getValue expressionMap word2)
    ExpressionLShift word num -> (getValue expressionMap word) `shiftL` num
    ExpressionRShift word num -> (getValue expressionMap word) `shiftR` num
    ExpressionNot word -> complement (getValue expressionMap word)

main :: IO ()
main = do
  -- let content = example
  -- let contentLines = lines content
  -- print contentLines
  -- let expressionMap = Map.fromList (map parseLine contentLines)
  -- print expressionMap
  -- print (getValue expressionMap "e")

  content <- readFile "input7.txt"
  let contentLines = lines content
  print contentLines
  let expressionMap = Map.fromList (map parseLine contentLines)
  print expressionMap
  print (getValue expressionMap "a")
