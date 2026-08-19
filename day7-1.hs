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

type Term = Either Word16 String
parseTerm :: String -> Term
parseTerm s = case readMaybe s of
  Just n -> Left n
  Nothing -> Right s

data Expression
  = ExpressionTerm Term
  | ExpressionAnd Term Term
  | ExpressionOr Term Term
  | ExpressionLShift Term Int
  | ExpressionRShift Term Int
  | ExpressionNot Term
  deriving (Show)

parseExpressionString :: String -> Expression
parseExpressionString expression =
  let strs = words expression
  in case strs of
    [str] -> ExpressionTerm (parseTerm str)
    [leftStr, "AND", rightStr] -> ExpressionAnd (parseTerm leftStr) (parseTerm rightStr)
    [leftStr, "OR", rightStr] -> ExpressionOr (parseTerm leftStr) (parseTerm rightStr)
    [leftStr, "LSHIFT", rightStr] -> ExpressionLShift (parseTerm leftStr) (read rightStr :: Int)
    [leftStr, "RSHIFT", rightStr] -> ExpressionRShift (parseTerm leftStr) (read rightStr :: Int)
    ["NOT", str] -> ExpressionNot (parseTerm str)

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

evaluateString :: Map String Expression -> String -> (Map String Expression, Word16)
evaluateString expressionMap str =
  -- let value = expressionMap Map.! str :: Expression
  let value = traceExpression (expressionMap Map.! str :: Expression)
  in case value of
    ExpressionTerm term -> evaluateTerm expressionMap term
    ExpressionAnd leftTerm rightTerm ->
      let leftPair = evaluateTerm expressionMap leftTerm
          rightPair = evaluateTerm (fst leftPair) rightTerm
          result = (snd leftPair) .&. (snd rightPair) 
      in ((fst rightPair), result)
    ExpressionOr leftTerm rightTerm ->
      let leftPair = evaluateTerm expressionMap leftTerm
          rightPair = evaluateTerm (fst leftPair) rightTerm
          result = (snd leftPair) .|. (snd rightPair) 
      in ((fst rightPair), result)
    ExpressionLShift term num ->
      let pair = evaluateTerm expressionMap term
          result = (snd pair) `shiftL` num
      in ((fst pair), result)
    ExpressionRShift term num ->
      let pair = evaluateTerm expressionMap term
          result = (snd pair) `shiftR` num
      in ((fst pair), result)
    ExpressionNot term ->
      let pair = evaluateTerm expressionMap term
          result = complement (snd pair)
      in ((fst pair), result)

evaluateTerm :: Map String Expression -> Term -> (Map String Expression, Word16)
evaluateTerm expressionMap term = case term of
  Left n -> (expressionMap, n)
  Right s -> evaluateString expressionMap s

main :: IO ()
main = do
  let content = example
  let contentLines = lines content
  print contentLines
  let expressionMap = Map.fromList (map parseLine contentLines)
  print expressionMap
  print (evaluateString expressionMap "e")

  -- content <- readFile "input7.txt"
  -- let contentLines = lines content
  -- print contentLines
  -- let expressionMap = Map.fromList (map parseLine contentLines)
  -- print expressionMap
  -- print (evaluateString expressionMap "a")
