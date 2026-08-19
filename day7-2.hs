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
      let (leftMap, leftValue) = evaluateTerm expressionMap leftTerm
          (rightMap, rightValue) = evaluateTerm leftMap rightTerm
          result = leftValue .&. rightValue
      in (Map.insert str (ExpressionTerm (Left result)) rightMap, result)
    ExpressionOr leftTerm rightTerm ->
      let (leftMap, leftValue) = evaluateTerm expressionMap leftTerm
          (rightMap, rightValue) = evaluateTerm leftMap rightTerm
          result = leftValue .|. rightValue
      in (Map.insert str (ExpressionTerm (Left result)) rightMap, result)
    ExpressionLShift term num ->
      let (map, value) = evaluateTerm expressionMap term
          result = value `shiftL` num
      in (Map.insert str (ExpressionTerm (Left result)) map, result)
    ExpressionRShift term num ->
      let (map, value) = evaluateTerm expressionMap term
          result = value `shiftR` num
      in (Map.insert str (ExpressionTerm (Left result)) map, result)
    ExpressionNot term ->
      let (map, value) = evaluateTerm expressionMap term
          result = complement value
      in (Map.insert str (ExpressionTerm (Left result)) map, result)

evaluateTerm :: Map String Expression -> Term -> (Map String Expression, Word16)
evaluateTerm expressionMap term = case term of
  Left n -> (expressionMap, n)
  Right s -> evaluateString expressionMap s

main :: IO ()
main = do
  content <- readFile "input7.txt"
  let contentLines = lines content
  print contentLines
  let map0 = Map.fromList (map parseLine contentLines)
  print map0
  let (map1, result1) = (evaluateString map0 "a")
  let map2 = Map.insert "b" (ExpressionTerm (Left result1)) map0
  let (map3, result3) = (evaluateString map2 "a")
  print result3
