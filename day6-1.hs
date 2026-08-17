import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

data ActionType = TurnOn | TurnOff | Toggle
  deriving (Show)

data Action = Action ActionType Coordinate Coordinate
  deriving (Show)

typeOf :: Action -> ActionType
typeOf (Action t _ _) = t

startOf :: Action -> Coordinate
startOf (Action _ s _) = s

data Coordinate = Coordinate Int Int
  deriving (Show)

firstOf :: Coordinate -> Int
firstOf (Coordinate x _) = x

secondOf :: Coordinate -> Int
secondOf (Coordinate _ y) = y

tupleOf :: Coordinate -> (Int, Int)
tupleOf (Coordinate x y) = (x, y)

parseCoordinate :: String -> Coordinate
parseCoordinate s = 
  let words = splitOn "," s
  in Coordinate (read (words !! 0)) (read (words !! 1)) 

parseActionLine :: String -> Action
parseActionLine line
  | "turn on" `isPrefixOf` line = Action TurnOn start end
  | "turn off" `isPrefixOf` line = Action TurnOff start end
  | "toggle" `isPrefixOf` line = Action Toggle start end
  where
    lineWords = words line
    start = parseCoordinate (reverse lineWords !! 2)
    end = parseCoordinate (last lineWords)

runAction :: STArray s (Int, Int) Bool -> Action -> ST s ()
runAction arr action = do
  case typeOf action of
    TurnOn -> writeArray arr (tupleOf (startOf action)) True

createGrid :: [Action] -> Array (Int, Int) Bool
createGrid actions = runSTArray $ do
  arr <- newArray ((0, 0), (5, 5)) False :: ST s (STArray s (Int, Int) Bool)
  runAction arr (head actions)
  forM_ (actions) (\action -> runAction arr action) 
  return arr

main :: IO ()
main = do
  -- content <- readFile "input6.txt"
  -- let actionLines = lines content
  --     actionLine = actionLines !! 0
  let actionLine = "turn on 1,1 through 2,2"
      action = parseActionLine actionLine
  print actionLine
  print action
  print (firstOf (parseCoordinate "123,345"))

  let grid = createGrid [action]
  print grid