import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

data ActionType = TurnOn | TurnOff | Toggle
  deriving (Show)

type Coordinate = (Int, Int)

data Action = Action ActionType Coordinate Coordinate
  deriving (Show)

typeOf :: Action -> ActionType
typeOf (Action t _ _) = t

startOf :: Action -> Coordinate
startOf (Action _ s _) = s

endOf :: Action -> Coordinate
endOf (Action _ _ e) = e

parseCoordinate :: String -> Coordinate
parseCoordinate s = 
  let words = splitOn "," s
  in (read (words !! 0), read (words !! 1)) 

parseActionLine :: String -> Action
parseActionLine line
  | "turn on" `isPrefixOf` line = Action TurnOn start end
  | "turn off" `isPrefixOf` line = Action TurnOff start end
  | "toggle" `isPrefixOf` line = Action Toggle start end
  where
    lineWords = words line
    start = parseCoordinate (reverse lineWords !! 2)
    end = parseCoordinate (last lineWords)

runFromStartToEnd :: Coordinate -> Coordinate -> (Coordinate -> ST s ()) -> ST s ()
runFromStartToEnd start end f = do
  forM_ ([fst start..fst end]) (\x -> forM_ ([fst start..fst end]) (\y -> f (x, y)))

runAction :: STArray s (Int, Int) Bool -> Action -> ST s ()
runAction arr action = do
  case typeOf action of
    TurnOn -> runFromStartToEnd (startOf action) (endOf action) (\c -> writeArray arr c True)

createGrid :: [Action] -> Array (Int, Int) Bool
createGrid actions = runSTArray $ do
  arr <- newArray ((0, 0), (5, 5)) False :: ST s (STArray s (Int, Int) Bool)
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
  print (fst (parseCoordinate "123,345"))

  let grid = createGrid [action]
  print grid