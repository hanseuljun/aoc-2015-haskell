import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

data Action = TurnOn | TurnOff | Toggle
  deriving (Show)

data Coordinate = Coordinate Int Int
  deriving (Show)

firstOf :: Coordinate -> Int
firstOf (Coordinate x _) = x

secondOf :: Coordinate -> Int
secondOf (Coordinate _ y) = y

parseCoordinate :: String -> Coordinate
parseCoordinate s = 
  let words = splitOn "," s
  in Coordinate (read (words !! 0)) (read (words !! 1)) 

parseActionLine :: String -> (Action, Coordinate, Coordinate)
parseActionLine line
  | "turn on" `isPrefixOf` line = (TurnOn, start, end)
  | "turn off" `isPrefixOf` line = (TurnOff, start, end)
  | "toggle" `isPrefixOf` line = (Toggle, start, end)
  where
    lineWords = words line
    start = parseCoordinate (reverse lineWords !! 2)
    end = parseCoordinate (last lineWords)

createGrid :: Array (Int, Int) Bool
createGrid = runSTArray $ do
  arr <- newArray ((0, 0), (5, 5)) False :: ST s (STArray s (Int, Int) Bool)
  writeArray arr (1,2) True
  return arr

main :: IO ()
main = do
  content <- readFile "input6.txt"
  let actionLines = lines content
      actionLine = actionLines !! 0
  print actionLine
  print (parseActionLine actionLine)
  print (firstOf (parseCoordinate "123,345"))

  -- let grid = createGrid
  -- print grid