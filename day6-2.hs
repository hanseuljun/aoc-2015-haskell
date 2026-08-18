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
  forM_ ([fst start..fst end]) (\x -> forM_ ([snd start..snd end]) (\y -> f (x, y)))

runAction :: STArray s (Int, Int) Int -> Action -> ST s ()
runAction array action = do
  case typeOf action of
    TurnOn -> runFromStartToEnd (startOf action) (endOf action) $ \c -> do
      v <- readArray array c
      writeArray array c (v + 1)
    TurnOff -> runFromStartToEnd (startOf action) (endOf action) $ \c -> do
      v <- readArray array c
      writeArray array c (max (v - 1) 0)
    Toggle -> runFromStartToEnd (startOf action) (endOf action) $ \c -> do
      v <- readArray array c
      writeArray array c (v + 2)

createGrid :: [Action] -> Array (Int, Int) Int
createGrid actions = runSTArray $ do
  array <- newArray ((0, 0), (999, 999)) 0 :: ST s (STArray s (Int, Int) Int)
  forM_ (actions) (\action -> runAction array action) 
  return array

count :: (Array (Int, Int) Int) -> Int
count array = sum (elems array)

main :: IO ()
main = do
  content <- readFile "input6.txt"
  let actionLines = lines content
      actions = map parseActionLine actionLines

  let grid = createGrid actions
  print (count grid)
