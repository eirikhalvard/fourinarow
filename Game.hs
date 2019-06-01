module Main where
import           Control.Monad (replicateM_)
import           Data.List
import           Text.Read

main :: IO ()
main = start

data Player = P1 | P2
type Board = [[Player]]

-- Game parameters

rows :: Int
rows = 6

cols :: Int
cols = 7

numInRow :: Int
numInRow = 4

instance Show Player where
  show P1 = "🔴"
  show P2 = "🔵"

initial :: Board
initial = replicate cols []

-- Show utilities

showGame :: Board -> IO ()
showGame s = undefined

emptyBoard :: [String]
emptyBoard = intersperse emptyRow $ replicate (rows+1) rowDelimiter
  where
    emptyRow     = intercalate "   " $ replicate (cols+1) "|"
    rowDelimiter = intercalate "---" $ replicate (cols+1) "+"

showEmptyBoard :: IO ()
showEmptyBoard = do 
  goto (1, 3)
  mapM_ putStrLn emptyBoard

updatePosition :: Player -> (Int, Int) -> IO ()
updatePosition player (colPos, rowPos) = do
  goto (15, 2)
  putStrLn $ "updating " ++ (show player) ++ " on position " ++ show colPos ++ ", " ++ show rowPos ++ ".... actuall numbers: " ++ show posx ++ ", " ++ show posy
  goto (posx, posy+2)
  putStr (show player)
  where
    posx = 4 * colPos + 3
    posy = 2 * rowPos + 2

showSelectionRow :: Player -> IO ()
showSelectionRow player = undefined

-- Update functions

updateBoard :: Board -> Player -> Int -> Board
updateBoard board player col = as ++ (player:b):bs
  where
    (as,b:bs) = splitAt col board

-- Play game functions

numInColumn :: Board -> Int -> Int
numInColumn (b:board) 0 = length b
numInColumn (b:board) n = numInColumn board (n-1)
numInColumn _ _ = error "du er dum"

next :: Player -> Player
next P1 = P2 
next P2 = P1

start :: IO ()
start = do
  cls
  showEmptyBoard
  play initial P1

play :: Board -> Player -> IO ()
play board player = do
  goto (1,1)
  putStrLn $ (show player) ++ "-s turn!"
  c <- getChar
  case readMaybe [c] of
    Just n' -> do 
      updatePosition player ((n'-1), rows -1 - numInColumn board (n'-1))
      goto (1,40)
      print $ (updateBoard board player (n'-1))
      play (updateBoard board player (n'-1)) (next player)
    Nothing -> play board player

-- Command line utils
cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


