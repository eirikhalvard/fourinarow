module Main where
import           Control.Monad (replicateM_)
import           Data.List
import           Text.Read
import           Data.Maybe

main :: IO ()
main = start

data Player = P1 | P2 deriving Eq
type Board = [[Maybe Player]]

-- Game parameters

rows :: Int
rows = 6

cols :: Int
cols = 7

numToWin :: Int
numToWin = 4

instance Show Player where
  show P1    = "🔴"
  show P2    = "🔵"

initial :: Board
initial = replicate cols $ replicate rows Nothing 

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
  goto (posx, posy+2)
  putStr (show player)
  where
    posx = 4 * colPos + 3
    posy = 2 * rowPos + 2

showSelectionRow :: Player -> IO ()
showSelectionRow player = undefined

-- Update functions

replaceFirstMatch :: (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirstMatch p r xs = if any p xs then Just (rep xs) else Nothing
  where
    rep (x:xs) 
      | p x       = r : xs
      | otherwise = x : rep xs

updateBoard :: Board -> Player -> Int -> Maybe Board
updateBoard board player col = do 
  (b,bs') <- uncons bs
  b' <- replaceFirstMatch isNothing (Just player) b
  return $ as ++ (b' : bs')
    where (as,bs) = splitAt col board

numInColumn :: Board -> Int -> Int
numInColumn (b:board) 0 = length $ catMaybes b
numInColumn (b:board) n = numInColumn board (n-1)
numInColumn _ _ = error "du er dum og rett og slett dårlig til å programmere"

next :: Player -> Player
next P1 = P2 
next P2 = P1

-- Win checking

columnWin :: Player -> Board -> Bool
columnWin player = any $ (>= numToWin)
  . maximum 
  . map length 
  . filter ((player ==) . head)
  . group
  . catMaybes

-- rowWin :: Player -> Board -> Bool
-- rowWin player = any
--   $ (>= numToWin)
--   . maximum 
--   . map length 
--   . filter ((player ==) . head)
--   . group

-- Play game functions

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
      case (updateBoard board player (n'-1)) of
        Just newBoard -> do 
          goto (1, 20)
          print $ newBoard
          updatePosition player ((n'-1), rows-1 - numInColumn board (n'-1))
          play newBoard (next player)
        Nothing -> play board player
    Nothing -> play board player

-- Command line utils
cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


