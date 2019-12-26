module Main where

import           Data.List
import           Data.Maybe
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  start

------------------------------------------------------------------------
--                             DATA TYPES                             --
------------------------------------------------------------------------


data Player = P1 | P2 deriving Eq

type Board = [[Maybe Player]]


------------------------------------------------------------------------
--                          GAME PARAMETERS                           --
------------------------------------------------------------------------


rows :: Int
rows = 6

cols :: Int
cols = 7

numToWin :: Int
numToWin = 4


------------------------------------------------------------------------
--                           SHOW FUNCTIONS                           --
------------------------------------------------------------------------


instance Show Player where
  show P1 = "🔴"
  show P2 = "🔵"

initial :: Board
initial = replicate cols $ replicate rows Nothing

emptyBoard :: [String]
emptyBoard = intersperse emptyRow $ replicate (rows + 1) rowDelimiter
  where
    emptyRow     = intercalate "   " $ replicate (cols + 1) "|"
    rowDelimiter = intercalate "---" $ replicate (cols + 1) "+"

showEmptyBoard :: IO ()
showEmptyBoard = do
  goto (1, 5)
  mapM_ putStrLn emptyBoard

updatePosition :: Player -> (Int, Int) -> IO ()
updatePosition player (colPos, rowPos) = do
  goto (posx, posy + 4)
  putStr (show player)
  where
    posx = 4 * colPos + 3
    posy = 2 * rowPos + 2

------------------------------------------------------------------------
--                          UPDATE FUNCTIONS                          --
------------------------------------------------------------------------


replaceFirstMatch :: (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirstMatch p r xs = if any p xs then Just (rep xs) else Nothing
  where
    rep [] = error "unreachable"
    rep (x : _) | p x       = r : xs
                | otherwise = x : rep xs

updateBoard :: Board -> Player -> Int -> Maybe Board
updateBoard board player col = do
  (b, bs') <- uncons bs
  b'       <- replaceFirstMatch isNothing (Just player) b
  return $ as ++ (b' : bs')
  where (as, bs) = splitAt col board

numInColumn :: Board -> Int -> Int
numInColumn (b : _    ) 0 = length $ catMaybes b
numInColumn (_ : board) n = numInColumn board (n - 1)
numInColumn _           _ = error "Something went wrong..."

next :: Player -> Player
next P1 = P2
next P2 = P1


------------------------------------------------------------------------
--                            WIN CHECKING                            --
------------------------------------------------------------------------


columnWin :: Player -> Board -> Bool
columnWin player = any
  ( any ((>= numToWin) . length)
  . filter ((== player) . fromJust . head)
  . filter (isJust . head)
  . group
  )

rowWin :: Player -> Board -> Bool
rowWin player = columnWin player . transpose

diagonalWin :: Player -> Board -> Bool
diagonalWin player board =
  columnWin player (diagonals board ++ diagonals (reverse board))

diagonals :: Board -> Board
diagonals []         = []
diagonals ([] : xss) = xss
diagonals xss        = zipWith (++)
                               (map ((: []) . head) xss ++ repeat [])
                               ([] : diagonals (map tail xss))

win :: Player -> Board -> Bool
win player board = any (\p -> p player board) [columnWin, rowWin, diagonalWin]


------------------------------------------------------------------------
--                        PLAY GAME FUNCTIONS                         --
------------------------------------------------------------------------


start :: IO ()
start = do
  cls
  showEmptyBoard
  play initial P1 0

play :: Board -> Player -> Int -> IO ()
play board player n = do
  n' <- select n player
  case updateBoard board player n' of
    Just newBoard -> do
      updatePosition player (n', rows - 1 - numInColumn board n')
      if win player newBoard
        then winner player
        else play newBoard (next player) n'
    Nothing -> play board player n'

select :: Int -> Player -> IO Int
select n p = do
  goto (4 * n + 2, 4)
  putStr (show p)
  goto (1, 2)
  clrline
  hFlush stdout
  c <- getChar
  goto (1, 4)
  clrline
  choose c
  where
    choose c | c `elem` "aj" = select ((n - 1) `mod` cols) p
             | c `elem` "dl" = select ((n + 1) `mod` cols) p
             | c `elem` "sk" = return n
             | otherwise     = select n p

winner :: Player -> IO ()
winner player = do
  goto (1, 1)
  putStrLn $ "Hooray! " ++ show player ++ " is the winner!"
  putStrLn "[q]uit, [r]eplay"
  c <- getChar
  clrline
  case c of
    'q' -> cls >> goto (1, 1)
    'r' -> start
    _   -> winner player


------------------------------------------------------------------------
--                         COMMAND LINE UTILS                         --
------------------------------------------------------------------------


cls :: IO ()
cls = putStr "\ESC[2J"

clrline :: IO ()
clrline = putStr "\ESC[2K"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


