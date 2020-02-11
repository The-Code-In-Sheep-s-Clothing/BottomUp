module Builtins where

import Prelude hiding (repeat,until)
import qualified Prelude (repeat)
import Data.Array
import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (digitToInt)
import Debug.Trace
import System.IO

data Player   = A | B deriving (Eq,Show)
data Grid     = Array (Int,Int) deriving(Show)
type Board c  = Array (Int,Int) c
type State    = (Board Content,Player)
data Content  = Occupied Player | Empty deriving (Show,Eq)
type Position = (Int,Int)

--
-- Generic functions on boards
--
type Row c = [c]

-- Create a board from rows, using bottom-up numbering of rows
--
board :: (Int,Int) -> Content -> Board
board size c = listArray ((1,1),size) (Prelude.repeat c)

byRows :: [[c]] -> Board c
byRows rows = listArray ((1,1),size) (concat (reverse rows))
              where size = (length rows,length (head rows))

getBoardContent :: (Board c, Position) -> c
getBoardContent (b,p) = b!p

printBoard :: Show c => Board c -> String
printBoard b = printBoardHelp b (bounds b)

printBoardHelp :: Show c => Board c -> ((Int, Int), (Int, Int)) -> String
printBoardHelp board ((a, b), (c, d)) = spaceString (board!(a, b)) ++ 
					if (a < c || b < d) then
						if(b < d) then
						printBoardHelp board ((a, b+1), (c, d)) 
						else "\n" ++ printBoardHelp board ((a+1, 1), (c, d))
					else ""

spaceString :: Show c => c -> String
spaceString c = show c

while :: (t -> Bool) -> (t -> t) -> t -> t
while cond exe v = if (cond) v then while cond exe (exe v) else v

next :: Player -> Player
next A = B
next B = A

place :: (Player, Board Content, Position) -> Board Content
place (p, b, pos) = b // [(pos, Occupied p)]

getInts :: Show c => Board c -> IO (Int, Int)
getInts b = do
    putStrLn $ printBoard b
    x <- getInt
    y <- getInt
    return (x, y)

getInt :: IO Int
getInt = do
    hFlush stdout
    i <- getLine
    return $ read i

input :: Show c => Board c -> [Int] -> Position
input b l = unsafePerformIO $ getInts b

-- Board size
--
size :: Board c -> (Int,Int)
size = snd . bounds

maxRow :: Board c -> Int
maxRow = fst . size

maxCol :: Board c -> Int
maxCol = snd . size

-- Extracting rows, columns, and diagonals
--
row :: Board c -> Int -> Row c
row b y = [b!(y,x) | x <- [1..maxCol b]]

rows :: Board c -> [Row c]
rows b = [row b r | r <- [1..maxRow b]]

col :: Board c -> Int -> Row c
col b x = [b!(y,x) | y <- [1..maxRow b]]

cols :: Board c -> [Row c]
cols b = [col b c | c <- [1..maxCol b]]

diagsUp :: Board c -> (Int,Int) -> Row c
diagsUp b (y,x) | y > maxRow b || x > maxCol b = []
               | otherwise = b!(y,x):diagsUp b (y+1,x+1)

diagsDown :: Board c -> (Int,Int) -> Row c
diagsDown b (y,x) | y < 1 || x < 1 = []
                 | otherwise = b!(y,x):diagsDown b (y-1,x-1)

diags :: Board c -> [Row c]
diags b = [diagsUp b (1,x) | x <- [1..maxCol b]] ++
          [diagsUp b (y,1) | y <- [2..maxRow b]] ++
          [diagsDown b (maxRow b,x) | x <- [1..maxCol b]] ++
          [diagsDown b (y,1) | y <- [1..maxRow b-1]]

allRows :: Board c -> [Row c]
allRows b = rows b ++ cols b ++ diags b


-- Printing a board
--
-- showRow :: Game -> Row -> String
-- showRow g = concatMap (output g)

-- maxRows :: Board -> Int
-- maxRows = fst . snd . bounds

-- showBoard :: Game -> Board -> String
-- showBoard g b = unlines [showRow g (row b r) | r <- reverse [1..maxRows b]]

-- printBoard :: Game -> Board -> IO ()
-- printBoard g = putStrLn . showBoard g


-- Conditions / board properties
--
type BoardProperty = Board Content -> Bool

(&&&) :: BoardProperty -> BoardProperty -> BoardProperty
p &&& q = \b -> p b && q b

(|||) :: BoardProperty -> BoardProperty -> BoardProperty
p ||| q = \b -> p b || q b

open :: Board Content -> [Position]
open g = [p | (p,v) <- assocs g, v==Empty]

isFull :: BoardProperty
isFull = null . open

-- (!!!) :: Board -> [Position] -> [Content]
-- g !!! ps = map (g!) ps

ofKind :: Int -> Player -> Row Content
-- ofKind n = map Occupied . replicate n
n `ofKind` p = map Occupied (replicate n p)

-- fourAs = 4 `ofKind` A
-- fourBs = 4 `ofKind` B

inARow :: (Int,Player,Board Content) -> Bool
inARow (n,p,b) = (any (isInfixOf (n `ofKind` p)) . allRows) b
