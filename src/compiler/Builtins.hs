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
data Grid     = Grid (Int,Int) deriving(Show)
type Board    = Array (Int,Int) Content
type State    = (Board,Player)
data Content  = Occupied Player | Empty deriving (Show,Eq)
type Position = (Int,Int)


--
-- Generic functions on boards
--
type Row = [Content]

-- Create a board from rows, using bottom-up numbering of rows
--
board :: (Int,Int) -> Content -> Board
board size c = listArray ((1,1),size) (Prelude.repeat c)

byRows :: [[Content]] -> Board
byRows rows = listArray ((1,1),size) (concat (reverse rows))
              where size = (length rows,length (head rows))

getBoardContent :: (Board, Position) -> Content
getBoardContent (b,p) = b!p

printBoard :: Board -> String
printBoard b = printBoardHelp b (bounds b)

printBoardHelp :: Board -> ((Int, Int), (Int, Int)) -> String
printBoardHelp board ((a, b), (c, d)) = spaceString (board!(a, b)) ++ 
					if (a < c || b < d) then
						if(b < d) then
						printBoardHelp board ((a, b+1), (c, d)) 
						else "\n" ++ printBoardHelp board ((a+1, 1), (c, d))
					else ""

spaceString :: Content -> String
spaceString Empty = "Empty "
spaceString (Occupied A) = "A     "
spaceString (Occupied B) = "B     "

while :: (t -> Bool) -> (t -> t) -> t -> t
while cond exe v = if (cond) v then while cond exe (exe v) else v

next :: Player -> Player
next A = B
next B = A

place :: (Player, Board, Position) -> Board
place (p, b, pos) = b // [(pos, Occupied p)]

getInt :: IO Int
getInt = do
    hFlush stdout
    i <- getLine
    return $ read i

input :: Position
input = (unsafePerformIO getInt, unsafePerformIO getInt)

-- Board size
--
size :: Board -> (Int,Int)
size = snd . bounds

maxRow :: Board -> Int
maxRow = fst . size

maxCol :: Board -> Int
maxCol = snd . size

-- Extracting rows, columns, and diagonals
--
row :: Board -> Int -> Row
row b y = [b!(y,x) | x <- [1..maxCol b]]

rows :: Board -> [Row]
rows b = [row b r | r <- [1..maxRow b]]

col :: Board -> Int -> Row
col b x = [b!(y,x) | y <- [1..maxRow b]]

cols :: Board -> [Row]
cols b = [col b c | c <- [1..maxCol b]]

diagsUp :: Board -> (Int,Int) -> Row
diagsUp b (y,x) | y > maxRow b || x > maxCol b = []
               | otherwise = b!(y,x):diagsUp b (y+1,x+1)

diagsDown :: Board -> (Int,Int) -> Row
diagsDown b (y,x) | y < 1 || x < 1 = []
                 | otherwise = b!(y,x):diagsDown b (y-1,x-1)

diags :: Board -> [Row]
diags b = [diagsUp b (1,x) | x <- [1..maxCol b]] ++
          [diagsUp b (y,1) | y <- [2..maxRow b]] ++
          [diagsDown b (maxRow b,x) | x <- [1..maxCol b]] ++
          [diagsDown b (y,1) | y <- [1..maxRow b-1]]

allRows :: Board -> [Row]
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
type BoardProperty = Board -> Bool

(&&&) :: BoardProperty -> BoardProperty -> BoardProperty
p &&& q = \b -> p b && q b

(|||) :: BoardProperty -> BoardProperty -> BoardProperty
p ||| q = \b -> p b || q b

open :: Board -> [Position]
open g = [p | (p,v) <- assocs g, v==Empty]

isFull :: BoardProperty
isFull = null . open

-- (!!!) :: Board -> [Position] -> [Content]
-- g !!! ps = map (g!) ps

ofKind :: Int -> Player -> Row
-- ofKind n = map Occupied . replicate n
n `ofKind` p = map Occupied (replicate n p)

-- fourAs = 4 `ofKind` A
-- fourBs = 4 `ofKind` B

inARow :: (Int,Player,Board) -> Bool
inARow (n,p,b) = (any (isInfixOf (n `ofKind` p)) . allRows) b
