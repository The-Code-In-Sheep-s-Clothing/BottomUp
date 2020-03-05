-- Imports section
import Data.Array
import Data.List
import System.IO.Unsafe
import Data.Char
import System.IO

-- Builtin types
data Player = A | B deriving (Show, Eq)
data Grid = Array (Int, Int) deriving Show
type Board = Array (Int, Int) Content
type Position = (Int, Int)
type Row = [Content]
type BoardProperty = Board -> Bool

-- User defined types
data Content = ContentCon Player|Empty deriving (Show, Eq)
data Player_Tie = Player_TieCon Player|Tie deriving (Show, Eq)

-- Generated User code
board_size = (Array (3,3))
type Input = Position

-- Input functions
input :: Board -> (Int,Int)
input b = unsafePerformIO $ getInts b

getInts :: Board -> IO (Int,Int)
getInts b = do
   putStrLn $ printBoard b
   return (unsafePerformIO getInt,unsafePerformIO getInt)

initialBoard :: Grid -> Board
initialBoard (Array (x,y)) = board (x, y)Empty

goFirst :: Player
goFirst =A

threeInARow :: Board -> Bool
threeInARow b=(or [(inARow (3,A,b)),(inARow (3,B,b))])

loop :: (Board,Player) -> (Board,Player)
loop (b,p)=while (not.gameOver) (tryMove) (b,p)

-- Game over function !
gameOver :: (Board,Player) -> Bool
gameOver (b,p)=(or [(threeInARow b),(isFull b)])

isValid :: (Board,Position) -> Bool
isValid (b,p)=if (getBoardContent (b,p))==Empty then True else False

outcome :: (Board,Player) -> Player_Tie
outcome (b,p)=if (inARow (3,A,b)) then Player_TieCon A else if (inARow (3,B,b)) then Player_TieCon B else Tie

tryMove :: (Board,Player) -> (Board,Player)
tryMove (b,p)=let pos=(input b ) in if (isValid (b,pos)) then ((place (p,b,pos)),(next p)) else (b,p)

play :: (Board,Player) -> Player_Tie
play (a,b)=(outcome (loop (a,b)))

result :: Player_Tie
result =(play (initialBoard board_size,goFirst))

-- PRELUDE
-- asdfasdf
-- Builtin functions
board :: (Int,Int) -> Content -> Board
board size c = listArray ((1,1),size) (Prelude.repeat c)

inARow :: (Int,Player,Board) -> Bool
inARow (n,p,b) = (any (isInfixOf (n `ofKind` p)) . allRows) b

ofKind :: Int -> Player -> Row
n `ofKind` p = map ContentCon (replicate n p)

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

size :: Board -> (Int,Int)
size = snd . bounds

maxRow :: Board -> Int
maxRow = fst . size

maxCol :: Board -> Int
maxCol = snd . size

place :: (Player, Board, Position) -> Board
place (p, b, pos) = b // [(pos, ContentCon p)]

next :: Player -> Player
next A = B
next B = A

while :: (t -> Bool) -> (t -> t) -> t -> t
while cond exe v = if (cond) v then while cond exe (exe v) else v

getInt :: IO Int
getInt = do
   hFlush stdout
   i <- getLine
   return $ read i

numRows :: ((Int, Int), (Int, Int)) -> Int
numRows ((a, b), (c, d)) = c

numCols :: ((Int, Int), (Int, Int)) -> Int
numCols ((a, b), (c, d)) = d

byRows :: [[Content]] -> Board
byRows rows = listArray ((1,1),size) (concat (reverse rows))
           where size = (length rows,length (head rows))

getBoardContent :: (Board, Position) -> Content
getBoardContent (b,p) = b!p

printBoard :: Board -> String
printBoard b = printBoardHelp b (bounds b) (maxLength b (bounds b))

printBoardHelp :: Board -> ((Int, Int), (Int, Int)) -> Int -> String
printBoardHelp board ((a, b), (c, d)) l = spaceString (board!(a, b)) l ++
                                       if (a < c || b < d) then
                                           if(b < d) then
                                           printBoardHelp board ((a, b+1), (c, d)) l
                                           else "\n" ++ printBoardHelp board ((a+1, 1), (c, d)) l
                                       else ""

maxLength :: Board -> ((Int, Int), (Int, Int)) -> Int
maxLength board ((a, b), (c, d)) = max (length (showCell (board!(a,b)))) (if (a < c || b < d) then
                                                       if(b < d) then
                                                       maxLength board ((a, b+1), (c, d))
                                                       else maxLength board ((a+1, 1), (c, d))
                                                       else 0)

spaceString :: Content -> Int -> String
spaceString c l = showCell c ++ extraSpaces (length (showCell c)) (l+1)

showCell :: Content -> String
showCell (ContentCon c) = show c
showCell c = show c

extraSpaces :: Int -> Int -> String
extraSpaces m l = if (m == l) then "" else " " ++ extraSpaces (m+1) l

open :: Board -> [Position]
open g = [p | (p,v) <- assocs g, v==Empty]

isFull :: BoardProperty
isFull = null . open