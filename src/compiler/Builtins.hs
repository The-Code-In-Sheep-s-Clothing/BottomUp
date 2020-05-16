module Builtins where

imports = [
    "import Data.Array",
    "import Data.List",
    "import System.IO.Unsafe",
    "import Data.Char",
    "import System.IO",
    "import Text.ParserCombinators.Parsec"]

builtin_types = [
    "data Grid = Array (Int, Int) deriving Show",
    "type Board = Array (Int, Int) Content",
    "type Position = (Int, Int)",
    "type Row = [Content]",
    "type BoardProperty = Board -> Bool"]

input_funcs = [
    "input :: Int -> Input\n\
    \input _ = \n\
    \    case parse check_input \"\" (unsafePerformIO $ getLine) of\n\
    \        Left err -> input 0\n\
    \        Right i -> i"]

builtin_funcs = [
    "board :: (Int,Int) -> Content -> Board\n\
    \board size c = listArray ((1,1),size) (Prelude.repeat c)",

    "inARow :: (Int,Player,Board) -> Bool\n\
    \inARow (n,p,b) = (any (isInfixOf (n `ofKind` p)) . allRows) b",

    "ofKind :: Int -> Player -> Row\n\
    \n `ofKind` p = map ContentCon (replicate n p)",

    "row :: Board -> Int -> Row\n\
    \row b y = [b!(y,x) | x <- [1..maxCol b]]",

    "rows :: Board -> [Row]\n\
    \rows b = [row b r | r <- [1..maxRow b]]",

    "col :: Board -> Int -> Row\n\
    \col b x = [b!(y,x) | y <- [1..maxRow b]]",

    "cols :: Board -> [Row]\n\
    \cols b = [col b c | c <- [1..maxCol b]]",

    "countColumn :: (Board, Player, Int) -> Int\n\
    \countColumn (b, c, y) = countColumnHelp b (numCols (bounds b)) y (ContentCon c)",

    "countColumnHelp :: Board -> Int -> Int -> Content -> Int\n\
    \countColumnHelp b 0 y c  = 0\n\
    \countColumnHelp b x y c = (if(b!(x,y) == c) then 1 else 0 ) + countColumnHelp b (x-1) y c",

    "countRow :: (Board, Player, Int) -> Int\n\
    \countRow (b, c, x) = countRowHelp b x (numRows (bounds b)) (ContentCon c)",

    "countRowHelp :: Board -> Int -> Int -> Content -> Int\n\
    \countRowHelp b x 0 c  = 0\n\
    \countRowHelp b x y c = (if(b!(x,y) == c) then 1 else 0) + countRowHelp b x (y-1) c",

    "diagsUp :: Board -> (Int,Int) -> Row\n\
    \diagsUp b (y,x) | y > maxRow b || x > maxCol b = []\n\
    \                | otherwise = b!(y,x):diagsUp b (y+1,x+1)",

    "diagsDown :: Board -> (Int,Int) -> Row\n\
    \diagsDown b (y,x) | y < 1 || x < 1 = []\n\
    \                  | otherwise = b!(y,x):diagsDown b (y-1,x-1)",

    "diags :: Board -> [Row]\n\
    \diags b = [diagsUp b (1,x) | x <- [1..maxCol b]] ++\n\
    \          [diagsUp b (y,1) | y <- [2..maxRow b]] ++\n\
    \          [diagsDown b (maxRow b,x) | x <- [1..maxCol b]] ++\n\
    \          [diagsDown b (y,1) | y <- [1..maxRow b-1]]",

    "allRows :: Board -> [Row]\n\
    \allRows b = rows b ++ cols b ++ diags b",

    "size :: Board -> (Int,Int)\n\
    \size = snd . bounds",

    "maxRow :: Board -> Int\n\
    \maxRow = fst . size",

    "maxCol :: Board -> Int\n\
    \maxCol = snd . size",

    "place :: (Content, Board, Position) -> Board\n\
    \place (p, b, pos) = unsafePerformIO $ place_wrapper (p, b, pos)",

    "place_wrapper :: (Content, Board, Position) -> IO Board\n\
    \place_wrapper (p, b, pos) = \n\
    \   let new_board = (b // [(pos, p)]) in do\n\
    \       putStrLn $ printBoard new_board\n\
    \       return (new_board)",

    "while :: (t -> Bool) -> (t -> t) -> t -> t\n\
    \while cond exe v = if (cond) v then while cond exe (exe v) else v",

    "modifyElement :: (Int, Int) -> Content -> Board -> Board\n\
    \modifyElement (x, y) c b = b // [((x,y), c)]",

    "modifyRow :: Int -> Content -> Board -> Board\n\
    \modifyRow x c b= modifyRowHelper b (x, 1) (numRows (bounds b)) c",

    "modifyRowHelper :: Board -> (Int, Int) -> Int -> Content -> Board\n\
    \modifyRowHelper b (x, y) max c = if (y <= max) then modifyElement (x, y) c (modifyRowHelper b (x, y+1) max c) else b",

    "modifyCol :: Int -> Content -> Board -> Board\n\
    \modifyCol y c b= modifyColHelper b (1, y) (numCols (bounds b)) c",

    "modifyColHelper :: Board -> (Int, Int) -> Int -> Content -> Board\n\
    \modifyColHelper b (x, y) max c = if (x <= max) then modifyElement (x, y) c (modifyColHelper b (x+1, y) max c) else b",

    "numRows :: ((Int, Int), (Int, Int)) -> Int\n\
    \numRows ((a, b), (c, d)) = d",

    "numCols :: ((Int, Int), (Int, Int)) -> Int\n\
    \numCols ((a, b), (c, d)) = c",

    "gridSize :: Grid -> (Int, Int)\n\
    \gridSize (Array (x, y)) = (x, y)",

    "countBoard :: (Content, Board) -> Int\n\
    \countBoard (c, b) = sum [1 | xs <- (rows b), x <- xs, c == x]",

    "byRows :: [[Content]] -> Board\n\
    \byRows rows = listArray ((1,1),size) (concat (reverse rows))\n\
    \           where size = (length rows,length (head rows))",

    "getBoardContent :: (Board, Position) -> Content\n\
    \getBoardContent (b,p) = b!p",

    "printBoardIO :: Board -> IO Board\n\
    \printBoardIO b = (putStrLn $ printBoard b) >> return b",

    "printBoard :: Board -> String\n\
    \printBoard b = printBoardHelp b (bounds b) (maxLength b (bounds b))",

    "printBoardHelp :: Board -> ((Int, Int), (Int, Int)) -> Int -> String\n\
    \printBoardHelp board ((a, b), (c, d)) l = spaceString (board!(a, b)) l ++\n\
    \                                       if (a < c || b < d) then\n\
    \                                           if(b < d) then\n\
    \                                           printBoardHelp board ((a, b+1), (c, d)) l\n\
    \                                           else \"\\n\" ++ printBoardHelp board ((a+1, 1), (c, d)) l\n\
    \                                       else \"\"",

    "maxLength :: Board -> ((Int, Int), (Int, Int)) -> Int\n\
    \maxLength board ((a, b), (c, d)) = max (length (showCell (board!(a,b)))) (if (a < c || b < d) then\n\
    \                                                       if(b < d) then\n\
    \                                                       maxLength board ((a, b+1), (c, d))\n\
    \                                                       else maxLength board ((a+1, 1), (c, d))\n\
    \                                                       else 0)",

    "spaceString :: Content -> Int -> String\n\
    \spaceString c l = showCell c ++ extraSpaces (length (showCell c)) (l+1)",

    "showCell :: Content -> String\n\
    \showCell (ContentCon c) = show c\n\
    \showCell c = show c",

    "extraSpaces :: Int -> Int -> String\n\
    \extraSpaces m l = if (m == l) then \"\" else \" \" ++ extraSpaces (m+1) l",

    "open :: Board -> [Position]\n\
    \open g = [p | (p,v) <- assocs g, v==Empty]"]
