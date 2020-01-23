import Builtins
data Content = Player|Empty
board_size = (Grid (3) (3))
type Input = Position
initialBoard :: Grid -> Board
initialBoard (Grid x y) = board (x, y)Empty
goFirst  :: Player
goFirst =A

threeInARow  :: Board->Bool
threeInARow (b)=(or [(inARow (3) (A) (b)),(inARow (3) (B) (b))])

loop  :: (Player,Board)->(Player,Board)
loop (p)(b)=while (not. gameOver) (tryMove) (p,b)

gameOver  :: (Board,Player)->Bool
gameOver (b)(p)=(or [(threeInARow (b)),(isFull (b))])

isValid  :: (Board,Position)->Bool
isValid (b)(p)=if (getBoardContent (b) (p))==Empty then True else False

main = return ()