import Builtins
data Content = Player|Empty
board_size = (Grid (3) (3))
type Input = Position
initialBoard :: Board
initialBoard (x)(y)=Empty

goFirst :: Player
goFirst =A

isValid :: (Board,Position)->Bool
isValid (b)(p)=if (getBoardContent (b) (p))==Empty then True else False

threeInARow :: Board->Bool
threeInARow (b)=(or [(inARow (3) (A) (b)),(inARow (3) (B) (b))])

main = return ()