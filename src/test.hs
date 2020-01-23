import Builtins

cond :: Int -> Bool
cond a = a == 5

loop :: Int -> Int
loop a = a + 1

t = while (cond) (loop) 0