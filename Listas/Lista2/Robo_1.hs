data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

solve :: (Int,Int) -> [Command] -> Int -> (Int,Int)
solve (x,y) [] _ = (x,y)
solve (x,y) ((TurnRight):comandos) dir = solve (x,y) comandos ((dir+1)`mod`4)
solve (x,y) ((TurnLeft):comandos) dir = solve (x,y) comandos (if dir == 0 then 3 else dir-1)
solve (x,y) ((Forward n):comandos) dir
  | dir == 0 = solve (x,y+n) comandos dir
  | dir == 1 = solve (x+n,y) comandos dir
  | dir == 2 = solve (x,y-n) comandos dir
  | dir == 3 = solve (x-n,y) comandos dir
solve (x,y) ((Backward n):comandos) dir
  | dir == 0 = solve (x,y-n) comandos dir
  | dir == 1 = solve (x-n,y) comandos dir
  | dir == 2 = solve (x,y+n) comandos dir
  | dir == 3 = solve (x+n,y) comandos dir


destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (x,y) comandos = solve (x,y) comandos 0

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result