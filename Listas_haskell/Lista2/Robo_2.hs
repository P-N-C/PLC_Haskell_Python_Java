data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

newD :: Direction -> Command -> Direction
newD d (Forward _) = d
newD d (Backward _) = d
newD North TurnLeft =  West
newD North TurnRight = East
newD South TurnLeft = East
newD South TurnRight = West
newD West TurnLeft = South
newD West TurnRight = North
newD East TurnLeft = North
newD East TurnRight = South

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (x:xs) = faces (newD d x) xs

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result