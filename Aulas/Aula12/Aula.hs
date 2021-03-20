data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

--------------------------------------------------

data List t = Nil | Cons t (List t) deriving(Show)

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr
            deriving(Show)

data Tree t = Nilt |
              Node t (Tree t) (Tree t) deriving(Show)

showExpr :: Expr -> String
showExpr exp = show exp

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs


fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

depth :: Tree t -> Int
depth Nilt = 0
depth (Node t t1 t2) = 1 + (max (depth t1) (depth t2))

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ Nilt = Nilt
mapTree func (Node t t1 t2) = (Node (func t) (mapTree func t1) (mapTree func t2))
