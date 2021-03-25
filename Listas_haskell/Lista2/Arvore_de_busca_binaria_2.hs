data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

insertNode :: Ord t => Tree t -> t -> Tree t
insertNode Nilt x = (Node x Nilt Nilt)
insertNode (Node t t1 t2) x
  | x < t = (Node t (insertNode t1 x) t2)
  | otherwise = (Node t t1 (insertNode t2 x))

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList t [] = t
insertList t (x:xs) = insertList (insertNode t x) xs


main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result