data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

val :: Ord t => Tree t -> t
val (Node x _ _) = x

is_Nilt :: Ord t => Tree t -> Bool
is_Nilt Nilt = True
is_Nilt (Node _ _ _) = False

quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort([y | y<-xs, y <= x]) ++ [x] ++ quicksort([y | y<-xs, y>x])

inorder :: Ord t=> Tree t -> [t]
inorder Nilt = []
inorder (Node x t1 t2)
    | (is_Nilt t1) && (is_Nilt t2) = [x]
    | (is_Nilt t1) = [x] ++ (inorder t2)
    | (is_Nilt t2) = (inorder t1) ++ [x]
    | otherwise = (inorder t1) ++ [x] ++ (inorder t2)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node x t1 t2) = inorder_list == inorder_sorted_list
  where inorder_list = inorder (Node x t1 t2)
        inorder_sorted_list = quicksort inorder_list


main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result