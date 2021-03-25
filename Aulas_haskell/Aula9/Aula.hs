f :: t -> t
f a = a
minin :: Int -> [Int] -> Bool
minin x [] = True
minin x xs = length (filter (x<) xs) == 0

isCrescentHelp :: [Int] -> Bool
isCrescentHelp [] = True
isCrescentHelp (x:xs) = minin x xs &&  isCrescentHelp xs

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f n = isCrescentHelp (map f [0..n])

---------------------------

powf :: Num t => t -> t
powf a = a * a

mapping :: Num t => [t] -> [t]
mapping list = map powf list

folding :: Num t => [t] -> t
folding list = foldr (+) 0 (map powf list)

filtering :: Ord t => Num t => [t] -> [t]
filtering list = filter (>0) list

----------------------------
mf :: (t->u) -> t -> [u] -> [u]
mf f a b = f a : b 

map' :: (t->u) -> [t] -> [u]
map' f [] = []
map' f list = foldr (mf f) [] list

ff :: (t->Bool) -> t -> [t] -> [t]
ff f a b
    | f a == True = a : b
    | otherwise = b

filter' :: (t -> Bool) -> [t] -> [t]
filter' f list = foldr (ff f) [] list

----------------------------------------

sing a = [a]
naosei l = foldr (++) [] (map sing l) -- nao faz nada :)

--maxin :: [Int] -> Int
maxin [] = -1
maxin (x:xs) = max x (maxin xs)

--maior :: [Int] -> [Int]

maior a b = [maxin a] ++ b

maiores :: [[Int]] -> [Int]
maiores [] = []
maiores list = foldr (maior) [] list

------------------------------------------

takeWhile' :: Ord t => (t -> Bool) -> [t] -> [t]
takeWhile' f [] = []
takeWhile' f (x:xs)
    | f x == True = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: Ord t => (t -> Bool) -> [t] -> [t]
dropWhile' f [] = []
dropWhile' f (x:xs)
    | f x == True = dropWhile' f xs
    | otherwise = x:xs

-------------------------------------------
dropSpace :: String -> String
dropSpace s = dropWhile (== ' ') s

getWord :: String -> String
getWord s = takeWhile (/= ' ') (dropSpace s)

dropWord :: String -> String
dropWord s = dropWhile (/= ' ') (dropSpace s)