
double :: [Int] -> [Int]
double [] = []
double (x:xs) = x : x : double xs

------------------------------------------------

member :: [Int] -> Int -> Bool
member [] e = False
member (x:xs) e
    | x == e = True
    | otherwise = member xs e

------------------------------------------------

digits :: String -> String
digits [] = []
digits (x:xs) = if x >= '0' && x <= '9'
                    then x : digits xs else digits xs

------------------------------------------------

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs [] (y:ys) = y:ys
sumPairs (x:xs) [] = (x:xs)
sumPairs (x:xs) (y:ys) = (x+y) : sumPairs xs ys

------------------------------------------------

smaller :: Int -> [Int] -> [Int]
smaller x [] = []
smaller x (y:ys)
    | y < x = y : (smaller x ys)
    | otherwise = smaller x ys

bigger :: Int -> [Int] -> [Int]
bigger x [] = []
bigger x (y:ys)
    | y >= x = y : (bigger x ys)
    | otherwise = bigger x ys

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort(smaller x xs) ++ [x] ++ (quickSort(bigger x xs))

------------------------------------------------

fibonacci :: Int -> [Int]
fibonacci n
    | n <= 0 = []
    | otherwise = 0 : (fibonacci' [1,0] (n-1))

fibonacci' :: [Int] -> Int -> [Int]
fibonacci' (x:xs) pair
    | pair <= 0 = []
    | nt `mod` 2 == 0 = nt : fibonacci' (nt:x:xs) (pair-1)
    | otherwise = fibonacci' (nt:x:xs) pair
    where
        f1 = x
        f2 = head xs
        nt = f1 + f2

------------------------------------------------

smaller2 :: Int -> [Int] -> [Int]
smaller2 x [] = []
smaller2 x (y:ys)
    | y1 < x1 = y : (smaller2 x ys)
    | otherwise = smaller2 x ys
    where
        y1 = (digit_sum y)
        x1 = (digit_sum x)

bigger2 :: Int -> [Int] -> [Int]
bigger2 x [] = []
bigger2 x (y:ys)
    | y1 >= x1 = y : (bigger2 x ys)
    | otherwise = bigger2 x ys
    where
        y1 = (digit_sum y)
        x1 = (digit_sum x)
        

digit_sum :: Int -> Int
digit_sum 0 = 0
digit_sum n = n `mod` 10 + (digit_sum (n `div` 10))

sort_digit_sum :: [Int] -> [Int]
sort_digit_sum [] = []
sort_digit_sum (x:xs) = sort_digit_sum(smaller2 x xs) ++ [x] ++ (sort_digit_sum(bigger2 x xs))