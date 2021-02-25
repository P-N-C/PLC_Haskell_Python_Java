{------------------- AULA 3 -------------------}
imc :: Double -> Double -> Double
imc peso altura = peso / (altura * altura)

------------------------------------------------

sort_check :: Int -> Int -> Int -> Bool
sort_check a b c = a < b && b < c

------------------------------------------------

vendas :: Int -> Int
vendas n = mod n 17

match :: Int -> Int -> Int
match s n
    | s == n = 1
    | otherwise = 0

vendas_spec :: Int -> Int -> Int
vendas_spec s n
    | n <= 0    = match s (vendas n)
    | otherwise = match s (vendas n) + vendas_spec s (n - 1)

------------------------------------------------

is_div :: Int -> Int -> Bool
is_div n t
    | mod n t == 0 = True
    | otherwise = False

is_prime_aux :: Int -> Int -> Bool
is_prime_aux n t
    | t == 1 = True
    | otherwise = not (is_div n t) && is_prime_aux n (t-1)

is_prime :: Int -> Bool
is_prime n
    | n < 2 = False
    | otherwise = is_prime_aux n (n - 1)

------------------------------------------------

mdc :: Int-> Int -> Int
mdc a b
    | a == 0 = b
    | otherwise = mdc (mod b a) a

is_prime_between :: Int -> Int -> Bool
is_prime_between a b
    | mdc (min a b) (max a b) == 1 = True
    | otherwise = False

------------------------------------------------

fat :: Int -> Int
fat n
    | n <= 1 = 1
    | otherwise = n * fat (n - 1)

------------------------------------------------

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
    | a == b && b == c && c == d = True
    | otherwise = False

------------------------------------------------

equalCount :: Int -> Int -> Int -> Int
equalCount a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

{----------------------------------------------}

{------------------- AULA 4 -------------------}

addEspacos :: Int -> String
addEspacos n
    | n == 0 = ""
    | otherwise = " " ++ addEspacos (n - 1)

------------------------------------------------

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

------------------------------------------------

val = 10

imprimeSemanas :: Int -> String
imprimeSemanas n
    | n == 0 = show n ++ "         " ++ show val ++ "\n"
    | otherwise = imprimeSemanas (n-1) ++ show n ++
        "         " ++ show ((val + n*23) `mod` 20) ++ "\n"

imprimeTotalHelp :: Int -> Int
imprimeTotalHelp n
    | n == 0 = val
    | otherwise = imprimeTotalHelp (n-1) + (val + n*23) `mod` 20

imprimeTotal :: Int -> String
imprimeTotal n = "Total    " ++ show (imprimeTotalHelp n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = show ( imprimeTotalHelp n `div` (n + 1)) ++ "\n"

imprimeTabela n = putStr ("Semana   Venda\n"
                ++ imprimeSemanas n
                ++ imprimeTotal n
                ++ imprimeMedia n)

{------------------- AULA 5 -------------------}

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

{------------------- AULA 6 -------------------}