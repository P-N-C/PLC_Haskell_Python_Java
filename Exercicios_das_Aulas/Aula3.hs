
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