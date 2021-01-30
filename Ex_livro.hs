{--------------------- CAP 3 ----------------------}

--------------------- TAREFA 3.3 ------------------

dobro x = x + x

quadruplo x = dobro (dobro x)

--------------------- TAREFA 3.4 ------------------

areaCirculo r = pi * r^2

--------------------- TAREFA 3.5 ------------------

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

--------------------- TAREFA 3.6 ------------------

quad_dob :: Double -> Double
quad_dob x = (x + x)^2

--------------------- TAREFA 3.7 ------------------

dob_quad :: Double -> Double
dob_quad x = 2*(x^2)

--------------------- TAREFA 3.8 ------------------

is_tri :: Double -> Double -> Double -> Bool
is_tri a b c = (a + b > c) && (b + c > a) && (a + c > b)

--------------------- TAREFA 3.9 ------------------
compressão :: Double -> Double -> Double
compressão f k = f / k

energia_pot :: Double -> Double -> Double
energia_pot k x = (1/2) * k * x^2

--------------------- TAREFA 3.10 -----------------

cost :: Double -> Double -> Double
cost s q = (s/5) * q * (1 - 0.15)

--------------------- TAREFA 3.11 -----------------

freq :: Double -> Double -> Double
freq l c = 1 / (2 * pi * (sqrt (l * c)))

--------------------- TAREFA 3.12 -----------------

area a b c = c * h / 2
    where
        h = b * sin
        sin = sqrt(1 - cos^2)
        cos = (b*b + c*c -(a*a)) / (2*a*c)

{--------------------------------------------------}

{--------------------- CAP 7 ----------------------}

--------------------- TAREFA 7.7 ------------------

fat2 :: Int -> Int
fat2 n
    | n == 2 || n == 1 = n
    | n < 1 = 1
    | otherwise = n * fat2 (n - 2)

--------------------- TAREFA 7.8 ------------------

mult_interval :: Int -> Int -> Int
mult_interval a b
    | a < b = a * mult_interval (a+1) b
    | a > b = a * mult_interval (a-1) b
    | otherwise = a

--------------------- TAREFA 7.9 ------------------

fatorial :: Int -> Int
fatorial n = mult_interval 1 n

--------------------- TAREFA 7.10 -----------------

adi :: Int -> Int -> Int
adi a b
    | b == 0 = a
    | otherwise = adi (succ a) (pred b)

--------------------- TAREFA 7.11 ------------------

pot :: Int -> Int -> Int
pot x n
    | n == 0 = 1
    | n == 1 = x
    | otherwise = x * pot x (n-1)

--------------------- TAREFA 7.12 ------------------

square_help :: Int -> Int -> Int
square_help a b
    | a * a <= b = a
    | otherwise = square_help (a-1) b

int_square :: Int -> Int
int_square n = square_help n n

--------------------- TAREFA 7.13 ------------------
resto :: Int -> Int -> Int
resto a b
    | a >= b = resto (a - b) b
    | otherwise = a

division_help :: Int -> Int -> Int -> Int
division_help a b c
    | a >= b = division_help (a - b) b (c + 1)
    | otherwise = c

division :: Int -> Int -> Int
division a b = division_help a b 0

--------------------- TAREFA 7.14 ------------------

mdc :: Int -> Int -> Int
mdc a b
    | b == 0 = a
    | b > 0 = mdc b (a `mod` b)
    | otherwise = mdc a (-b)

{--------------------------------------------------}

{--------------------- CAP 4 ----------------------}

--------------------- TAREFA 4.1 ------------------
g = 6.67*(10**(-11))

fg :: Double -> Double -> Double -> Double
fg m1 m2 d = g * m1 * m2 / (d^2)

--------------------- TAREFA 4.2 ------------------

salario :: Double -> Double
salario s = s + (s*0.1) - (s*0.07)