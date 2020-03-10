------ Questão 1 -----------

putEsp :: Int -> [Char]
putEsp n
 | n <= 0 = []
 | otherwise = ' ' : putEsp (n-1)

addEspOnStr :: [Char] -> Int -> [Char]
addEspOnStr [] _ = []
addEspOnStr (x:xs) n = x : putEsp n ++ addEspOnStr xs n

------ Questão 2 -----------

appear :: Int -> Int -> Int
appear n x
 | n == 0 = 0
 | (mod n 10 == x) = 1 + appear (n `div` 10) x
 | otherwise = 0 + appear (n `div` 10) x

quantidade :: Int -> Int -> Int
quantidade n x
 | n < 0 = 0
 | otherwise = appear n x + quantidade (n - 1) x

------ Questão 3 ----------- FALTANDO
{-}
removeUm :: Int -> [Int]
removeUm x
 | x == 0 = [x]
 | mod x 10 == 1 = (mod x 10 ) - (1) + removeUm (x `div` 10)
 | otherwise = x + removeUm (x `div` 10)
limpaUm :: [Int] -> [Int]
limpaUm [] = []
limpaUm (x:xs) = (removeUm x) ++ (limpaUm xs)
-}
------ Questão 4 -----------

rev :: String -> String
rev [] = []
rev (x:xs)
 | xs == [] = [x]
 | otherwise = rev(xs) ++ [x]

isPalindromo :: String -> Bool
isPalindromo s =  s ==  (rev s)

------ Questão 5 -----------

potencia :: Int -> Int -> Int
potencia x y
 | y == 0 = 1
 | otherwise = x * potencia x (y - 1)

len [] = 0
len (_:xs)
 | xs == [] = 1
 | otherwise = 1 + len(xs)

realBtoi :: String -> Int -> Int
realBtoi [] n = 0
realBtoi (x:xs) n 
 | n == (-1) = 0 
 | x == '0' = realBtoi xs (n-1)
 | otherwise = (potencia 2 n) + realBtoi xs (n-1)

btoi :: String -> Int
btoi s = realBtoi s ((len s) - 1)

------ Questão 6 -----------

metade :: [Int] -> ([Int], [Int])
metade [] = ([], [])
metade xs = (take num xs, drop num xs)
 where num = len(xs) `div` 2

------ Questão 7 -----------

repete :: Char -> Int -> String
repete _ 0 = []
repete c n = c : repete c (n-1)

isReplica :: String -> Int -> Char -> Bool
isReplica xs n c = xs == (repete c n)

------ Questão 8 -----------

solve :: Char -> [(Char, Char)] -> Char
solve _ [] = error "LETRA NAO ESTA NA LISTA DE TUPLAS"
solve x (y:ys)
 | x == fst y = snd y
 | otherwise = solve x ys

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] _ = []
decEnigma (x:xs) ys = solve x ys : decEnigma xs ys
