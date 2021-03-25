{-
Tarefa 14.7

a) (’c’:)
- Adiciona o caractere 'c' no inicio da lista
- ('c':) :: [Char] -> [Char]
- \list -> c : list

b) (:"fim") 
- Adiciona algum caractere no inicio da lista "fim"
- (:"fim") :: Char -> [Char]

Tarefa 14.8

[(2,5),(0,1),(4,4)]
[9, -1, 15]
23

Tarefa 14.9
mult :: Int -> (Int -> Int -> Int)
mult :: Int -> (Int -> Int)
mult :: Int -> Int
mult = \x -> \y -> \z -> x * y * z
-}
