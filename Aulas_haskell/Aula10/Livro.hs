-- Tarefa 14.1
type Nome = String
type Peso = Double
type Altura = Double
type Imc = Double

imc :: (Nome, Peso, Altura) -> Imc
imc = \(n, p, a) ->  p  / (a**2)

-- Tarefa 14.2
filtro :: [Int] -> [Int]
filtro list = filter (\x -> x `mod` 3 == 0) list 

-- Tarefa 14.3
func :: Num t => Bool -> (t, t) -> t
func = \a (m,n) -> if a then (m+n)^2 else (m+n)^3

-- Tarefa 14.4
composta :: (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)
composta a b = ab
    where ab = a.b

-- Tarefa 14.5
composta' :: (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)
composta' = \a b -> a.b

-- Tarefa 15.6
{-
(toUpper . head . head) ["maria","jose","silva"] = M
(not . odd . length) "felicidade" = True
(isLetter . head . head . reverse) ["maria","silva","pereira"] = True
(even . (\x -> x*2 + 3) . (\x -> div x 2) . snd) (9+4,9-4) = Fale
-}