menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (mini, maxi)
    where 
        maxi = max (max a b) c
        mini = min (min a b) c
        
-----------------------------------------------

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort([y | y <- xs, y < x]) ++ [x] ++ sort([y | y <- xs, y >= x])

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (l, m, r)
    where
        l = list !! 0
        m = list !! 1
        r = list !! 2
        list = sort [a, b, c]

-----------------------------------------------

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

fstp :: Ponto -> Float
fstp p = fst p

sndp :: Ponto -> Float
sndp p = snd p

is_vertical :: Reta -> Bool
is_vertical reta = (x1 == x2)
    where
        x1 = fst (fst reta)
        x2 = fst (snd reta)

pontoY :: Float -> Reta -> Float
pontoY x reta
    | x1 == x2 = 0
    | otherwise = y
    where
        y = ( ( (y2-y1) * (x-x1) ) / (x2-x1) ) + y1
        x1 = fst (fst reta)
        x2 = fst (snd reta)
        y1 = snd (fst reta)
        y2 = snd (snd reta)
