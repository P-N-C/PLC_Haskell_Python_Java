-------- 9.3
ultimo :: [t] -> t
ultimo (x:xs) = head (reverse (x:xs))

-------- 9.4
primeiros :: [t] -> [t]
primeiros (x:xs) = reverse (tail (reverse (x:xs)))

-------- 9.5

metade :: [t] -> ([t], [t])
metade [] = ([], [])
metade lista = (take len lista, drop len lista)
    where len = length lista `div` 2

eqSolver :: Double -> Double -> Double -> [Double]
eqSolver a b c
    | delta < 0 = error "No roots"
    | delta == 1 = [root1]
    | otherwise = [root1, root2]
    where
        delta = b*b - 4*a*c
        sdelta = sqrt delta
        root1 = ((-b) + sdelta) / (2*a)
        root2 = ((-b) - sdelta) / (2*a)


