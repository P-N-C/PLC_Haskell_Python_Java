make0 :: Int -> [Int]
make0 0 = []
make0 n= 0 : (make0 (n-1))

mul2 :: [Int] -> [Int] -> [Int]
mul2 xs ys = [(xs!!it)*(ys!!it)| it <-[0..tam-1]] ++ (make0 dif) 
        where tam = min (length xs) (length ys)
              dif = max (length xs) (length ys) - (min (length xs) (length ys))

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result