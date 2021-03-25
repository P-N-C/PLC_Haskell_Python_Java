halve xs = ([xs !!x| x <- [0..(length xs)-1], x `mod` 2 == 0],
            [xs !!x| x <- [0..(length xs)-1], x `mod` 2 /= 0])


main = do
  x <- getLine
  print $ halve (words x)