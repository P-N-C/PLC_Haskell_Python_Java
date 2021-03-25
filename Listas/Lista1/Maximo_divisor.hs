mdc :: Int-> Int -> Int
mdc a b
    | a == 0 = b
    | otherwise = mdc (mod b a) a

main = do
   a <- readLn
   b <- readLn
   print (mdc (a :: Int) (b :: Int))