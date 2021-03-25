solve :: Int-> [Char]
solve cp
  | (f+s)^2 == cp = "Charmander vitorioso"
  | otherwise = "Charmander derrotado"
  where
    f = ((cp `div` 1000) `mod` 10)*10 + ((cp `div` 100) `mod` 10)
    s = ((cp `div` 10) `mod` 10)*10 + (cp `mod` 10)

main = do
  cp <- readLn
  putStr (solve cp)