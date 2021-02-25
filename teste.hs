solve :: Int -> Int -> [Char] -> [Char] -> Double -> Double -> [Char]
solve f1 f2 (a1:as1) (a2:as2) p1 p2
  | f1 > f2 = victory
  | f1 < f2 = loose
  | a1 > a2 = victory
  | a1 < a2 = loose
  | p1 > p2 = victory
  | p1 < p2 = loose
  | otherwise = victory
  where victory = "Aang venceu o combate!"
        loose = "Aang perdeu o combate e agora esta preso na fortaleza da nacao do fogo."
        
        
main = do
  f1 <- readLn
  f2 <- readLn
  a1 <- readLn
  a2 <- readLn
  p1 <- readLn
  p2 <- readLn
  putStr (solve f1 f2 a1 a2 p1 p2)