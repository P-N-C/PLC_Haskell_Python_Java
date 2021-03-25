bb :: Int -> Int -> Int
bb n 0 = 0
bb n s
  | s`mod`10 == 0 = bb (n+1) (s`div`10)
  | otherwise = (2^n) + (bb (n+1) (s`div`10))

btoi :: String -> Int
btoi s = bb 0 (read s::Int)

main = do
    s <- getLine
    let result = btoi s
    print result