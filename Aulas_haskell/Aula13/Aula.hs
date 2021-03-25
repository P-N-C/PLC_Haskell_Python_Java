splitWords :: String -> [String]
splitWords [] = []
splitWords (l:ls) = reverse [x | x <- (splitHelp ls [[l]]), x /= "", x /= " "]

splitHelp :: String -> [String] -> [String]
splitHelp [] resp = resp
splitHelp (x:xs) (l:ls)
    | x /= ' ' = splitHelp (xs) ((lc ++ [x]):ls)
    | otherwise = splitHelp (xs) ("":lc:ls)
    where lc = filter (/=' ') l