listMaker :: Eq t => [[t]] -> [t]
listMaker [] = []
listMaker (x:xs) = x ++ listMaker xs

agruparSolve :: Eq t => [t] -> [(t, Int)]
agruparSolve [] = []
agruparSolve list = (h, hn) : agruparSolve nlist
    where
        h = head list
        hn = length[x | x <- list, h == x]
        nlist = [x | x <- list, h /= x]

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar l = agruparSolve (listMaker l)