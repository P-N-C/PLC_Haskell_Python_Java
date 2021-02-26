
addEspacos :: Int -> String
addEspacos n
    | n == 0 = ""
    | otherwise = " " ++ addEspacos (n - 1)

------------------------------------------------

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

------------------------------------------------

val = 10

imprimeSemanas :: Int -> String
imprimeSemanas n
    | n == 0 = show n ++ "         " ++ show val ++ "\n"
    | otherwise = imprimeSemanas (n-1) ++ show n ++
        "         " ++ show ((val + n*23) `mod` 20) ++ "\n"

imprimeTotalHelp :: Int -> Int
imprimeTotalHelp n
    | n == 0 = val
    | otherwise = imprimeTotalHelp (n-1) + (val + n*23) `mod` 20

imprimeTotal :: Int -> String
imprimeTotal n = "Total    " ++ show (imprimeTotalHelp n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = show ( imprimeTotalHelp n `div` (n + 1)) ++ "\n"

imprimeTabela n = putStr ("Semana   Venda\n"
                ++ imprimeSemanas n
                ++ imprimeTotal n
                ++ imprimeMedia n)