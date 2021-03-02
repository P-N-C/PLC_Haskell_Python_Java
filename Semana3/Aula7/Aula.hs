type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = 
    [("Sergio","O Senhor dos Aneis"),
    ("Andre","Duna"),
    ("Fernando","Jonathan Strange & Mr.Norrell"),
    ("Fernando","A Game of Thrones")]
----------------------------------
membro :: [Int] -> Int -> Bool
membro ls l = length [x | x <- ls, x == l] > 0

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [x | (y, x) <- bd, y == p]

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos bd l = [x | (x, y) <- bd, y == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = length [x | (x, y) <- bd, y == l] > 0

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l = [(x, y) | (x, y) <- bd, x /= p || y /= l]

-------------------------------------------

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = [y | y<-xs,y<=x] ++ [x] ++ [y | y<-xs,y>x]

--------------------------------------------
wordBase :: String
wordBase = "ACB     DE     FG"

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | x == ' ' = dropSpace xs
    | otherwise = (x:xs)

getWordSolve :: String -> String
getWordSolve [] = []
getWordSolve (x:xs)
    | x == ' ' = []
    | otherwise = x : getWordSolve xs

getWord :: String -> String
getWord [] = []
getWord l = getWordSolve list
    where list = dropSpace l

dropWordSolve :: String -> String
dropWordSolve [] = []
dropWordSolve (x:xs)
    | x == ' ' = (x:xs)
    | otherwise = dropWordSolve xs

dropWord :: String -> String
dropWord [] = []
dropWord l = dropWordSolve list
    where list = dropSpace l

splitWords :: String -> [String]
splitWords [] = []
splitWords l
    | getWord l == [] = []
    | otherwise = getWord l : splitWords (dropWord l)