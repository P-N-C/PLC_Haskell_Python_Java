import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

getNum :: String -> String -> (Int,String)
getNum [] num = (read num::Int, [])
getNum (x:xs) num
  | x >= '0' && x <= '9' = getNum xs (num++[x])
  | length num == 0 = (0, (x:xs))
  | otherwise = (read num::Int, (x:xs))

getOp :: String -> String -> (String,String)
getOp [] op = (op,[])
getOp (x:xs) op
  | not (x >= '0' && x <= '9') = getOp xs (op++[x])
  | otherwise = (op, (x:xs))

safeCalc :: String -> IO ()
safeCalc a = print (solve a)

solve :: String -> Maybe Int
solve str
  | op == "div" && n2 == 0 = Nothing
  | op == "sum" = Just (n1+n2)
  | op == "sub" = Just (n1-n2)poi
  | op == "mul" = Just (n1*n2)
  | otherwise =   Just (n1`div`n2)
    where (n1,str2) = getNum str []
          (op,str3) = getOp str2 []
          (n2,_) = getNum str3 []

main = do
       a <- getLine
       safeCalc a