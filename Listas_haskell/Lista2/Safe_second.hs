import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond (x:[]) = Nothing
safeSecond (x:xs) = Just (head xs)

main = do
       a <- getLine
       let result = safeSecond (read a::[Int])
       print result
